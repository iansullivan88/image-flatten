{-# LANGUAGE BangPatterns #-}

module ImageFlatten
    (
        InputSource(Directory),
        OutputDestination(JpgFile, PngFile),
        Operation(HideDifferences,CombineDifferences),
        flatten
    ) where

import Data.Either
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Codec.Picture hiding(readImage)
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import System.Directory
import Control.Exception
import System.IO.Error
import System.FilePath((</>))

data InputSource = Directory FilePath
data OutputDestination = JpgFile FilePath Int | PngFile FilePath
data Operation = HideDifferences | CombineDifferences Float

type ErrorIO a = EitherT String IO a
type PixelFunction = Pixel8 -> Int
type AddPixel p = Int -> PixelRGB8 -> p -> p
data ImageWithPath = ImageWithPath FilePath (Image PixelRGB8)

flatten :: Operation -> InputSource -> OutputDestination -> IO (Either String ())
flatten op i o = runEitherT $ do
    (nImages, (w, h), inputStream) <- createValidatedImageStream i
    let nPixels = w*h
        nImagesFloat = fromIntegral nImages
    totals <- foldImageData calculateTotals (V.replicate nPixels (0,0,0,0,0,0)) inputStream
    let averageTotals   = V.map (mapTup6 (/ nImagesFloat)) totals
        variances       = V.map (\(r,g,b,rst,gst,bst) -> (rst + gst + bst - r*r - g*g - b*b)) averageTotals
        means           = V.map (\(r,g,b,_,_,_) -> (r,g,b)) averageTotals
    (_,_,inputStream2)  <- createValidatedImageStream i
    resultImageData     <- performOperation op inputStream2 means variances nPixels
    let resultImage     = generateImageFromVector w h resultImageData
    liftIO $ outputImage o (ImageRGB8 resultImage)

calculateTotals :: Int -> PixelRGB8 -> (Float, Float, Float, Float, Float, Float) -> (Float, Float, Float, Float, Float, Float)
calculateTotals _ px (rt, gt, bt, rst, gst, bst) = (r + rt, g + gt, b + bt, rst + r*r, gst + g*g, bst + b*b)
    where (r, g, b) = getFloatPixel px

performOperation :: Operation -> InputStream (Either String (Image PixelRGB8)) -> V.Vector (Float, Float, Float) -> V.Vector Float -> Int -> ErrorIO (V.Vector (Float, Float, Float))
performOperation HideDifferences i means variances nPixels = do
    totalImageData <- foldImageData (stripOutlierPixels means variances) (V.replicate nPixels (0,0,0,0)) i
    return $ V.map (\(r,g,b,t) -> let d = fromIntegral t in (r / d, g / d, b / d)) totalImageData
performOperation (CombineDifferences t) i means variances nPixels = do
    totalImageData <- foldImageData (keepOutlierPixels t means variances) (V.replicate nPixels (0,0,0,0,0)) i
    return $ V.map (\(r,g,b,t,_) -> let d = fromIntegral t in (r / d, g / d, b / d)) totalImageData


keepOutlierPixels :: Float -> V.Vector (Float, Float, Float) -> V.Vector Float -> Int -> PixelRGB8 -> (Float, Float, Float, Int, Float) -> (Float, Float, Float, Int, Float)
keepOutlierPixels t means variances i px p@(rt, gt, bt, n, maxDeviation)
    | isSignificant && distanceSquared > maxDeviation = (r, g, b, 1, distanceSquared)
    | not outlierIdentified && includeInMean = (rt + r, gt + g, bt + b, n + 1, maxDeviation)
    | otherwise = p
    where
        (r, g, b)         = getFloatPixel px
        (ar, ag, ab)      = means V.! i
        variance          = variances V.! i
        includeInMean     = distanceSquared <= variance -- within 1 standard deviation
        isSignificant     = distanceSquared > t
        distanceSquared   = squareVector (r - ar) (g - ag) (b - ab)
        outlierIdentified = maxDeviation > 0

stripOutlierPixels :: V.Vector (Float, Float, Float) -> V.Vector Float -> Int -> PixelRGB8 -> (Float, Float, Float, Int) -> (Float, Float, Float, Int)
stripOutlierPixels means variances i px p@(rt, gt, bt, t)
    | includeInMean = (rt + r, gt + g, bt + b, t + 1)
    | otherwise = p
    where
        (r, g, b)     = getFloatPixel px
        (ar, ag, ab)  = means V.! i
        variance      = variances V.! i
        includeInMean = squareVector (r - ar) (g - ag) (b - ab) <= variance -- within 1 standard deviation

createValidatedImageStream :: InputSource -> ErrorIO (Int, (Int, Int), InputStream (Either String (Image PixelRGB8)))
createValidatedImageStream i = do
    (nImages, inputStream) <- getInputFileStream i
    imageStream            <- liftIO $ Streams.map readImage inputStream
    (Image w h _)          <- EitherT $ fromJust <$> Streams.peek imageStream -- fromJust is safe as imageStream has 3 or more values
    s                      <- liftIO $ streamSequenceEither (validateImage w h) imageStream
    return (nImages, (w, h), s)

-- |Folds the given AddPixel function over a stream of Images, accumulating the data into a vector
foldImageData :: (V.Unbox p) => AddPixel p -> V.Vector p -> InputStream (Either String (Image PixelRGB8)) -> ErrorIO (V.Vector p)
foldImageData f = foldUntilError accumPixels where
    accumPixels v img@(Image w h imgData) = V.imap (\i p -> f i (pixelAt img (indexToX w i) (indexToY w i)) p) v
    indexToX w i = i `mod` w
    indexToY w i = i `div` w

validateImage :: Int -> Int -> Image PixelRGB8 -> Either String (Image PixelRGB8)
validateImage w h i@(Image w' h' _)
    | w == w' && h == h' = Right i
    | otherwise          = Left "Images must all have the same dimensions"

outputImage :: OutputDestination -> DynamicImage -> IO ()
outputImage (JpgFile path quality) = saveJpgImage quality path
outputImage (PngFile path)         = savePngImage path

getInputFileStream :: InputSource -> ErrorIO (Int, InputStream (FilePath, B.ByteString))
getInputFileStream (Directory dir) = do
    names <- liftIOSafeWithMessage formatlistDirectoryError (listDirectory dir)
    let nImages = length names
    when (nImages < 3) (left $ dir ++ " must contain 3 or more files")
    let paths = fmap (dir </>) names
    stream <- liftIO $ Streams.fromList paths >>= Streams.mapM (\p -> B.readFile p >>= (\b -> return (p, b)))
    return (nImages, stream)
        where
        formatlistDirectoryError e = if isDoesNotExistError e
            then dir ++ " does not exist"
            else displayException e

generateImageFromVector :: Int -> Int -> V.Vector (Float, Float, Float) -> Image PixelRGB8
generateImageFromVector w h v = generateImage getPixel w h where
    getPixel x y = let (r,g,b) = v V.! (x + y*w) in PixelRGB8 (round r) (round g) (round b)

readImage :: (FilePath, B.ByteString) -> Either String (Image PixelRGB8)
readImage (path, bs) = either (Left . formatError) (Right . convertRGB8) (decodeImage bs) where
    formatError e = path ++ ": " ++ e

addTuples :: Num a => (a, a, a) -> (a, a, a) -> (a, a, a)
addTuples (a, b, c) (a', b', c') = (a + a', b + b', c + c')

-- |Fold an input stream of 'Eithers' producing a result in the ErrorIO monad
foldUntilError :: (s -> a -> s) -> s -> InputStream (Either String a) -> ErrorIO s
foldUntilError f seed stream = EitherT $ go seed
    where
        go !s = Streams.read stream >>= \v -> case v of
            Just (Right v') -> go $ f s v'
            Just (Left  v') -> return $ Left v'
            Nothing         -> return $ Right s

-- |Map a function producing Eithers over an input stream of eithers
streamSequenceEither :: (a -> Either e b) -> InputStream (Either e a) -> IO (InputStream (Either e b))
streamSequenceEither f = Streams.map f' where
    f' (Right v) = f v
    f' (Left e)  = Left e

getFloatPixel :: PixelRGB8 -> (Float, Float, Float)
getFloatPixel (PixelRGB8 r g b) = (fromIntegral r, fromIntegral g, fromIntegral b)

mapTup3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTup3 f (a, b, c) = (f a, f b, f c)

squareVector :: (Num a) => a -> a -> a -> a
squareVector a b c = a*a + b*b + c*c

mapTup6 :: (a -> b) -> (a, a, a, a, a, a) -> (b, b, b, b, b, b)
mapTup6 g (a, b, c, d, e, f) = (g a, g b, g c, g d, g e, g f)

liftIOSafeWithMessage :: (IOException -> String) -> IO a -> ErrorIO a
liftIOSafeWithMessage getErrorMessage a = EitherT $ catch (Right <$> a) (return . Left . getErrorMessage)
