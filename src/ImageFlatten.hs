{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ImageFlatten
    (
        InputSource(Directory),
        OutputDestination(JpgFile, PngFile),
        Operation(HideDifferences,CombineDifferences),
        flatten
    ) where

import Codec.Picture
import Data.Either
import Data.Maybe
import Data.Typeable
import Data.Foldable
import qualified Data.Vector.Unboxed as V
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import System.Directory
import System.FilePath((</>))

data InputSource = Directory FilePath
data OutputDestination = JpgFile FilePath Int | PngFile FilePath
data Operation = HideDifferences | CombineDifferences Float
data FlattenException =
    NotEnoughInputFilesException FilePath |
    ImageLoadException FilePath |
    CouldNotReadException FilePath |
    CouldNotWriteException FilePath |
    ImagesNotSameSizeException deriving (Typeable)

instance Show FlattenException where
    show (NotEnoughInputFilesException p) = p ++ " must contain 3 or more files"
    show (ImageLoadException p)           = "could not load image " ++ p
    show (CouldNotReadException p)        = "could not read " ++ p
    show (CouldNotWriteException p)       = "could not write" ++ p
    show ImagesNotSameSizeException       = "all images must have the same dimensions"

instance Exception FlattenException

type PixelFunction = Pixel8 -> Int
type AddPixel p = Int -> PixelRGB8 -> p -> p
type ImageStream = [IO (Image PixelRGB8)]
data ImageWithPath = ImageWithPath FilePath (Image PixelRGB8)

flatten :: Operation -> InputSource -> OutputDestination -> IO ()
flatten op i o = do
    inputStream   <- getInputFileStream i
    (Image w h _) <- head inputStream
    let nPixels = w*h
        nImages = length inputStream
        nImagesFloat = fromIntegral nImages
    totals <- foldImageData w h calculateTotals (V.replicate nPixels (0,0,0,0,0,0)) inputStream
    let averageTotals   = V.map (mapTup6 (/ nImagesFloat)) totals
        variances       = V.map (\(r,g,b,rst,gst,bst) -> (rst + gst + bst - r*r - g*g - b*b)) averageTotals
        means           = V.map (\(r,g,b,_,_,_) -> (r,g,b)) averageTotals
    resultImageData     <- performOperation w h op inputStream means variances nPixels
    let resultImage     = generateImageFromVector w h resultImageData
    outputImage o (ImageRGB8 resultImage)

calculateTotals :: Int -> PixelRGB8 -> (Float, Float, Float, Float, Float, Float) -> (Float, Float, Float, Float, Float, Float)
calculateTotals _ px (rt, gt, bt, rst, gst, bst) = (r + rt, g + gt, b + bt, rst + r*r, gst + g*g, bst + b*b)
    where (r, g, b) = getFloatPixel px

performOperation :: Int -> Int -> Operation -> ImageStream -> V.Vector (Float, Float, Float) -> V.Vector Float -> Int -> IO (V.Vector (Float, Float, Float))
performOperation w h HideDifferences i means variances nPixels = do
    totalImageData <- foldImageData w h (stripOutlierPixels means variances) (V.replicate nPixels (0,0,0,0)) i
    return $ V.map (\(r,g,b,t) -> let d = fromIntegral t in (r / d, g / d, b / d)) totalImageData
performOperation w h (CombineDifferences t) i means variances nPixels = do
    totalImageData <- foldImageData w h (keepOutlierPixels t means variances) (V.replicate nPixels (0,0,0,0,0)) i
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

-- |Folds the given AddPixel function over a stream of Images, accumulating the data into a vector
foldImageData :: (V.Unbox p) => Int -> Int -> AddPixel p -> V.Vector p -> ImageStream -> IO (V.Vector p)
foldImageData w h f = foldlM accumPixels where
    accumPixels v loadImage = do
        img@(Image w' h' imgData) <- loadImage
        when (w' /= w || h' /= h) (throwIO ImagesNotSameSizeException)
        return $ V.imap (\i p -> f i (pixelAt img (indexToX w i) (indexToY w i)) p) v
    indexToX w i = i `mod` w
    indexToY w i = i `div` w

outputImage :: OutputDestination -> DynamicImage -> IO ()
outputImage d i = case d of
    (JpgFile path quality) -> save (saveJpgImage quality path i) path
    (PngFile path)         -> save (savePngImage path i) path
    where save a p = a `catch` \(ex :: IOException) -> throwIO $ CouldNotWriteException p

getInputFileStream :: InputSource -> IO ImageStream
getInputFileStream (Directory dir) = do
    names <- listDirectory dir `catch` \(ex :: IOException) -> throwIO $ CouldNotReadException dir
    let nImages = length names
    when (nImages < 3) (throwIO $ NotEnoughInputFilesException dir)
    let paths = fmap (dir </>) names
    return $ readImageE <$> paths where
        readImageE p = readImage p >>= either (\_ -> throwIO $ ImageLoadException p) (return . convertRGB8)

generateImageFromVector :: Int -> Int -> V.Vector (Float, Float, Float) -> Image PixelRGB8
generateImageFromVector w h v = generateImage getPixel w h where
    getPixel x y = let (r,g,b) = v V.! (x + y*w) in PixelRGB8 (round r) (round g) (round b)

addTuples :: Num a => (a, a, a) -> (a, a, a) -> (a, a, a)
addTuples (a, b, c) (a', b', c') = (a + a', b + b', c + c')

getFloatPixel :: PixelRGB8 -> (Float, Float, Float)
getFloatPixel (PixelRGB8 r g b) = (fromIntegral r, fromIntegral g, fromIntegral b)

squareVector :: (Num a) => a -> a -> a -> a
squareVector a b c = a*a + b*b + c*c

mapTup6 :: (a -> b) -> (a, a, a, a, a, a) -> (b, b, b, b, b, b)
mapTup6 g (a, b, c, d, e, f) = (g a, g b, g c, g d, g e, g f)
