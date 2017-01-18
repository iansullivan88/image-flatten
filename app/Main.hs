module Main where

import ImageFlatten
import Data.Maybe
import Data.Char
import Control.Monad.Trans.Either
import Options.Applicative
import System.IO
import System.Environment
import System.FilePath
import Control.Monad.IO.Class

data Options = Options
    { input    :: String,
      output   :: String,
      combine  :: Bool,
      hide     :: Bool,
      thresh   :: Maybe Float,
      quality :: Maybe Int } deriving (Show)

main :: IO ()
main = eitherT (hPutStrLn stderr) (const $ return ()) go
 where
    go = do
        opt <- liftIO $ execParser $ info (helper <*> optParser) description
        (op, inp, out) <- hoistEither $ validateOptions opt
        EitherT $ flatten op inp out

description :: InfoMod Options
description = fullDesc
     <> progDesc "Flatten DIRECTORY into OUTPUT"
     <> header   "image-flatten - flatten multiple images into a single image, combining or hiding differences"

optParser :: Parser Options
optParser = Options
        <$> strOption (long "input"   <> short 'i' <> metavar "DIRECTORY"  <> help "A directory containing images to flatten")
        <*> strOption (long "output"  <> short 'o' <> metavar "OUTPUT" <> help "File path to save result image to")
        <*> switch    (long "combine" <> short 'c' <> help "Specifies that differences in photos should be combined")
        <*> switch    (long "remove"    <> short 'r' <> help "Specifies that differences in photos should be removed")
        <*> optional  (option auto (long "threshold" <> short 't' <> help "Adjust the sensitivity for detecting features. A low number is required to detect subtle differences eg a green jumper on grass. A high number will suffice for white stars against a black sky. Default is 10."))
        <*> optional  (option auto (long "quality"   <> short 'q' <> help "If output is a JPEG, this specifies the quality to use when saving"))

validateOptions :: Options -> Either String (Operation, InputSource, OutputDestination)
validateOptions (Options i o c h t q)
    | q' < 0 || q' > 100       = Left "Jpeg quality must be between 0 and 100 inclusive"
    | not (isPng || isJpg)     = Left "Output file must be a .png or .jpg"
    | c && h                   = Left "Hide (-h) and combine (-c) options cannot both be specified"
    | otherwise                = Right (if c then CombineDifferences t' else HideDifferences,
                                        Directory i,
                                        output) where
        isPng     = extension == ".png"
        isJpg     = extension == ".jpg" || extension == ".jpeg"
        output    = if isJpg then JpgFile o q' else PngFile o
        q'        = fromMaybe 100 q
        extension = map toLower $ takeExtension o
        t'        = fromMaybe 10 t
