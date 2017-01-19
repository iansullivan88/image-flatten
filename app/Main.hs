module Main where

import ImageFlatten
import Data.Maybe
import Data.Char
import Options.Applicative
import System.IO
import System.Environment
import System.FilePath

data Options = Options
    { input   :: String,
      output  :: String,
      combine :: Bool,
      remove  :: Bool,
      avg     :: Bool,
      thresh  :: Maybe Float,
      quality :: Maybe Int } deriving (Show)

main :: IO ()
main = do
    opt <- execParser $ info (helper <*> optParser) description
    let validated = validateOptions opt
    case validated of
        Left e               -> hPutStrLn stderr e
        Right (op, inp, out) -> flatten op inp out

description :: InfoMod Options
description = fullDesc
     <> progDesc "Flatten DIRECTORY into OUTPUT"
     <> header   "image-flatten - flatten multiple images into a single image, combining or hiding differences"

optParser :: Parser Options
optParser = Options
        <$> strOption (long "input"   <> short 'i' <> metavar "DIRECTORY"  <> help "A directory containing images to flatten")
        <*> strOption (long "output"  <> short 'o' <> metavar "OUTPUT" <> help "File path to save result image to")
        <*> switch    (long "combine" <> short 'c' <> help "Specifies that differences in images should be combined")
        <*> switch    (long "remove"  <> short 'r' <> help "Specifies that differences in images should be removed")
        <*> switch    (long "average" <> short 'a' <> help "Specifies that images should be averaged")
        <*> optional  (option auto (long "threshold" <> short 't' <> help "Adjust the sensitivity for detecting features. A low number is required to detect subtle differences eg a green jumper on grass. A high number will suffice for white stars against a black sky. Default is 10."))
        <*> optional  (option auto (long "quality"   <> short 'q' <> help "If output is a JPEG, this specifies the quality to use when saving"))

validateOptions :: Options -> Either String (Operation, InputSource, OutputDestination)
validateOptions (Options i o c r a t q)
    | q' < 0 || q' > 100             = Left "Jpeg quality must be between 0 and 100 inclusive"
    | not (isPng || isJpg)           = Left "Output file must be a .png or .jpg"
    | length (filter id [c,r,a]) > 1 = Left "Only one of remove (-r), combine (-c) and average (-a) can be specified"
    | otherwise                      = Right (
                                        case (c,r,a) of
                                            (True,_,_) -> CombineDifferences t'
                                            (_,_,True) -> Average
                                            _          -> HideDifferences,
                                        Directory i,
                                        output) where
        isPng     = extension == ".png"
        isJpg     = extension == ".jpg" || extension == ".jpeg"
        output    = if isJpg then JpgFile o q' else PngFile o
        q'        = fromMaybe 100 q
        extension = map toLower $ takeExtension o
        t'        = fromMaybe 10 t
