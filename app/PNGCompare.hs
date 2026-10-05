module Main (main) where

import Codec.Picture (readPng)
import PNGCompareCore
  ( CompareResult (..)
  , PngImage
  , Tolerance (..)
  , comparePngImages
  , pngImageFromDynamic
  , withinTolerance
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | Exit codes: 0 when the images match (or no threshold was given), 1 when a
-- threshold is exceeded, 2 for usage errors or unreadable images.
main :: IO ()
main = do
  args <- getArgs
  case parseArgs args noLimits of
    Right (limits, [leftPath, rightPath]) -> runCompare limits leftPath rightPath
    Right _ -> usage
    Left err -> do
      hPutStrLn stderr err
      usage

noLimits :: Tolerance
noLimits =
  Tolerance {toleranceMean = 1 / 0, toleranceMax = 1 / 0}

parseArgs :: [String] -> Tolerance -> Either String (Tolerance, [FilePath])
parseArgs args limits =
  case args of
    "--threshold" : value : rest -> do
      mean <- parseDouble "--threshold" value
      parseArgs rest limits {toleranceMean = mean}
    "--max-threshold" : value : rest -> do
      largest <- parseDouble "--max-threshold" value
      parseArgs rest limits {toleranceMax = largest}
    _ -> Right (limits, args)

parseDouble :: String -> String -> Either String Double
parseDouble flag value =
  maybe (Left ("Invalid number for " <> flag <> ": " <> value)) Right (readMaybe value)

usage :: IO ()
usage = do
  hPutStrLn stderr "Usage: png-compare [--threshold MEAN] [--max-threshold MAX] <left.png> <right.png>"
  hPutStrLn stderr "Prints the mean and maximum per-pixel RGBA distance (channels in [0, 1])."
  hPutStrLn stderr "Exits 1 if either threshold is exceeded, 2 on errors."
  exitWith (ExitFailure 2)

runCompare :: Tolerance -> FilePath -> FilePath -> IO ()
runCompare limits leftPath rightPath = do
  result <- compareFiles leftPath rightPath
  case result of
    Left err -> do
      hPutStrLn stderr err
      exitWith (ExitFailure 2)
    Right comparison -> do
      printf "mean %.6f max %.6f\n" (meanError comparison) (maxError comparison)
      if withinTolerance limits comparison
        then pure ()
        else exitWith (ExitFailure 1)

compareFiles :: FilePath -> FilePath -> IO (Either String CompareResult)
compareFiles leftPath rightPath = do
  leftImage <- loadPngImage leftPath
  rightImage <- loadPngImage rightPath
  pure $ do
    left <- leftImage
    right <- rightImage
    comparePngImages left right

loadPngImage :: FilePath -> IO (Either String PngImage)
loadPngImage path = do
  result <- readPng path
  pure $
    case result of
      Left err -> Left ("Failed to read PNG " <> path <> ": " <> err)
      Right dyn ->
        either (\err -> Left ("Unsupported PNG " <> path <> ": " <> err)) Right (pngImageFromDynamic dyn)
