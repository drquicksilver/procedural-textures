module Main (main) where

import Codec.Picture (readPng)
import PNGCompareCore (PngImage, comparePngImages, pngImageFromDynamic)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [leftPath, rightPath] -> runCompare leftPath rightPath
    _ -> do
      hPutStrLn stderr "Usage: PNGCompare <left.png> <right.png>"
      exitFailure

runCompare :: FilePath -> FilePath -> IO ()
runCompare leftPath rightPath = do
  result <- compareFiles leftPath rightPath
  case result of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right avg ->
      printf "%.4f\n" avg

compareFiles :: FilePath -> FilePath -> IO (Either String Double)
compareFiles leftPath rightPath = do
  leftImage <- loadPngImage leftPath
  rightImage <- loadPngImage rightPath
  pure $
    case (leftImage, rightImage) of
      (Right left, Right right) -> comparePngImages left right
      (Left err, _) -> Left err
      (_, Left err) -> Left err

loadPngImage :: FilePath -> IO (Either String PngImage)
loadPngImage path = do
  result <- readPng path
  case result of
    Left err ->
      pure (Left ("Failed to read PNG " <> path <> ": " <> err))
    Right dyn ->
      case pngImageFromDynamic dyn of
        Left err ->
          pure (Left ("Unsupported PNG " <> path <> ": " <> err))
        Right img ->
          pure (Right img)
