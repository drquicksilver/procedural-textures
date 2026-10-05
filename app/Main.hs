module Main (main) where

import Examples (examples)
import HtmlOutput (writeGallery)
import Render (ImageFn, writeImage, writeImageRaw)
import Texture (Texture, textureToImageFn)
import Data.List (intercalate)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))

main :: IO ()
main = do
  args <- getArgs
  if "--benchmark" `elem` args
    then runBenchmark
    else if "--html" `elem` args
      then renderHtml
      else renderDefault

renderDefault :: IO ()
renderDefault = do
  let outputDir = "out"
      width = 128
      height = 128
  createDirectoryIfMissing True outputDir
  mapM_ (writeImage width height . toImageFn . inDir outputDir) examples

runBenchmark :: IO ()
runBenchmark = do
  small <- timeAll 128 128
  putStrLn "Benchmark: 128x128"
  putStrLn (renderTable small)
  large <- timeAll 512 512
  putStrLn "Benchmark: 512x512"
  putStrLn (renderTable large)

timeAll :: Int -> Int -> IO [(FilePath, Integer)]
timeAll width height =
  mapM (timeOne width height) examples

timeOne :: Int -> Int -> (FilePath, Texture) -> IO (FilePath, Integer)
timeOne width height example = do
  start <- getCurrentTime
  writeImageRaw width height (toImageFn example)
  end <- getCurrentTime
  let ms = round (diffUTCTime end start * 1000.0)
  pure (fst example, ms)

renderHtml :: IO ()
renderHtml = do
  let outputDir = "site"
      width = 512
      height = 512
      outputExamples = map (inDir outputDir) examples
  createDirectoryIfMissing True outputDir
  mapM_ (writeImageRaw width height . toImageFn) outputExamples
  writeGallery (outputDir </> "gallery.html") "Procedural Textures" (map (\(path, texture) -> (path, show texture)) examples)

renderTable :: [(FilePath, Integer)] -> String
renderTable rows =
  let nameWidth = maximum (length "File" : map (length . fst) rows)
      msWidth = maximum (length "Ms" : map (length . show . snd) rows)
      header = formatRow nameWidth msWidth "File" "Ms"
      separator = replicate (nameWidth + msWidth + 5) '-'
      body = map (\(name, ms) -> formatRow nameWidth msWidth name (show ms)) rows
  in intercalate "\n" (header : separator : body)

formatRow :: Int -> Int -> String -> String -> String
formatRow nameWidth msWidth name ms =
  padRight nameWidth name <> " | " <> padLeft msWidth ms

padRight :: Int -> String -> String
padRight width value =
  value <> replicate (width - length value) ' '

padLeft :: Int -> String -> String
padLeft width value =
  replicate (width - length value) ' ' <> value

inDir :: FilePath -> (FilePath, a) -> (FilePath, a)
inDir dir (path, x) = (dir </> path, x)

toImageFn :: (FilePath, Texture) -> (FilePath, ImageFn)
toImageFn (path, texture) = (path, textureToImageFn texture)
