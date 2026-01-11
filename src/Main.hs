module Main (main) where

import ColourRamps (RampMode (Clamp, Mirror, Wrap), colourRamp, sinusoidalColourRamp, twoStopRamp)
import Colours (blue, green, lightGrey, pink, red, skyBlue, slateGrey, transparent, white)
import Render (ImageFn, writeImage, writeImageRaw)
import Texture (Texture (..), textureToImageFn)
import Data.List (intercalate)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if "--benchmark" `elem` args
    then runBenchmark
    else renderDefault

renderDefault :: IO ()
renderDefault = do
  let width = 128
      height = 128
  mapM_ (writeImage width height . toImageFn) textures

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
  mapM (timeOne width height) textures

timeOne :: Int -> Int -> (FilePath, Texture) -> IO (FilePath, Integer)
timeOne width height (path, texture) = do
  start <- getCurrentTime
  writeImageRaw width height (path, textureToImageFn texture)
  end <- getCurrentTime
  let ms = round (diffUTCTime end start * 1000.0)
  pure (path, ms)

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

toImageFn :: (FilePath, Texture) -> (FilePath, ImageFn)
toImageFn (path, texture) = (path, textureToImageFn texture)

textures :: [(FilePath, Texture)]
textures =
  [ ("gradient.png", gradient)
  , ("rings.png", rings)
  , ("radial.png", radial)
  , ("layered.png", layered)
  , ("stripes.png", stripes)
  , ("clouds.png", clouds)
  , ("wobbly-stripes.png", wobblyStripes)
  , ("swirly-stripes.png", swirlyStripes)
  , ("marble.png", marble)
  , ("checker.png", checker)
  , ("redgreensaw.png", redGreenSaw)
  , ("redgreensine.png", redGreenSine)
  ]

-- some basic examples using ImageFn directly
gradient :: Texture
gradient =
  Linear (0.0, 0.5) (1.0, 0.5) (twoStopRamp Clamp red blue)

rings :: Texture
rings =
  Circular (0.5, 0.5) 0.08 (twoStopRamp Mirror white blue)

radial :: Texture
radial =
  Radial (0.5, 0.5) (twoStopRamp Clamp red green)

layered :: Texture
layered =
  Layer
    (Linear (0.0, 0.5) (1.0, 0.5) (twoStopRamp Clamp transparent blue))
    (Linear (0.5, 0.0) (0.5, 1.0) (twoStopRamp Clamp red green))

stripes :: Texture
stripes =
  let ramp = colourRamp Wrap [(0.0, red), (0.5, pink), (0.5, white), (1.0, lightGrey)]
  in Linear (0.0, 0.5) (1.0 / 6.0, 0.5) ramp

clouds :: Texture
clouds =
  let ramp = colourRamp Clamp [(0.0, skyBlue), (0.6, white), (1.0, lightGrey)]
  in Perlin (4.0, 4.0) ramp

baseStripes :: Texture
baseStripes =
  let ramp =
        colourRamp Wrap
          [ (0.0, (0.15, 0.2, 0.6, 1.0))
          , (0.5, (0.15, 0.2, 0.6, 1.0))
          , (0.5, (0.85, 0.9, 1.0, 1.0))
          , (1.0, (0.85, 0.9, 1.0, 1.0))
          ]
  in Linear (0.0, 0.5) (1.0 / 4.0, 0.5) ramp

wobblyStripes :: Texture
wobblyStripes =
  Turbulence 0.08 3 0.5 2.0 baseStripes

swirlyStripes :: Texture
swirlyStripes =
  Turbulence 0.80 8 0.4 2.2 baseStripes

marble :: Texture
marble =
  let bandRamp =
        colourRamp Wrap
          [ (0.0, white)
          , (0.65, white)
          , (0.75, slateGrey)
          , (1.0, slateGrey)
          ]
      bands = Linear (0.0, 0.5) (1.0 / 4.0, 0.5) bandRamp
  in Turbulence 0.6 9 0.65 2.5 bands

checker :: Texture
checker =
  Tiled 8 8 (Flat (0.9, 0.9, 0.9, 1.0)) (Flat (0.1, 0.1, 0.1, 1.0))

redGreenSaw :: Texture
redGreenSaw =
  Linear (0.0, 0.5) (1.0 / 6.0, 0.5) (twoStopRamp Mirror red green)

redGreenSine :: Texture
redGreenSine =
  Linear (0.0, 0.5) (1.0 / 6.0, 0.5) (sinusoidalColourRamp red green)
