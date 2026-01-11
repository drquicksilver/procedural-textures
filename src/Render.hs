module Render
  ( ImageFn
  , writeImage
  , writeImageRaw
  ) where

import Codec.Picture (PixelRGBA8 (..), generateImage, writePng)
import Colours (Colour)
import Data.Word (Word8)

type ImageFn = Double -> Double -> Colour

writeImage :: Int -> Int -> (FilePath, ImageFn) -> IO ()
writeImage width height (path, f) = do
  writeImageRaw width height (path, f)
  putStrLn ("Wrote " <> path)

writeImageRaw :: Int -> Int -> (FilePath, ImageFn) -> IO ()
writeImageRaw width height (path, f) = do
  let image = generateImage (renderAt width height f) width height
  writePng path image

renderAt :: Int -> Int -> ImageFn -> Int -> Int -> PixelRGBA8
renderAt width height f x y =
  let u = indexToUnit x width
      v = indexToUnit y height
  in toPixel (f u v)

indexToUnit :: Int -> Int -> Double
indexToUnit i size =
  let denom = max 1 size
  in (fromIntegral i + 0.5) / fromIntegral denom

toByte :: Double -> Word8
toByte value =
  let clamped = max 0.0 (min 1.0 value)
  in fromIntegral (round (clamped * 255.0))

toPixel :: Colour -> PixelRGBA8
toPixel (r, g, b, a) =
  PixelRGBA8 (toByte r) (toByte g) (toByte b) (toByte a)
