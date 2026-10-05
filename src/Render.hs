module Render
  ( ImageFn
  , renderImage
  , writeImage
  , writeImageRaw
  ) where

import Codec.Picture (Image (..), PixelRGBA8 (..), writePng)
import Colours (Colour)
import Control.Monad (forM_)
import Control.Parallel.Strategies (parListChunk, rseq, withStrategy)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word (Word8)

type ImageFn = Double -> Double -> Colour

writeImage :: Int -> Int -> (FilePath, ImageFn) -> IO ()
writeImage width height (path, f) = do
  writeImageRaw width height (path, f)
  putStrLn ("Wrote " <> path)

writeImageRaw :: Int -> Int -> (FilePath, ImageFn) -> IO ()
writeImageRaw width height (path, f) =
  writePng path (renderImage width height f)

-- | Render an image, evaluating rows in parallel when the runtime has more
-- than one capability (programs built with @-threaded@ and run with @-N@).
renderImage :: Int -> Int -> ImageFn -> Image PixelRGBA8
renderImage width height f =
  Image width height (VS.concat (withStrategy (parListChunk rowsPerSpark rseq) rows))
  where
    rows = map (renderRow width height f) [0 .. height - 1]
    rowsPerSpark = 4

-- | One row of RGBA bytes. Storable vectors are strict, so evaluating a row
-- to weak head normal form renders all of it.
renderRow :: Int -> Int -> ImageFn -> Int -> VS.Vector Word8
renderRow width height f y =
  VS.create $ do
    row <- VSM.new (width * 4)
    forM_ [0 .. width - 1] $ \x -> do
      let PixelRGBA8 r g b a = renderAt width height f x y
          i = x * 4
      VSM.unsafeWrite row i r
      VSM.unsafeWrite row (i + 1) g
      VSM.unsafeWrite row (i + 2) b
      VSM.unsafeWrite row (i + 3) a
    pure row

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
  in fromIntegral (round (clamped * 255.0) :: Int)

toPixel :: Colour -> PixelRGBA8
toPixel (r, g, b, a) =
  PixelRGBA8 (toByte r) (toByte g) (toByte b) (toByte a)
