-- | Rendering benchmarks: every example at 256², plus the server's full
-- render-and-encode path for the most expensive example. Times are
-- wall-clock, on all cores.
--
-- Run with `stack bench`. To compare against a saved baseline:
--   stack bench --ba '--csv bench/baseline.csv'          (save)
--   stack bench --ba '--baseline bench/baseline.csv'     (compare)
module Main (main) where

import Codec.Picture (Image (imageData))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Storable as VS
import Examples (Example (..), defaultExamplesDirectory, loadExamples)
import Render (renderImage)
import Server (renderPng)
import Test.Tasty (localOption)
import Test.Tasty.Bench (TimeMode (WallTime), bench, bgroup, defaultMain, whnf)
import Texture (Texture, textureToImageFn)
import RampLibrary (defaultRampsDirectory, loadRampLibrary)
import Resolve (resolveDocument)

main :: IO ()
main = do
  library <- loadRampLibrary defaultRampsDirectory
  examples <- loadExamples defaultExamplesDirectory
  let textureOf example = either error id (resolveDocument library (exampleDocument example))
      marble = [textureOf e | e <- examples, exampleId e == "marble"]
  -- Rendering is parallel, so CPU time (tasty-bench's default) would hide
  -- the speed-up: measure wall-clock time instead.
  defaultMain . map (localOption WallTime) $
    [ bgroup "render 256" [bench (exampleId e) (whnf (render 256) (textureOf e)) | e <- examples]
    , bgroup "render+png" [bench ("marble " <> show size) (whnf (BL.length . renderPng size) t) | t <- marble, size <- [96, 256, 512]]
    ]

-- | Rendering into a storable vector forces every pixel.
render :: Int -> Texture -> Int
render size texture =
  VS.length (imageData (renderImage size size (textureToImageFn texture)))
