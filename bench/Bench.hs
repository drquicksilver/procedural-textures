-- | Rendering benchmarks: every example at 256², plus the server's full
-- render-and-encode path for representative examples. Times are
-- wall-clock, on all cores.
--
-- Run with `stack bench --ba '-j 1'`. To compare against a saved baseline:
--   stack bench --ba '-j 1 --csv /tmp/texture-bench.csv'  (save representative run)
--   stack bench --ba '-j 1 --baseline bench/baseline.csv' (compare)
--   stack bench --ba '--full-library -j 1 --stdev 15 --csv bench/baseline.csv' (full baseline)
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
import System.Environment (getArgs, withArgs)

main :: IO ()
main = do
  library <- loadRampLibrary defaultRampsDirectory
  examples <- loadExamples defaultExamplesDirectory
  args <- getArgs
  let textureOf example = either error id (resolveDocument library (exampleDocument example))
      fullLibrary = "--full-library" `elem` args
      -- Keep the simple control and historical case, plus the four slowest
      -- 512² PNG paths in the 2026-10-05 full-library sweep (see RESULTS.md).
      representatives = ["checker", "marble", "cumulus", "moss", "rust", "ice"]
      encoded = [e | e <- examples, fullLibrary || exampleId e `elem` representatives]
  -- Rendering is parallel, so CPU time (tasty-bench's default) would hide
  -- the speed-up: measure wall-clock time instead.
  withArgs (filter (/= "--full-library") args) . defaultMain . map (localOption WallTime) $
    [ bgroup "render 256" [bench (exampleId e) (whnf (render 256) (textureOf e)) | e <- examples]
    , bgroup "render+png"
        [ bench (exampleId e <> " " <> show size) (whnf (BL.length . renderPng size) (textureOf e))
        | e <- encoded, size <- [96, 256, 512]
        ]
    ]

-- | Rendering into a storable vector forces every pixel.
render :: Int -> Texture -> Int
render size texture =
  VS.length (imageData (renderImage size size (textureToImageFn texture)))
