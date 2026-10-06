-- Developer probe; see README.md in this directory for build and run commands.
module Main (main) where

import Codec.Picture (Image (imageData), encodePng)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Storable as VS
import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM_)
import Data.IORef (newIORef, readIORef)
import Examples (Example (..), loadExamples, defaultExamplesDirectory)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import RampLibrary (loadRampLibrary, defaultRampsDirectory)
import Render (renderImage)
import Resolve (resolveDocument)
import System.Environment (getArgs)
import System.Mem (performGC)
import Texture (Texture, textureToImageFn)

expensive :: [String]
expensive = ["cumulus", "moss", "rust", "ice", "water-ripples", "tiger-fur", "jupiter", "marble", "wood-knot", "mountains"]

main :: IO ()
main = do
  args <- getArgs
  ramps <- loadRampLibrary defaultRampsDirectory
  examples <- loadExamples defaultExamplesDirectory
  let textures = [(exampleId e, either error id (resolveDocument ramps (exampleDocument e))) | e <- examples]
  case args of
    ["export-core", path] -> writeFile path (coreSource [(n,t) | (n,t) <- textures, n `elem` expensive])
    [name, stage, sizeText, iterationsText, batchesText] ->
      case lookup name textures of
        Nothing -> fail ("Unknown example: " <> name)
        Just texture -> measure name stage (read sizeText) (read iterationsText) (read batchesText) texture
    _ -> fail "Usage: texture-profile export-core PATH | NAME render|encode|combined SIZE ITERATIONS BATCHES (+RTS -T -N...)"

measure :: String -> String -> Int -> Int -> Int -> Texture -> IO ()
measure name stage size iterations batches texture = do
  if size <= 0 || iterations <= 0 || batches <= 0 then fail "All counts must be positive" else pure ()
  enabled <- getRTSStatsEnabled
  if not enabled then fail "Run with +RTS -T" else pure ()
  -- Read the argument in IO on every iteration so the optimiser cannot share
  -- the pure render/encoding result across iterations. Keep preparation outside.
  fnRef <- newIORef (textureToImageFn texture)
  image <- case stage of
    "encode" -> do
      let image = renderImage size size (textureToImageFn texture)
      _ <- evaluate (VS.length (imageData image))
      pure (Just image)
    _ -> pure Nothing
  imageRef <- newIORef image
  let action = case stage of
        "render" -> do
          fn <- readIORef fnRef
          evaluate (VS.length (imageData (renderImage size size fn)))
        "combined" -> do
          fn <- readIORef fnRef
          fromIntegral <$> evaluate (BL.length (encodePng (renderImage size size fn)))
        "encode" -> do
          stored <- readIORef imageRef
          case stored of
            Just img -> fromIntegral <$> evaluate (BL.length (encodePng img))
            Nothing -> fail "Missing prepared image"
        _ -> fail "Stage must be render, encode or combined"
  _ <- action -- warm compilation, arrays and code paths before measuring
  forM_ [1 .. batches] $ \batch -> do
    performGC
    before <- getRTSStats
    start <- getMonotonicTimeNSec
    replicateM_ iterations action
    -- Account for allocations since the last collection. Include this final
    -- collection in wall/CPU time so the statistics describe the same interval.
    performGC
    end <- getMonotonicTimeNSec
    after <- getRTSStats
    let per value = fromIntegral value / fromIntegral iterations :: Double
    putStrLn (concat
      [ name, ",", stage, ",", show size, ",", show batch, ",", show iterations
      , ",", show (per (end - start) / 1e6)
      , ",", show (per (cpu_ns after - cpu_ns before) / 1e6)
      , ",", show (per (gc_cpu_ns after - gc_cpu_ns before) / 1e6)
      , ",", show (per (allocated_bytes after - allocated_bytes before) / 1e6)
      , ",", show (per (gcs after - gcs before))
      ])

-- The focused profiler uses only boot libraries, so dependency packages need
-- not be rebuilt with profiling. Export resolved constructors from real JSON;
-- do not maintain a second, hand-written copy of the example textures.
coreSource :: [(String, Texture)] -> String
coreSource textures = unlines
  [ "{-# LANGUAGE BangPatterns #-}"
  , "module Main (main) where"
  , "import Texture"
  , "import ColourRamps"
  , "import System.Environment (getArgs)"
  , "import Control.Exception (evaluate)"
  , "import Data.IORef (newIORef, readIORef)"
  , "import Control.Monad (replicateM_)"
  , "textures :: [(String, Texture)]"
  , "textures = " <> show textures
  , "main :: IO ()"
  , "main = do"
  , "  [name, sizeText, countText] <- getArgs"
  , "  let size = read sizeText; count = read countText"
  , "  case lookup name textures of"
  , "    Nothing -> fail name"
  , "    Just texture -> do"
  , "      ref <- newIORef (textureToImageFn texture)"
  , "      replicateM_ count $ do"
  , "        fn <- readIORef ref"
  , "        result <- evaluate (checksum size fn)"
  , "        result `seq` pure ()"
  , "{-# NOINLINE checksum #-}"
  , "checksum size fn = rows 0 0"
  , "  where"
  , "    unit n = (fromIntegral n + 0.5) / fromIntegral size"
  , "    rows !y !acc | y >= size = acc"
  , "                 | otherwise = rows (y+1) (cols y 0 acc)"
  , "    cols !y !x !acc | x >= size = acc"
  , "                   | otherwise = let (r,g,b,a) = fn (unit x) (unit y)"
  , "                                 in cols y (x+1) (acc+r+g+b+a)"
  ]
