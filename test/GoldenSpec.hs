module GoldenSpec
  ( goldenTests
  , goldenTextureTest
  , goldenViewTest
  ) where

import Codec.Picture (Image, PixelRGBA8, convertRGBA8, decodePng, encodePng)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Examples (Example (..))
import RampLibrary (RampLibrary)
import Resolve (resolveDocument)
import PNGCompareCore (CompareResult (..), compareRgbaImages, defaultTolerance, withinTolerance)
import Render (renderImage)
import Scene (View, renderView)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit (assertFailure, testCase)
import Texture (Texture, textureToImageFn)
import Text.Printf (printf)

-- | Golden images live in golden/textures. Regenerate them deliberately with
-- @stack test --ta --accept@ and say why in the commit.
goldenTests :: RampLibrary -> [Example] -> TestTree
goldenTests library examples =
  testGroup
    "Golden"
    [ either (\err -> testCase (exampleId example) (assertFailure err)) (goldenTextureTest (exampleId example)) (resolveDocument library (exampleDocument example))
    | example <- examples
    ]

goldenSize :: Int
goldenSize = 128

goldenTextureTest :: String -> Texture -> TestTree
goldenTextureTest name texture =
  goldenTest
    name
    (readGolden goldenPath)
    (pure (renderImage goldenSize goldenSize (textureToImageFn texture)))
    compareImages
    (BL.writeFile goldenPath . encodePng)
  where
    goldenPath = "golden" </> "textures" </> (name <> ".png")

-- | Reading with 'B.readFile' throws a does-not-exist error for a missing
-- golden file, which tasty-golden turns into "create it" under --accept.
readGolden :: FilePath -> IO (Image PixelRGBA8)
readGolden path = do
  bytes <- B.readFile path
  either (\err -> fail ("Cannot decode " <> path <> ": " <> err)) (pure . convertRGBA8) (decodePng bytes)

compareImages :: Image PixelRGBA8 -> Image PixelRGBA8 -> IO (Maybe String)
compareImages golden actual =
  pure $
    case compareRgbaImages golden actual of
      Left err -> Just err
      Right result
        | withinTolerance defaultTolerance result -> Nothing
        | otherwise ->
            Just (printf "Rendered image differs from golden: mean %.6f, max %.6f" (meanError result) (maxError result))

-- | Deliberate scene goldens cover the camera, surface normals and cutouts.
goldenViewTest :: String -> View -> Texture -> TestTree
goldenViewTest name view texture =
  goldenTest name (readGolden path) (pure (renderView 96 view texture)) compareImages
    (BL.writeFile path . encodePng)
  where path = "golden" </> "scenes" </> (name <> ".png")
