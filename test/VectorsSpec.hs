{-# LANGUAGE OverloadedStrings #-}

-- | Fixtures shared with the frontend's unit tests, kept in test-vectors/ as
-- golden files: if the schema or ramp evaluation changes, these tests fail
-- until the files are regenerated with @stack test --ta --accept@, and the
-- frontend tests then check the TypeScript code against the new files.
--
-- Sampled ramps are compared number by number within 'vectorTolerance', not
-- byte by byte: functions such as 'cos' come from the platform's maths
-- library, and macOS and Linux can differ in the last bit.
module VectorsSpec (vectorTests) where

import ColourRamps (ColourRamp (..), RampMode (..), evalRamp)
import Data.Aeson (Value (..), eitherDecode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Scientific (toRealFloat)
import Data.List (nub, sort)
import Examples (Example (..))
import Schema (schema, schemaToValue)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.Golden.Advanced (goldenTest)
import Texture (Texture (..))
import RampLibrary (LibraryRamp (..), RampLibrary)
import Resolve (resolveDocument)
import TextureJson (encodeValuePretty, rampToValue)

vectorTests :: RampLibrary -> [Example] -> TestTree
vectorTests library examples =
  testGroup
    "Shared test vectors"
    [ goldenVsString "test-vectors/schema.json" "test-vectors/schema.json" (pure (encodeValuePretty (schemaToValue schema)))
    , goldenJsonApprox "test-vectors/ramps.json" (rampVectors library examples)
    ]

-- | Same tolerance as the frontend's check against these vectors.
vectorTolerance :: Double
vectorTolerance = 1e-12

goldenJsonApprox :: FilePath -> Value -> TestTree
goldenJsonApprox path value =
  goldenTest
    path
    (BL.readFile path >>= either fail pure . eitherDecode)
    (pure value)
    (\golden actual -> pure (firstDifference "$" golden actual))
    (BL.writeFile path . encodeValuePretty)

-- | The first place two JSON values differ, allowing numbers to differ by
-- 'vectorTolerance' (relative to their size, for numbers above 1).
firstDifference :: String -> Value -> Value -> Maybe String
firstDifference path golden actual =
  case (golden, actual) of
    (Number a, Number b)
      | abs (x - y) <= vectorTolerance * max 1 (abs x) -> Nothing
      | otherwise -> Just (path <> ": expected " <> show x <> ", got " <> show y)
      where
        x = toRealFloat a :: Double
        y = toRealFloat b
    (Array as, Array bs)
      | length as /= length bs -> Just (path <> ": expected " <> show (length as) <> " items, got " <> show (length bs))
      | otherwise -> listToMaybe (catMaybes (zipWith3 (\i a b -> firstDifference (path <> "[" <> show i <> "]") a b) [0 :: Int ..] (toList as) (toList bs)))
    (Object as, Object bs)
      | KeyMap.keys as /= KeyMap.keys bs -> Just (path <> ": keys differ")
      | otherwise ->
          listToMaybe
            (catMaybes [firstDifference (path <> "." <> Key.toString k) a b | (k, a) <- KeyMap.toList as, Just b <- [KeyMap.lookup k bs]])
    _
      | golden == actual -> Nothing
      | otherwise -> Just (path <> ": values differ")

-- | Edge cases, every library ramp, and every ramp the examples use.
rampVectors :: RampLibrary -> [Example] -> Value
rampVectors library examples =
  object ["ramps" .= map rampCase (nub (edgeCases <> map libraryRamp library <> concatMap exampleRamps examples))]
  where
    exampleRamps example =
      either (const []) texturesRamps (resolveDocument library (exampleDocument example))

rampCase :: ColourRamp -> Value
rampCase ramp =
  object
    [ "ramp" .= rampToValue ramp
    , "samples" .= [[t, r, g, b, a] | t <- samplePositions ramp, let (r, g, b, a) = evalRamp ramp t]
    ]

-- | A spread of positions inside and outside [0, 1], plus every stop position
-- and points just either side of it, where hard edges and rounding live.
samplePositions :: ColourRamp -> [Double]
samplePositions ramp =
  nub (sort (grid <> around))
  where
    grid = [fromIntegral i / 20 | i <- [-30 .. 50 :: Int]]
    around = case ramp of
      Ramp _ stops -> concat [[p - 1e-9, p, p + 1e-9] | (p, _) <- stops]
      _ -> []

texturesRamps :: Texture -> [ColourRamp]
texturesRamps texture =
  case texture of
    Flat _ -> []
    Linear _ _ ramp -> [ramp]
    Radial _ ramp -> [ramp]
    Circular _ _ ramp -> [ramp]
    Perlin _ ramp -> [ramp]
    Fbm _ _ _ _ _ ramp -> [ramp]
    Turbulence _ _ _ _ base -> texturesRamps base
    Tiled _ _ a b -> texturesRamps a <> texturesRamps b
    Layer top bottom -> texturesRamps top <> texturesRamps bottom

edgeCases :: [ColourRamp]
edgeCases =
  [ Ramp Clamp []
  , Ramp Wrap [(0.3, red)]
  , Ramp Clamp [(0.8, red), (0.2, blue), (0.5, green)]
  , Ramp Mirror [(0.25, red), (0.75, blue)]
  , Ramp Wrap [(-0.5, red), (1.5, blue)]
  , Ramp Clamp [(0.0, red), (0.5, green), (0.5, blue), (0.5, red), (1.0, blue)]
  , Ramp Wrap [(0.0, (0.15, 0.2, 0.6, 1.0)), (0.5, (1.0, 1.0, 1.0, 0.0))]
  , Sinusoidal red blue
  ]
  where
    red = (1.0, 0.0, 0.0, 1.0)
    green = (0.0, 1.0, 0.0, 1.0)
    blue = (0.0, 0.0, 1.0, 1.0)
