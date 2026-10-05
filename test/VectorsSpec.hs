{-# LANGUAGE OverloadedStrings #-}

-- | Fixtures shared with the frontend's unit tests, kept in test-vectors/ as
-- golden files: if the schema or ramp evaluation changes, these tests fail
-- until the files are regenerated with @stack test --ta --accept@, and the
-- frontend tests then check the TypeScript code against the new files.
module VectorsSpec (vectorTests) where

import ColourRamps (ColourRamp (..), RampMode (..), evalRamp)
import Data.Aeson (Value, object, (.=))
import Data.List (nub, sort)
import Examples (Example (..))
import Schema (schema, schemaToValue)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Texture (Texture (..))
import TextureJson (Document (..), encodeValuePretty, rampToValue)

vectorTests :: [Example] -> TestTree
vectorTests examples =
  testGroup
    "Shared test vectors"
    [ goldenVsString "test-vectors/schema.json" "test-vectors/schema.json" (pure (encodeValuePretty (schemaToValue schema)))
    , goldenVsString "test-vectors/ramps.json" "test-vectors/ramps.json" (pure (encodeValuePretty (rampVectors examples)))
    ]

rampVectors :: [Example] -> Value
rampVectors examples =
  object ["ramps" .= map rampCase (edgeCases <> concatMap (texturesRamps . documentTexture . exampleDocument) examples)]

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
      Sinusoidal _ _ -> []

texturesRamps :: Texture -> [ColourRamp]
texturesRamps texture =
  case texture of
    Flat _ -> []
    Linear _ _ ramp -> [ramp]
    Radial _ ramp -> [ramp]
    Circular _ _ ramp -> [ramp]
    Perlin _ ramp -> [ramp]
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
