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
import qualified Geometry as G
import qualified Cellular as C
import Schema (Schema(..), Variant(..), schema, schemaToValue)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.Golden.Advanced (goldenTest)
import Texture (NoiseStyle (..), Texture (..), Scalar(..), Vector(..), Domain(..), textureToField)
import Perlin (perlin3)
import RampLibrary (LibraryRamp (..), RampLibrary)
import Resolve (resolveDocument)
import Data.Aeson.Types (parseEither)
import TextureJson (parseScalar, parseVector, parseDomain, encodeValuePretty, rampToValue, textureToValue)
import EditorAssets (editorAssets, documentVectors)
import GeometryJson (sdfValue)

vectorTests :: RampLibrary -> [Example] -> TestTree
vectorTests library examples =
  testGroup
    "Shared test vectors"
    [ goldenVsString "test-vectors/schema.json" "test-vectors/schema.json" (pure (encodeValuePretty (schemaToValue schema)))
    , goldenJsonApprox "test-vectors/ramps.json" (rampVectors library examples)
    , goldenJsonApprox "test-vectors/gpu-materials.json" (gpuVectors library examples)
    , goldenJsonApprox "test-vectors/gpu-geometry.json" geometryVectors
    , goldenJsonApprox "frontend/src/generated/metadata.json" (editorAssets library examples)
    , goldenJsonApprox "test-vectors/documents.json" (documentVectors library examples)
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

-- | Every edge case in every mode, every library ramp (clamped), and every
-- ramp the examples use, with the mode they use it with.
rampVectors :: RampLibrary -> [Example] -> Value
rampVectors library examples =
  object ["ramps" .= map rampCase (nub (edgeCases <> libraryCases <> concatMap exampleRamps examples))]
  where
    libraryCases = [(Clamp, libraryRamp ramp) | ramp <- library]
    exampleRamps example =
      either (const []) texturesRamps (resolveDocument library (exampleDocument example))

rampCase :: (RampMode, ColourRamp) -> Value
rampCase (mode, ramp) =
  object
    [ "mode" .= modeName mode
    , "ramp" .= rampToValue ramp
    , "samples" .= [[t, r, g, b, a] | t <- samplePositions ramp, let (r, g, b, a) = evalRamp mode ramp t]
    ]
  where
    modeName m = case m of
      Clamp -> "clamp" :: String
      Wrap -> "wrap"
      Mirror -> "mirror"

-- | A spread of positions inside and outside [0, 1], plus every stop position
-- and points just either side of it, where hard edges and rounding live.
samplePositions :: ColourRamp -> [Double]
samplePositions ramp =
  nub (sort (grid <> around))
  where
    grid = [fromIntegral i / 20 | i <- [-30 .. 50 :: Int]]
    around = case ramp of
      Ramp stops -> concat [[p - 1e-9, p, p + 1e-9] | (p, _) <- stops]
      _ -> []

texturesRamps :: Texture -> [(RampMode, ColourRamp)]
texturesRamps texture =
  case texture of
    Flat _ -> []
    Linear _ _ mode ramp -> [(mode, ramp)]
    Radial _ _ mode ramp -> [(mode, ramp)]
    Circular _ _ mode ramp -> [(mode, ramp)]
    Perlin _ mode ramp -> [(mode, ramp)]
    Fbm _ _ _ _ _ mode ramp -> [(mode, ramp)]
    VectorColour _ -> []
    Colourise _ mode ramp -> [(mode,ramp)]
    InDomain _ base -> texturesRamps base
    Mix _ a b -> texturesRamps a <> texturesRamps b
    Turbulence _ _ _ _ base -> texturesRamps base
    Tiled _ _ _ a b -> texturesRamps a <> texturesRamps b
    Layer top bottom -> texturesRamps top <> texturesRamps bottom

edgeCases :: [(RampMode, ColourRamp)]
edgeCases =
  [ (mode, ramp)
  | ramp <-
      [ Ramp []
      , Ramp [(0.3, red)]
      , Ramp [(0.8, red), (0.2, blue), (0.5, green)]
      , Ramp [(0.25, red), (0.75, blue)]
      , Ramp [(-0.5, red), (1.5, blue)]
      , Ramp [(0.0, red), (0.5, green), (0.5, blue), (0.5, red), (1.0, blue)]
      , Ramp [(0.0, (0.15, 0.2, 0.6, 1.0)), (0.5, (1.0, 1.0, 1.0, 0.0))]
      , Ramp [(0.0, (1.0, 0.0, 0.0, 0.0)), (1.0, (0.0, 0.0, 1.0, 1.0))]
      , Sinusoidal red blue
      ]
  , mode <- [Clamp, Wrap, Mirror]
  ]
  where
    red = (1.0, 0.0, 0.0, 1.0)
    green = (0.0, 1.0, 0.0, 1.0)
    blue = (0.0, 0.0, 1.0, 1.0)


-- Binary-exact coordinates distinguish evaluator arithmetic from input rounding.
-- Neighbours of lattice/ramp boundaries are far enough apart to survive FP32.
gpuPoints :: [(Double, Double, Double)]
gpuPoints =
  [ (-1.5,-0.5,0.25), (-1,0,1), (-1+epsilon,epsilon,1-epsilon)
  , (-epsilon,0.5,0), (0,0,0), (epsilon,0.5,0), (0.25,0.5,0.75)
  , (0.5-epsilon,0.5,0.5), (0.5,0.5,0.5), (0.5+epsilon,0.5,0.5)
  , (0.75,0.25,0.125), (1-epsilon,0,0), (1,1,1), (1+epsilon,0,0)
  , (1.5,2.25,-0.75), (255.5,-256,0.5)
  ]
  where epsilon = 1/65536

gpuVectors :: RampLibrary -> [Example] -> Value
gpuVectors library examples = object
  [ "materials" .= [materialCase name texture | (name,texture) <- cases]
  , "noise" .= [vec p <> [perlin3 x y z] | p@(x,y,z) <- gpuPoints]
  ]
  where
    cases =
      [("ramp-" <> show i, Linear (0,0,0) (1,0,0) mode ramp)
      | (i,(mode,ramp)) <- zip [0::Int ..] edgeCases]
      <> [("fbm-" <> show style, Fbm (3,5,2) 5 0.6 2.1 style Clamp grey) | style <- [Smooth,Billowy,Ridged]]
      <> [("fbm-fallback-" <> show persistence <> "-" <> show style, Fbm (3,5,2) 2 persistence 2.1 style Clamp grey)
         | persistence <- [-1,-2], style <- [Smooth,Billowy,Ridged]]
      <> [("constant-alpha-" <> show alpha <> suffix, texture)
         | alpha <- [-0.5,0.5,2]
         , let colour = (0.2,0.3,0.4,alpha)
               ramp = Linear (0,0,0) (1,0,0) Clamp (Ramp [(0,colour),(1,colour)])
         , (suffix,texture) <- [("",ramp),("-over-red",Layer ramp (Flat (1,0,0,1)))]]
      <> [("nested-warp", Turbulence 0.2 4 0.5 2 (Turbulence 0.1 3 0.6 1.8 (Perlin (3,4,5) Mirror grey)))
         ,("checker-negative", Tiled 3 4 5 (Flat (1,0,0,0.3)) (Flat (0,0,1,0.7)))
         ,("layer-alpha", Layer (Flat (1,0,0,0.3)) (Flat (0,0,1,0.7)))
         ,("layer-transparent", Layer (Flat (1,0,0,0)) (Flat (0,0,1,0)))
         ,("radial-zero-axis", Radial (0,0,0) (0,0,0) Clamp grey)
         ,("radial-pole", Radial (0,0,0) (0,1,0) Mirror grey)
         ,("radial-tilted", Radial (0,0,0) (1,3,2) Clamp grey)
         ,("linear-degenerate", Linear (0,0,0) (0,0,0) Clamp grey)
         ,("circular-zero", Circular (0,0,0) 0 Clamp grey)
         ,("shared-warp", Layer (warp 0.15) (warp (-0.35)))
         ]
      <> [("cellular-" <> show dims <> "-" <> show m <> "-" <> show out <> "-" <> show jitter,Colourise (Remap 0 3 0 1 (Worley dims jitter 4294967295 m out)) Clamp grey) | dims<-[2,3],m<-[C.Euclidean,C.Manhattan,C.Chebyshev],out<-[C.F1,C.F2,C.Gap],jitter<-[0,1]]
      <> [("cell-edge-" <> show dims <> "-" <> show seed,Colourise (CellEdge dims 0.7 seed) Clamp grey) | dims<-[2,3],seed<-[0,2147483648,4294967295]]
      <> [("cell-colour-" <> show seed,VectorColour (CellColour 3 1 seed)) | seed<-[0,2147483648,4294967295]]
      <> [("cell-id-negative",VectorColour (VectorScale (Constant 0.1) (VectorDomain (Translate (2,3,1)) (CellIdentity 3 1 2147483648))))]
      <> [("scalar-default-" <> show (variantType v), Colourise (either error id (parseEither parseScalar (variantDefault v))) Clamp grey) | v <- scalarVariants schema]
      <> [("vector-default-" <> show (variantType v), VectorColour (either error id (parseEither parseVector (variantDefault v)))) | v <- vectorVariants schema]
      <> [("domain-default-" <> show (variantType v), VectorColour (VectorDomain (either error id (parseEither parseDomain (variantDefault v))) Position)) | v <- domainVariants schema]
      <> [("domain-zero-scale",VectorColour (VectorDomain (Scale (0,-2,1)) Position))
         ,("domain-disabled-repeat",VectorColour (VectorDomain (Repeat (0,-1,0)) Position))
         ,("domain-reverse-compose",VectorColour (VectorDomain (Compose (Scale (2,1,1)) (Translate (1,0,0))) Position))
         ,("scalar-zero-octaves",Colourise (Fractal 0 0.5 2 Smooth Noise) Clamp grey)
         ,("scalar-reversed-threshold",Colourise (Threshold 0.8 0.2 Noise) Clamp grey)
         ,("scalar-hard-threshold",Colourise (Threshold 0.5 0.5 (Planar (0,0,0) (1,0,0))) Clamp grey)
         ,("scalar-degenerate-remap",Colourise (Remap 1 1 0.3 0.7 Noise) Clamp grey)
         ]
      <> [("generic-fallback-" <> show persistence <> "-" <> show style,Colourise (Fractal 2 persistence 2 style Noise) Clamp grey) | persistence <- [-1,-2], style <- [Smooth,Billowy,Ridged]]
      <> [("absolute-fallback-" <> show persistence,Colourise (AbsoluteFractal 2 persistence 2 Noise) Clamp grey) | persistence <- [-1,-2]]
      <> [(exampleId e, either error id (resolveDocument library (exampleDocument e)))
         | e <- examples]
    grey = Ramp [(0,(0,0,0,1)),(1,(1,1,1,1))]
    warp amount = Turbulence amount 4 0.6 2.1 (Linear (0,0,0) (1,0,0) Wrap (Ramp [(0,(1,0,0,0.2)),(1,(0,0,1,0.8))]))
    materialCase name texture = object
      [ "name" .= name, "texture" .= textureToValue texture
      , "tolerance" .= (if name `elem` map exampleId examples then 0.00025 else 0.00005 :: Double)
      , "samples" .= [vec p <> rgba (textureToField texture x y z) | p@(x,y,z) <- take 15 gpuPoints]
      ]
    vec (x,y,z) = [x,y,z]
    rgba (r,g,b,a) = [r,g,b,a]


geometryVectors :: Value
geometryVectors = object
  [ "shapes" .= [object ["id" .= G.shapeName shape, "solid" .= sdfValue (G.shapeSolid shape)] | shape <- G.shapes]
  , "cases" .= [object ["name" .= name, "solid" .= sdfValue solid, "samples" .= [vec p <> [G.distance solid p] | p <- points]] | (name,solid) <- cases]
  ]
  where
    cases = [(G.shapeName shape,G.shapeSolid shape) | shape <- G.shapes]
      <> [("union", G.Union (G.Sphere (0,0,0) 0.3) (G.Box (0.5,0.5,0.5) (0.2,0.3,0.4)))
         ,("rounded", G.Rounded 0.1 (G.Sphere (0,0,0) 0.3))]
    points = [(x,y,z) | x <- [-0.25,0.125,0.5,0.875,1.25], y <- [0,0.25,0.5,0.75,1], z <- [0.125,0.5,0.875]]
    vec (x,y,z) = [x,y,z]
