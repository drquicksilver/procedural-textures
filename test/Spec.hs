module Main (main) where

import ColourRamps
  ( RampMode (Clamp, Mirror, Wrap)
  , colourRamp
  , sinusoidalColourRamp
  , twoStopRamp
  )
import Colours
  ( Colour
  , black
  , blue
  , green
  , red
  , transparent
  , white
  )
import Perlin (perlin2)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Texture (Texture (..), textureToImageFn)

main :: IO ()
main =
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "procedural-textures"
    [ rampTests
    , textureTests
    , perlinTests
    ]

rampTests :: TestTree
rampTests =
  testGroup
    "ColourRamps"
    [ testCase "Clamp below and above stops" $ do
        let ramp = twoStopRamp Clamp red blue
        assertColourApprox "below" red (ramp (-1.0))
        assertColourApprox "above" blue (ramp 2.0)
    , testCase "Wrap repeats" $ do
        let ramp = twoStopRamp Wrap red blue
        assertColourApprox "wrap" (ramp 0.25) (ramp 1.25)
    , testCase "Mirror reverses" $ do
        let ramp = twoStopRamp Mirror red blue
        assertColourApprox "mirror" (ramp 0.25) (ramp 1.75)
    , testCase "Discontinuous stop uses last colour at position" $ do
        let ramp =
              colourRamp
                Clamp
                [ (0.0, red)
                , (0.5, green)
                , (0.5, blue)
                , (1.0, white)
                ]
        assertColourApprox "jump" blue (ramp 0.5)
    , testCase "Sinusoidal ramp endpoints" $ do
        let ramp = sinusoidalColourRamp red blue
        assertColourApprox "start" red (ramp 0.0)
        assertColourApprox "end" red (ramp 2.0)
    ]

textureTests :: TestTree
textureTests =
  testGroup
    "Texture"
    [ testCase "Flat returns constant colour" $ do
        let f = textureToImageFn (Flat green)
        assertColourApprox "flat" green (f 0.2 0.9)
    , testCase "Linear uses ramp" $ do
        let ramp = twoStopRamp Clamp red blue
            f = textureToImageFn (Linear (0.0, 0.0) (1.0, 0.0) ramp)
        assertColourApprox "linear" (ramp 0.5) (f 0.5 0.2)
    , testCase "Tiled alternates" $ do
        let f = textureToImageFn (Tiled 2 2 (Flat red) (Flat blue))
        assertColourApprox "tile-00" red (f 0.1 0.1)
        assertColourApprox "tile-11" red (f 0.6 0.6)
        assertColourApprox "tile-01" blue (f 0.1 0.6)
    , testCase "Layer with transparent top returns bottom" $ do
        let f = textureToImageFn (Layer (Flat transparent) (Flat green))
        assertColourApprox "layer" green (f 0.3 0.7)
    , testCase "Layer with opaque top returns top" $ do
        let f = textureToImageFn (Layer (Flat red) (Flat green))
        assertColourApprox "layer-opaque" red (f 0.3 0.7)
    , testCase "Turbulence amount 0 returns base" $ do
        let base = Linear (0.0, 0.0) (1.0, 0.0) (twoStopRamp Clamp black white)
            fBase = textureToImageFn base
            fWarp = textureToImageFn (Turbulence 0.0 3 0.5 2.0 base)
        assertColourApprox "turbulence" (fBase 0.3 0.7) (fWarp 0.3 0.7)
    ]

perlinTests :: TestTree
perlinTests =
  testGroup
    "Perlin"
    [ testCase "Range is within [0,1]" $ do
        let samples =
              [ perlin2 0.0 0.0
              , perlin2 1.3 2.7
              , perlin2 10.5 42.25
              , perlin2 (-3.1) 7.9
              ]
        mapM_
          (\v -> assertBool "range" (v >= 0.0 && v <= 1.0))
          samples
    , testCase "Deterministic output" $ do
        let v1 = perlin2 0.25 0.75
            v2 = perlin2 0.25 0.75
        assertEqual "deterministic" v1 v2
    ]

assertColourApprox :: String -> Colour -> Colour -> IO ()
assertColourApprox label expected actual =
  assertBool label (colourApprox expected actual)

colourApprox :: Colour -> Colour -> Bool
colourApprox (r1, g1, b1, a1) (r2, g2, b2, a2) =
  and
    [ approx r1 r2
    , approx g1 g2
    , approx b1 b2
    , approx a1 a2
    ]

approx :: Double -> Double -> Bool
approx a b =
  abs (a - b) <= 1.0e-6
