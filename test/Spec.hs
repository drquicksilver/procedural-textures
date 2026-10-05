module Main (main) where

import ColourRamps
  ( RampMode (Clamp, Mirror, Wrap)
  , colourRamp
  , evalRamp
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
import Examples (Example, defaultExamplesDirectory, loadExamples)
import GoldenSpec (goldenTests)
import RampLibrary (RampLibrary, defaultRampsDirectory, loadRampLibrary)
import RampLibrarySpec (rampLibraryTests)
import HtmlOutput (GalleryEntry (..), renderGallery)
import Data.List (isInfixOf, isPrefixOf, tails)
import OkLabSpec (okLabTests)
import PNGCompareSpec (pngCompareTests)
import SchemaSpec (schemaTests)
import ServerSpec (serverTests)
import VectorsSpec (vectorTests)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Texture (NoiseStyle (..), Texture (..), fbmFn, textureToImageFn)
import TextureJsonSpec (textureJsonTests)

main :: IO ()
main = do
  library <- loadRampLibrary defaultRampsDirectory
  examples <- loadExamples defaultExamplesDirectory
  defaultMain (tests library examples)

tests :: RampLibrary -> [Example] -> TestTree
tests library examples =
  testGroup
    "procedural-textures"
    [ rampTests
    , textureTests
    , perlinTests
    , okLabTests
    , pngCompareTests
    , galleryTests
    , textureJsonTests examples
    , schemaTests examples
    , serverTests examples
    , rampLibraryTests library
    , vectorTests library examples
    , goldenTests library examples
    ]

rampTests :: TestTree
rampTests =
  testGroup
    "ColourRamps"
    [ testCase "Clamp below and above stops" $ do
        let ramp = twoStopRamp red blue
        assertColourApprox "below" red (evalRamp Clamp ramp (-1.0))
        assertColourApprox "above" blue (evalRamp Clamp ramp 2.0)
    , testCase "Wrap repeats" $ do
        let ramp = twoStopRamp red blue
        assertColourApprox "wrap" (evalRamp Wrap ramp 0.25) (evalRamp Wrap ramp 1.25)
    , testCase "Mirror reverses" $ do
        let ramp = twoStopRamp red blue
        assertColourApprox "mirror" (evalRamp Mirror ramp 0.25) (evalRamp Mirror ramp 1.75)
    , testCase "Discontinuous stop uses last colour at position" $ do
        let ramp =
              colourRamp
                [ (0.0, red)
                , (0.5, green)
                , (0.5, blue)
                , (1.0, white)
                ]
        assertColourApprox "jump" blue (evalRamp Clamp ramp 0.5)
    , testCase "Sinusoidal ramp endpoints" $ do
        let ramp = sinusoidalColourRamp red blue
        assertColourApprox "start" red (evalRamp Mirror ramp 0.0)
        assertColourApprox "back at the start" red (evalRamp Mirror ramp 2.0)
        assertColourApprox "clamped end" blue (evalRamp Clamp ramp 2.0)
    ]

textureTests :: TestTree
textureTests =
  testGroup
    "Texture"
    [ testCase "Flat returns constant colour" $ do
        let f = textureToImageFn (Flat green)
        assertColourApprox "flat" green (f 0.2 0.9)
    , testCase "Linear uses ramp" $ do
        let ramp = twoStopRamp red blue
            f = textureToImageFn (Linear (0.0, 0.0) (1.0, 0.0) Clamp ramp)
        assertColourApprox "linear" (evalRamp Clamp ramp 0.5) (f 0.5 0.2)
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
    , testCase "Fractal noise stays within [0, 1] in every style" $
        sequence_
          [ assertBool (show style <> " " <> show octaves) (all (\v -> v >= 0 && v <= 1) samples)
          | style <- [Smooth, Billowy, Ridged]
          , octaves <- [1, 4, 9]
          , let noise = fbmFn (5, 3) octaves 0.6 2.1 style
                samples = [noise (x / 37) (y / 41) | x <- [-20 .. 60], y <- [-20 .. 60]]
          ]
    , testCase "Fractal noise styles differ and are deterministic" $ do
        let at style = fbmFn (4, 4) 5 0.5 2 style 0.31 0.77
        assertEqual "deterministic" (at Ridged) (at Ridged)
        assertBool "smooth vs ridged" (at Smooth /= at Ridged)
        assertBool "smooth vs billowy" (at Smooth /= at Billowy)
    , testCase "Turbulence amount 0 returns base" $ do
        let base = Linear (0.0, 0.0) (1.0, 0.0) Clamp (twoStopRamp black white)
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

galleryTests :: TestTree
galleryTests =
  testGroup
    "Gallery"
    [ testCase "Cards show the title, description and escaped document" $ do
        let html = renderGallery "Gallery" [GalleryEntry "a.png" "Fish & <Chips>" "Tasty" "natural" "{\"type\": \"flat\"}"]
        assertBool "title" ("Fish &amp; &lt;Chips&gt;" `isInfixOf` html)
        assertBool "description" ("Tasty" `isInfixOf` html)
        assertBool "image" ("src=\"a.png\"" `isInfixOf` html)
        assertBool "code" ("{&quot;type&quot;: &quot;flat&quot;}" `isInfixOf` html)
    , testCase "Cards are grouped by category, known categories first" $ do
        let entry name category = GalleryEntry (name <> ".png") name "" category "{}"
            html = renderGallery "Gallery" [entry "w" "weird", entry "p" "pattern", entry "n" "natural", entry "o" ""]
            position needle = length (takeWhile (not . (needle `isPrefixOf`)) (tails html))
        assertBool "order" (position "<h2>Natural" < position "<h2>Pattern" && position "<h2>Pattern" < position "<h2>Weird" && position "<h2>Weird" < position "<h2>Other")
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
