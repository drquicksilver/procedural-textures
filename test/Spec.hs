module Main (main) where

import SceneSpec (sceneTests)
import Core3DSpec (core3DTests)
import ContactSheetSpec (contactSheetTests)
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
import Examples (Example (..), defaultExamplesDirectory, loadExamples)
import Gallery (selectShapeMaterials, shapeMaterials, shapeTitle)
import Geometry (shapes)
import GoldenSpec (goldenTests)
import RampLibrary (RampLibrary, defaultRampsDirectory, loadRampLibrary)
import RampLibrarySpec (rampLibraryTests)
import HtmlOutput (GalleryEntry (..), SiteLink (..), renderGallery, renderShapePage, renderSiteIndex, renderSolidGallery)
import Data.List (isInfixOf, isPrefixOf, nub, tails)
import OkLabSpec (okLabTests)
import PNGCompareSpec (pngCompareTests)
import SchemaSpec (schemaTests)
import ServerSpec (serverTests)
import VectorsSpec (vectorTests)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Texture (NoiseStyle (..), Texture (..), fbmFn, textureToImageFn, textureToField)
import TextureJsonSpec (textureJsonTests)

main :: IO ()
main = do
  library <- loadRampLibrary defaultRampsDirectory
  examples <- loadExamples defaultExamplesDirectory
  sheetTests <- contactSheetTests
  defaultMain (testGroup "All tests" [tests library examples, sheetTests])

tests :: RampLibrary -> [Example] -> TestTree
tests library examples =
  testGroup
    "procedural-textures"
    [ sceneTests library examples
    , core3DTests library examples
    , rampTests
    , textureTests
    , perlinTests
    , okLabTests
    , pngCompareTests
    , galleryTests examples
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
            f = textureToImageFn (Linear (0.0, 0.0, 0) (1.0, 0.0, 0) Clamp ramp)
        assertColourApprox "linear" (evalRamp Clamp ramp 0.5) (f 0.5 0.2)
    , testCase "Tiled alternates" $ do
        let f = textureToImageFn (Tiled 2 2 1 (Flat red) (Flat blue))
        assertColourApprox "tile-00" red (f 0.1 0.1)
        assertColourApprox "tile-11" red (f 0.6 0.6)
        assertColourApprox "tile-01" blue (f 0.1 0.6)
    , testCase "Layer with transparent top returns bottom" $ do
        let f = textureToImageFn (Layer (Flat transparent) (Flat green))
        assertColourApprox "layer" green (f 0.3 0.7)
    , testCase "Layer with opaque top returns top" $ do
        let f = textureToImageFn (Layer (Flat red) (Flat green))
        assertColourApprox "layer-opaque" red (f 0.3 0.7)
    , testCase "Opaque layer does not evaluate the hidden colour" $ do
        let f = textureToImageFn (Layer (Flat red) (Flat (error "hidden colour evaluated")))
        assertEqual "opaque" red (f 0.3 0.7)
    , testCase "Shared warps preserve opaque-layer skipping" $ do
        let warp = Turbulence 0.3 3 0.5 2.0
            f = textureToImageFn (Layer (warp (Flat red)) (warp (Flat (error "hidden shared colour evaluated"))))
        assertEqual "opaque shared" red (f 0.3 0.7)
    , testCase "Shared displacement preserves different warp amplitudes" $ do
        let a = Turbulence 0.15 4 0.6 2.1 translucentField
            b = Turbulence (-0.35) 4 0.6 2.1 translucentField
        assertIndependentLayers a b
    , testCase "Nested warp domains keep their own displacement samples" $ do
        let warp = Turbulence 0.25 3 0.5 2.0
            a = warp (Layer (warp translucentField) (Turbulence (-0.4) 3 0.5 2.0 translucentField))
            b = Turbulence (-0.15) 3 0.5 2.0 translucentField
        assertIndependentLayers a b
    , testCase "Different displacement parameters are not shared" $ do
        let a = Turbulence 0.3 3 0.5 2.0 translucentField
            b = Turbulence 0.3 4 0.7 2.2 translucentField
        assertIndependentLayers a b
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
        let base = Linear (0.0, 0.0, 0) (1.0, 0.0, 0) Clamp (twoStopRamp black white)
            fBase = textureToImageFn base
            fWarp = textureToImageFn (Turbulence 0.0 3 0.5 2.0 base)
        assertColourApprox "turbulence" (fBase 0.3 0.7) (fWarp 0.3 0.7)
    ]


-- Compare composition with separately compiled domains, so the reference does
-- not use sibling displacement sharing. Preserve the original blend arithmetic.
translucentField :: Texture
translucentField =
  Linear (-0.2, 0.1, -0.3) (1.2, 0.8, 0.6) Clamp
    (twoStopRamp (0.1, 0.2, 0.7, 0.25) (0.8, 0.4, 0.1, 0.65))

assertIndependentLayers :: Texture -> Texture -> IO ()
assertIndependentLayers top bottom = do
  let f = textureToField (Layer top bottom)
      topFn = textureToField top
      bottomFn = textureToField bottom
      reference (r1, g1, b1, a1) (r2, g2, b2, a2) =
        let a = a1 + a2 * (1.0 - a1)
            weightTop = if a <= 0.0 then 0.0 else a1 / a
            weightBottom = 1.0 - weightTop
            component v1 v2 = v1 + (v2 - v1) * weightBottom
        in (component r1 r2, component g1 g2, component b1 b2, a)
  sequence_
    [ assertEqual (show (x, y, z)) (reference (topFn x y z) (bottomFn x y z)) (f x y z)
    | (x, y) <- [(-0.3, 0.7), (0.0, 0.0), (0.13, 0.91), (0.4, 0.6), (1.2, -0.4)]
    , z <- [0, 0.4, 1.2]
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

galleryTests :: [Example] -> TestTree
galleryTests examples =
  testGroup
    "Gallery"
    [ testCase "Cards show the title, description and escaped document" $ do
        let html = renderGallery "Gallery" [GalleryEntry "a.png" "Fish & <Chips>" "Tasty" "natural" "{\"type\": \"flat\"}"]
        assertBool "title" ("Fish &amp; &lt;Chips&gt;" `isInfixOf` html)
        assertBool "description" ("Tasty" `isInfixOf` html)
        assertBool "image" ("src=\"a.png\"" `isInfixOf` html)
        assertBool "code" ("{&quot;type&quot;: &quot;flat&quot;}" `isInfixOf` html)
    , testCase "3D gallery labels and escapes both views of each material" $ do
        let html = renderSolidGallery "Materials" [GalleryEntry ("solid&.png", "slice<.png") "Stone" "" "natural" "{}"]
        assertBool "solid" ("src=\"solid&amp;.png\"" `isInfixOf` html)
        assertBool "slice" ("src=\"slice&lt;.png\"" `isInfixOf` html)
        assertBool "captions" ("3D cutaway" `isInfixOf` html && "XY slice" `isInfixOf` html)
    , testCase "Cards are grouped by category, known categories first" $ do
        let entry name category = GalleryEntry (name <> ".png") name "" category "{}"
            html = renderGallery "Gallery" [entry "w" "weird", entry "p" "pattern", entry "n" "natural", entry "o" ""]
            position needle = length (takeWhile (not . (needle `isPrefixOf`)) (tails html))
        assertBool "order" (position "<h2>Natural" < position "<h2>Pattern" && position "<h2>Pattern" < position "<h2>Weird" && position "<h2>Weird" < position "<h2>Other")
    , testCase "The site index links to each page with an escaped preview" $ do
        let html = renderSiteIndex "Site" [SiteLink "gallery.html" "a&b.png" "Gallery" "All <of> it", SiteLink "shape-cube.html" "c.png" "Cube" ""]
        assertBool "gallery link" ("href=\"gallery.html\"" `isInfixOf` html)
        assertBool "shape link" ("href=\"shape-cube.html\"" `isInfixOf` html)
        assertBool "preview" ("src=\"a&amp;b.png\"" `isInfixOf` html)
        assertBool "description" ("All &lt;of&gt; it" `isInfixOf` html)
    , testCase "Shape pages keep material order and link back to the index" $ do
        let entry name = GalleryEntry (name <> ".png") name "" "natural" "{}"
            html = renderShapePage "Torus" "index.html" "torus" [entry "Walnut", entry "Agate"]
            position needle = length (takeWhile (not . (needle `isPrefixOf`)) (tails html))
        assertBool "back link" ("href=\"index.html\"" `isInfixOf` html)
        assertBool "alt text" ("alt=\"Agate on a torus\"" `isInfixOf` html)
        assertBool "order" (position "src=\"Walnut.png\"" < position "src=\"Agate.png\"")
        assertBool "no category headings" (not ("<h2>" `isInfixOf` html))
    , testCase "Custom showcases only select supplied materials in deterministic order" $ do
        assertEqual "repository unchanged" shapeMaterials (selectShapeMaterials (map exampleId examples))
        assertEqual "checker only" ["checker"] (selectShapeMaterials ["checker"])
        assertEqual "fallback order and duplicates" ["walnut","checker","custom"] (selectShapeMaterials ["custom","checker","walnut","checker"])
        assertEqual "empty" [] (selectShapeMaterials [])
    , testCase "Empty libraries have a useful gallery and no missing preview" $ do
        let gallery = renderSolidGallery "Empty" []
            index = renderSiteIndex "Empty" [SiteLink "gallery.html" "" "Materials" "An empty library"]
        assertBool "empty explanation" ("No materials in this library" `isInfixOf` gallery)
        assertBool "gallery remains reachable" ("href=\"gallery.html\"" `isInfixOf` index)
        assertBool "no missing image" (not ("<img" `isInfixOf` index))
    , testCase "Every shape page shows the same six distinct examples, agate and walnut included" $ do
        assertEqual "count" 6 (length (nub shapeMaterials))
        assertBool "agate and walnut" (all (`elem` shapeMaterials) ["agate", "walnut"])
        mapM_ (\name -> assertBool name (name `elem` map exampleId examples)) shapeMaterials
        assertEqual "shape titles" (length shapes) (length (nub (map shapeTitle shapes)))
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
