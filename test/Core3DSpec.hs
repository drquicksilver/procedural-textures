{-# LANGUAGE OverloadedStrings #-}
module Core3DSpec (core3DTests) where

import Codec.Picture (convertRGBA8, decodePng)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (Value(..), eitherDecode)
import qualified Data.Aeson.KeyMap as K
import Data.List (find, nub)
import Data.Text (unpack)
import Examples (Example(..))
import Perlin (perlin3)
import Texture (NoiseStyle(..), Texture(..), textureToField, textureToImageFn)
import TextureJson (Document(..), parseDocument)
import ColourRamps (RampMode(..), twoStopRamp)
import Colours (black, white, red, blue)
import Resolve (resolveDocument)
import RampLibrary (RampLibrary)
import Render (renderImage)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)

core3DTests :: RampLibrary -> [Example] -> TestTree
core3DTests library examples = testGroup "3D core"
  [ testCase "Noise is bounded, deterministic and varies through z" $ do
      let samples = [perlin3 (x/7) (y/9) (z/11) | x <- [-12..12], y <- [-12..12], z <- [-3..3]]
      assertBool "bounded" (all (\v -> v >= 0 && v <= 1) samples)
      assertEqual "lattice" 0.5 (perlin3 2 (-3) 7)
      assertBool "depth" (perlin3 0.31 0.77 0 /= perlin3 0.31 0.77 0.6)
      assertEqual "period" (perlin3 0.25 0.5 0.75) (perlin3 256.25 256.5 256.75)
  , testCase "Noise and its first derivative are continuous at lattice boundaries" $ do
      let f x = perlin3 x 0.31 0.67
          h = 1e-5
      assertBool "value" (abs (f (1-h)-f (1+h)) < 1e-4)
      assertBool "derivative" (abs ((f 1-f (1-h))/h - (f (1+h)-f 1)/h) < 1e-3)
  , testCase "Linear projection and spherical shells use z" $ do
      let ramp = twoStopRamp black white
      assertEqual "linear endpoint" white (textureToField (Linear (0,0,0) (0,0,1) Clamp ramp) 0 0 1)
      assertEqual "shell endpoint" white (textureToField (Circular (0,0,0) 1 Clamp ramp) 0 0 1)
  , testCase "Warping and fractal fields use the depth coordinate" $ do
      let base = Linear (0,0,0) (0,0,1) Clamp (twoStopRamp black white)
          warped = textureToField (Turbulence 0.5 4 0.5 2 base)
          original = textureToField base
          fractal = textureToField (Fbm (2,3,4) 4 0.5 2 Smooth Clamp (twoStopRamp black white))
      assertBool "z displacement" (original 0.23 0.61 0.32 /= warped 0.23 0.61 0.32)
      assertBool "fractal depth" (fractal 0.23 0.61 0 /= fractal 0.23 0.61 0.32)
  , testCase "Checker parity changes along z" $ do
      let f = textureToField (Tiled 2 2 2 (Flat red) (Flat blue))
      assertEqual "front" red (f 0.1 0.1 0.1)
      assertEqual "back" blue (f 0.1 0.1 0.6)
  , testCase "Radial is invariant along its cylinder axis" $ do
      let f = textureToField (Radial (0,0,0) (1,0,0) Clamp (twoStopRamp black white))
      assertEqual "axis" (f 0 0.3 0.4) (f 4 0.3 0.4)
  , testCase "Smiley leaves the background visible outside its finite face" $ do
      case find ((== "smiley") . exampleId) examples of
        Nothing -> fail "missing smiley"
        Just e -> do
          texture <- either fail pure (resolveDocument library (exampleDocument e))
          case texture of
            Layer face background -> do
              let f = textureToField texture
                  bg = textureToField background
                  fg = textureToField face
              mapM_ (\(x,y) -> assertEqual "uncovered corner" (bg x y 0) (f x y 0))
                [(0.05,0.05),(0.95,0.05),(0.05,0.95),(0.95,0.95)]
              assertBool "face present" (fg 0.5 0.5 0 /= fg 0.05 0.05 0)
              assertEqual "face projected through depth" (fg 0.5 0.5 0) (fg 0.5 0.5 0.7)
            _ -> fail "expected face over background"
  , testCase "Seamless stone matches values and slopes across repeat boundaries" $ do
      case find ((== "seamless-stone") . exampleId) examples of
        Nothing -> fail "missing seamless stone"
        Just e -> do
          texture <- either fail pure (resolveDocument library (exampleDocument e))
          let f = textureToField texture
              channels (r,g,b,a) = [r,g,b,a]
              near a b tolerance = maximum (zipWith (\x y -> abs (x-y)) (channels a) (channels b)) < tolerance
              h = 1e-5
              slope a b = zipWith (\x y -> (x-y)/h) (channels a) (channels b)
          assertBool "nonconstant stone" (f 0.1 0.2 0 /= f 0.3 0.4 0)
          mapM_ (\(other,z) -> mapM_ (\sample -> do
              assertBool "continuous colour" (near (sample (0.5-h)) (sample (0.5+h)) 0.001)
              let left = slope (sample 0.5) (sample (0.5-h))
                  right = slope (sample (0.5+h)) (sample 0.5)
              assertBool "continuous slope" (maximum (zipWith (\a b -> abs (a-b)) left right) < 0.02))
            [\x -> f x other z, \y -> f other y z]) [(0.13,0),(0.37,0.4),(0.81,0.8)]
  , testCase "Alpha examples distinguish interpolation from added coverage" $ do
      let load name = case find ((== name) . exampleId) examples of
            Nothing -> fail ("missing " <> name)
            Just e -> either fail pure (resolveDocument library (exampleDocument e))
          alpha (_,_,_,a) = a
          half = 128/255
      mixed <- textureToField <$> load "scalar-mix-alpha"
      layered <- textureToField <$> load "source-over-alpha"
      mapM_ (\x -> assertBool "mix preserves input alpha" (abs (alpha (mixed x 0.5 0)-half) < 1e-12)) [0,0.25,0.5,0.75,1]
      assertBool "source-over accumulates coverage" (abs (alpha (layered 1 0.5 0)-(half+half*(1-half))) < 1e-12)
  , testGroup "Planar medallion examples remain visible throughout the cube"
      [ testCase name $ case find ((== name) . exampleId) examples of
          Nothing -> fail ("missing example " <> name)
          Just e -> do
            texture <- either fail pure (resolveDocument library (exampleDocument e))
            let f = textureToField texture
                points = [(0.12 + 0.076 * x, 0.12 + 0.076 * y) | x <- [0..10], y <- [0..10]]
                front = [f x y 0.12 | (x,y) <- points]
            assertBool "cube face contains a pattern" (length (nub front) > 4)
            mapM_ (\(x,y) -> mapM_ (\z -> assertEqual "extruded XY pattern" (f x y 0) (f x y z)) [0.12,0.5,0.88]) points
      | name <- ["offset-medallions", "diagonal-inlay", "stretched-enamel"]
      ]
  , testGroup "Structured gallery sampling contracts"
      [ testCase "Planar subjects are invariant through depth" $ do
          let names = ["truchet-paths", "plain-weave", "denim-twill", "overlapping-fish-scales",
                       "leopard-rosettes", "bamboo-nodes", "travertine-pores", "hierarchical-crackle",
                       "combed-marbled-paper", "variable-halftone", "digital-camouflage", "turtle-scute-growth",
                       "quarter-sawn-rays", "multi-eye-burl", "dalmatian-spots", "octagon-and-dot",
                       "regular-honeycomb", "knitted-loops", "chipped-multicoat-paint", "tile-boundary-wear",
                       "sea-foam-loops", "greek-key-border", "peacock-eye", "fixed-herringbone",
                       "source-linked-stains", "bayer-coverage"]
          mapM_ (\name -> do
            f <- loadExampleField library examples name
            mapM_ (\(x,y) -> assertEqual (name <> " extrusion") (f x y 0) (f x y 0.73))
              [(0.13,0.29),(0.37,0.61),(0.83,0.47)]) names
      , testCase "Porous and inclusion materials vary through depth" $ do
          mapM_ (\name -> do
            f <- loadExampleField library examples name
            assertBool (name <> " volume") (any (\(x,y) -> f x y 0 /= f x y 0.47)
              [(x/13,y/13) | x <- [1..12], y <- [1..12]])) ["cellular-pumice", "salami-cross-section"]
      , testCase "Truchet exits meet across every sampled tile boundary" $ do
          f <- loadExampleField library examples "truchet-paths"
          let near (r,g,b,a) (u,v,w,t) = maximum [abs(r-u),abs(g-v),abs(b-w),abs(a-t)] < 1e-8
          mapM_ (\(edge,centre) -> do
            assertBool "horizontal exit" (near (f (edge-1e-7) centre 0) (f (edge+1e-7) centre 0))
            assertBool "vertical exit" (near (f centre (edge-1e-7) 0) (f centre (edge+1e-7) 0)))
            [((i+0.5)/8,j/8) | i <- [0..7], j <- [0..8]]
      , testCase "Digital camouflage is constant inside each quantised sampling cell" $ do
          f <- loadExampleField library examples "digital-camouflage"
          mapM_ (\(i,j) -> assertEqual "one pigment sample per cell"
            (f ((i+0.2)/32) ((j+0.2)/32) 0) (f ((i+0.8)/32) ((j+0.8)/32) 0))
            [(i,j) | i <- [0..31], j <- [0..31]]
      ]
  , testCase "Every original v3 example migrates to its frozen historical document" $ do
      bytes <- BL.readFile "test/fixtures/phase1-examples.json"
      values <- either fail pure (eitherDecode bytes :: Either String [Value])
      expected <- BL.readFile "test/fixtures/phase1-migrated.json" >>= either fail pure . eitherDecode
      mapM_ (\value -> case value of
        Object o -> case (K.lookup "id" o, K.lookup "document" o) of
          (Just (String name), Just old) -> case [d | Object item <- (expected :: [Value]), Just (String key) <- [K.lookup "id" item], key == name, Just d <- [K.lookup "document" item]] of
            [document] -> assertEqual (unpack name) (parseDocument document) (parseDocument old)
            _ -> fail "missing frozen migrated example"
          _ -> fail "bad legacy fixture"
        _ -> fail "bad fixture") values
  , testCase "Non-noise z=0 slices retain all original 2D pixels" $ do
      fixture <- BL.readFile "test/fixtures/phase1-examples.json" >>= either fail pure . eitherDecode
      let ids = [unpack name | Object o <- (fixture :: [Value]), Just (String name) <- [K.lookup "id" o]]
      mapM_ (\e -> if hasNoise (documentTexture (exampleDocument e)) then pure () else do
        bytes <- B.readFile ("golden/legacy-2d/" <> exampleId e <> ".png")
        old <- either fail (pure . convertRGBA8) (decodePng bytes)
        texture <- either fail pure (resolveDocument library (exampleDocument e))
        assertBool (exampleId e <> " exact pixels") (old == renderImage 128 128 (textureToImageFn texture))) (filter ((`elem` ids) . exampleId) examples)
  ]
  where
    hasNoise (Perlin _ _ _) = True
    hasNoise (Fbm _ _ _ _ _ _ _) = True
    hasNoise (Turbulence _ _ _ _ _) = True
    hasNoise (Layer a b) = hasNoise a || hasNoise b
    hasNoise (Tiled _ _ _ a b) = hasNoise a || hasNoise b
    hasNoise _ = False

loadExampleField :: RampLibrary -> [Example] -> String -> IO (Double -> Double -> Double -> (Double, Double, Double, Double))
loadExampleField library examples name = case find ((== name) . exampleId) examples of
  Nothing -> fail ("missing " <> name)
  Just e -> textureToField <$> either fail pure (resolveDocument library (exampleDocument e))
