{-# LANGUAGE OverloadedStrings #-}
module Core3DSpec (core3DTests) where

import Codec.Picture (convertRGBA8, decodePng)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (Value(..), eitherDecode)
import qualified Data.Aeson.KeyMap as K
import Data.List (find)
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
  , testCase "Every original v3 example migrates to its v4 source document" $ do
      bytes <- BL.readFile "test/fixtures/phase1-examples.json"
      values <- either fail pure (eitherDecode bytes :: Either String [Value])
      mapM_ (\value -> case value of
        Object o -> case (K.lookup "id" o, K.lookup "document" o) of
          (Just (String name), Just old) -> case find ((== unpack name) . exampleId) examples of
            Just e -> assertEqual (unpack name) (Right (exampleDocument e)) (parseDocument old)
            Nothing -> fail "missing migrated example"
          _ -> fail "bad legacy fixture"
        _ -> fail "bad fixture") values
  , testCase "Non-noise z=0 slices retain all original 2D pixels" $ do
      mapM_ (\e -> if hasNoise (documentTexture (exampleDocument e)) then pure () else do
        bytes <- B.readFile ("golden/legacy-2d/" <> exampleId e <> ".png")
        old <- either fail (pure . convertRGBA8) (decodePng bytes)
        texture <- either fail pure (resolveDocument library (exampleDocument e))
        assertBool (exampleId e <> " exact pixels") (old == renderImage 128 128 (textureToImageFn texture))) examples
  ]
  where
    hasNoise (Perlin _ _ _) = True
    hasNoise (Fbm _ _ _ _ _ _ _) = True
    hasNoise (Turbulence _ _ _ _ _) = True
    hasNoise (Layer a b) = hasNoise a || hasNoise b
    hasNoise (Tiled _ _ _ a b) = hasNoise a || hasNoise b
    hasNoise _ = False
