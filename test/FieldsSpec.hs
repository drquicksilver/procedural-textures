module FieldsSpec (fieldTests) where

import Data.Aeson.Types (parseEither)
import Texture
import qualified Geometry as G
import TextureJson (parseScalar, scalarToValue, parseVector, vectorToValue, parseDomain, domainToValue)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

fieldTests :: TestTree
fieldTests = testGroup "Composable fields"
  [ testCase "SDF scalar primitives share exact geometry distances" $ do
      let fields=[(SdfSphere (0.5,0.5,0.5) 0.3,G.Sphere (0.5,0.5,0.5) 0.3),(SdfBox (0,0,0) (1,2,3),G.Box (0,0,0) (1,2,3)),(SdfCylinder (0,0,0) 1 2,G.Cylinder (0,0,0) 1 2),(SdfTorus (0,0,0) 1 0.2,G.Torus (0,0,0) 1 0.2),(SdfPlane (0,0,0) 0.2,G.Plane (0,0,0) 0.2)]
      mapM_ (\(f,g) -> mapM_ (\p -> assertEqual "shared distance" (G.distance g p) (scalarField f p)) [(0,0,0),(1,2,3),(-1,0.2,0.4)]) fields
      mapM_ (\(f,_) -> assertEqual "round trip" (Right f) (parseEither parseScalar (scalarToValue f))) fields
  , testCase "Hard and smooth SDF combinations preserve signs and degenerate smoothing" $ do
      let f op subtractB k=SdfCombine op subtractB k (Constant (-0.2)) (Constant (-0.2))
      close "hard union" (-0.2) (scalarField (f Minimum False 0) (0,0,0))
      close "smooth union" (-0.3) (scalarField (f Minimum False 0.4) (0,0,0))
      close "smooth intersection" (-0.1) (scalarField (f Maximum False 0.4) (0,0,0))
      close "difference" 0.2 (scalarField (f Maximum True 0) (0,0,0))
      close "negative radius is hard" (-0.2) (scalarField (f Minimum False (-1)) (0,0,0))
      close "geometry zero blend" 2 (G.distance (G.Blend 0 (G.Plane (1,0,0) 0) (G.Plane (1,0,0) (-1))) (2,0,0))
  , testCase "Composition applies first then second, and is noncommutative" $ do
      let a=Translate (1,0,0); b=Scale (2,1,1); p=(3,2,1)
      assertEqual "first then second" (1,2,1) (domainField (Compose a b) p)
      assertEqual "reversed" (0.5,2,1) (domainField (Compose b a) p)
      assertEqual "nested application" (scalarField (ScalarDomain a (ScalarDomain b (Planar (0,0,0) (1,0,0)))) p)
        (scalarField (ScalarDomain (Compose a b) (Planar (0,0,0) (1,0,0))) p)
  , testCase "Scale collapse, disabled repeat axes and negative cells are defined" $ do
      assertEqual "collapse" (0,-1,3) (domainField (Scale (0,-2,1)) (7,2,3))
      assertEqual "centred cell" (0.25,-0.25,3) (domainField (Repeat (1,1,0)) (-0.75,0.75,3))
      assertEqual "disabled" (-2,3,4) (domainField (Repeat (0,-1,0)) (-2,3,4))
  , testCase "Mirror folds selected axes, preserving centre and depth" $
      assertEqual "fold" (0.75,0.3,2) (domainField (MirrorDomain (0.5,0.5,1) (1,0,0)) (0.25,0.3,2))
  , testCase "Inverse Euler rotation undoes a quarter turn" $ do
      let (x,y,z)=domainField (Rotate (0,0,90)) (0,1,2)
      close "x" 1 x; close "y" 0 y; close "z" 2 z
  , testCase "Polar origin and radial origin are finite" $ do
      assertEqual "polar centre" (0.5,0.5,3) (domainField (PolarRepeat (0.5,0.5,1) 8) (0.5,0.5,3))
      assertEqual "radial centre" (0.5,0.5,3) (domainField (RadialRepeat (0.5,0.5,1) 0.2) (0.5,0.5,3))
      let (_,y,z)=domainField (PolarRepeat (0,0,0) 4) (0,1,7)
      close "sector" 0 y; close "depth" 7 z
  , testCase "Twist varies with height; bend varies with horizontal position" $ do
      let (x,y,z)=domainField (Twist (0,0,0) 90) (1,0,1)
      close "twisted x" 0 x; close "twisted y" (-1) y; close "twisted z" 1 z
      let (bx,by,_)=domainField (Bend (0,0,0) 90) (1,0,0)
      close "bent x" 0 bx; close "bent y" (-1) by
  , testCase "A scalar mask attenuates an arbitrary vector displacement" $ do
      let displacement=VectorScale (Planar (0,0,0) (1,0,0)) (VectorConstant (2,0,1))
      assertEqual "masked warp" (1,0,0.25) (domainField (Warp 0.5 displacement) (0.5,0,0))
  , testCase "Field arithmetic separates broad shape and fine detail" $ do
      let broad=Planar (0,0,0) (1,0,0)
          detail=Arithmetic Multiply (Constant 0.2) (Threshold 0.4 0.6 broad)
          field=Arithmetic Add broad detail
      close "valley" 0.2 (scalarField field (0.2,0,0))
      close "ridge" 1 (scalarField field (0.8,0,0))
      close "reversed remap" 0.75 (scalarField (Remap 1 0 0 1 broad) (0.25,0,0))
      close "degenerate remap" 7 (scalarField (Remap 1 1 7 8 broad) (0.25,0,0))
      close "hard threshold at edge" 1 (scalarField (Threshold 0.5 0.5 broad) (0.5,0,0))
  , testCase "Explicit scalar mixing is independent of colour alpha" $ do
      let texture=Mix (Constant 0.25) (Flat (1,0,0,0.2)) (Flat (0,0,1,0.8))
          (r,g,b,a)=textureToField texture 0 0 0
      close "red" 0.25 r; close "green" 0 g; close "blue" 0.75 b; close "alpha" 0.65 a
  , testCase "Generic fractal source preserves legacy Perlin conventions" $ do
      let field=Fractal 5 0.6 2.1 Ridged Noise
      mapM_ (\p@(x,y,z) -> close "fractal" (fbm3Fn (1,1,1) 5 0.6 2.1 Ridged x y z) (scalarField field p)) [(0.1,0.2,0.3),(-1,2,0.4),(0,0,0)]
  , testCase "Generic source and fallback mappings are composable" $ do
      close "constant source" 0.25 (scalarField (Fractal 3 0.5 2 Smooth (Constant 0.375)) (2,3,4))
      close "billowy fallback" 0.875 (scalarField (Fractal 2 (-1) 2 Billowy Noise) (2,3,4))
      close "absolute fallback" 0 (scalarField (AbsoluteFractal 2 (-1) 2 Noise) (2,3,4))
      close "absolute constant" 0.5 (scalarField (AbsoluteFractal 3 0.5 2 (Constant 0.75)) (2,3,4))
  , testCase "Typed expressions round-trip and reject wrong edge types" $ do
      let s=ScalarDomain (Warp 0.2 (Components Noise (Constant 0) Noise)) (Fractal 3 0.5 2 Smooth Noise)
          v=VectorScale (Threshold 0.2 0.8 Noise) Position
          d=Compose (Translate (0.5,0.5,0)) (Rotate (20,30,40))
      assertEqual "scalar" (Right s) (parseEither parseScalar (scalarToValue s))
      assertEqual "vector" (Right v) (parseEither parseVector (vectorToValue v))
      assertEqual "domain" (Right d) (parseEither parseDomain (domainToValue d))
      assertBool "vector not scalar" (either (const True) (const False) (parseEither parseScalar (vectorToValue v)))
  ]
  where close label expected actual = assertBool (label <> ": " <> show actual) (abs (actual-expected) < 1e-12)
