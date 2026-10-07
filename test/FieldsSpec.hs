module FieldsSpec (fieldTests) where

import Data.Aeson.Types (parseEither)
import Texture
import qualified Geometry as G
import qualified Cellular as C
import Vector3 (sub,dot,norm,add,mul)
import Data.List (sortOn)
import TextureJson (parseScalar, scalarToValue, parseVector, vectorToValue, parseDomain, domainToValue)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

fieldTests :: TestTree
fieldTests = testGroup "Composable fields"
  [ testCase "Regular lattice distances, identities and exact bisectors" $ do
      let p=(0.75,0.5,9)
      close "F1" 0.25 (C.sample 2 0 0 C.Euclidean C.F1 p)
      close "F2" 0.75 (C.sample 2 0 0 C.Euclidean C.F2 p)
      close "gap" 0.5 (C.sample 2 0 0 C.Euclidean C.Gap p)
      close "true edge, not gap" 0.25 (C.edge 2 0 0 p)
      close "corner edge" 0 (C.edge 3 0 0 (1,1,1))
      assertEqual "negative identity" (-1,-2,0) (C.identity 2 0 17 (-0.25,-1.5,99))
      assertEqual "2D ignores depth" (C.colour 2 1 42 (0.2,0.3,0)) (C.colour 2 1 42 (0.2,0.3,123))
  , testCase "Cellular bounded search agrees with exhaustive larger neighbourhood" $ do
      let points=[(0.01,0.99,0.02),(-1.23,2.78,-0.49),(3.5,-2.5,0.5)]
          metric m (x,y,z)=case m of C.Euclidean -> sqrt(x*x+y*y+z*z); C.Manhattan -> abs x+abs y+abs z; C.Chebyshev -> max (abs x) (max (abs y) (abs z))
          candidates dims p@(x,y,z)=[((a,b,c),C.feature dims 1 4294967295 (a,b,c)) | a<-[floor x-7..floor x+7],b<-[floor y-7..floor y+7],c<-if dims==2 then [0] else [floor z-7..floor z+7]]
      mapM_ (\(dims,point,m) -> do
        let p=if dims==2 then let (x,y,_)=point in (x,y,0) else point
            sites=candidates dims p
            sorted=sortOn (metric m . (`sub` p) . snd) sites
            a=metric m (sub (snd (head sorted)) p); b=metric m (sub (snd (sorted!!1)) p)
        close "first" a (C.sample dims 1 4294967295 m C.F1 point)
        close "second" b (C.sample dims 1 4294967295 m C.F2 point)
        if m/=C.Euclidean then pure () else do
          let (ident,q)=head sorted
              expected=minimum [dot (sub (mul 0.5 (add q r)) p) v/norm v | (id',r)<-sites,id'/=ident,let v=sub r q]
          close "edge" expected (C.edge dims 1 4294967295 point)
          assertBool "edge nonnegative" (expected>=0)
        ) [(d,p,m) | d<-[2,3],p<-points,m<-[C.Euclidean,C.Manhattan,C.Chebyshev]]
  , testCase "Seeds, jitter clamps and typed cellular projections" $ do
      let p=(0.2,-0.1,0.3)
      assertEqual "clamp low" (C.sample 3 0 13 C.Manhattan C.F2 p) (C.sample 3 (-2) 13 C.Manhattan C.F2 p)
      assertEqual "clamp high" (C.sample 3 1 13 C.Chebyshev C.Gap p) (C.sample 3 9 13 C.Chebyshev C.Gap p)
      assertBool "seed changes sites" (C.feature 3 1 13 (0,0,0)/=C.feature 3 1 14 (0,0,0))
      assertBool "cell value constant inside cell" (C.value 2 0 42 (0.2,0.3,0)==C.value 2 0 42 (0.8,0.7,9))
      mapM_ (\f -> assertEqual "scalar round trip" (Right f) (parseEither parseScalar (scalarToValue f))) [Worley 3 0.4 4294967295 C.Manhattan C.F2,CellValue 2 0 19,CellEdge 3 1 13]
      mapM_ (\f -> assertEqual "vector round trip" (Right f) (parseEither parseVector (vectorToValue f))) [CellIdentity 2 1 19,CellColour 3 1 4294967295]
      assertBool "invalid dimensions" (either (const True) (const False) (parseEither parseScalar (scalarToValue (CellValue 4 1 1))))
      assertBool "invalid seed" (either (const True) (const False) (parseEither parseScalar (scalarToValue (CellValue 3 1 (-1)))))
  , testCase "SDF scalar primitives share exact geometry distances" $ do
      let fields=[(SdfSphere (0.5,0.5,0.5) 0.3,G.Sphere (0.5,0.5,0.5) 0.3),(SdfBox (0,0,0) (1,2,3),G.Box (0,0,0) (1,2,3)),(SdfCylinder (0,0,0) 1 2,G.Cylinder (0,0,0) 1 2),(SdfTorus (0,0,0) 1 0.2,G.Torus (0,0,0) 1 0.2),(SdfPlane (0,0,0) 0.2,G.Plane (0,0,0) 0.2)]
      mapM_ (\(f,g) -> mapM_ (\p -> assertEqual "shared distance" (G.distance g p) (scalarField f p)) [(0,0,0),(1,2,3),(-1,0.2,0.4)]) fields
      mapM_ (\(f,_) -> assertEqual "round trip" (Right f) (parseEither parseScalar (scalarToValue f))) fields
  , testCase "Hard and smooth SDF combinations preserve signs and degenerate smoothing" $ do
      let f op k=SdfCombine op k (Constant (-0.2)) (Constant (-0.2))
      close "hard union" (-0.2) (scalarField (f SdfUnion 0) (0,0,0))
      close "smooth union" (-0.3) (scalarField (f SdfUnion 0.4) (0,0,0))
      close "smooth intersection" (-0.1) (scalarField (f SdfIntersection 0.4) (0,0,0))
      close "difference" 0.2 (scalarField (f SdfDifference 0) (0,0,0))
      close "negative radius is hard" (-0.2) (scalarField (f SdfUnion (-1)) (0,0,0))
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
