module SceneSpec (sceneTests) where

import Data.List (find)
import Examples (Example(..))
import Geometry
import GoldenSpec (goldenViewTest)
import RampLibrary (RampLibrary)
import Resolve (resolveDocument)
import Scene
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, assertFailure)
import Vector3

sceneTests :: RampLibrary -> [Example] -> TestTree
sceneTests library examples = testGroup "Scenes"
  [ testCase "Primitive distances have correct signs and analytic values" $ do
      assertEqual "sphere" (-1) (distance (Sphere (0,0,0) 1) (0,0,0))
      assertEqual "box surface" 0 (distance (Box (0,0,0) (1,1,1)) (1,0,0))
      assertBool "box exterior" (abs (distance (Box (0,0,0) (1,1,1)) (2,2,2)-sqrt 3) < 1e-12)
      assertEqual "cylinder cap" 0 (distance (Cylinder (0,0,0) 1 2) (0,2,0))
      assertEqual "torus hole" 1.5 (distance (Torus (0,0,0) 2 0.5) (0,0,0))
  , testCase "Boolean subtraction exposes interior surfaces" $ do
      assertBool "bite is outside" (distance (shapeSolid BittenCube) (0.8,0.2,0.2) > 0)
      assertBool "remaining cube" (distance (shapeSolid BittenCube) (0.2,0.8,0.8) < 0)
      assertBool "octant is outside" (distance (shapeSolid CutSphere) (0.6,0.4,0.4) > 0)
      assertBool "remaining sphere" (distance (shapeSolid CutSphere) (0.4,0.6,0.6) < 0)
      let a = Sphere (0,0,0) 1
          b = Box (0,0,0) (0.5,0.5,0.5)
      assertEqual "union" (-1) (distance (Union a b) (0,0,0))
      assertEqual "intersection" (-0.5) (distance (Intersection a b) (0,0,0))
  , testCase "Tracing finds analytic sphere entry and rejects misses" $ do
      let solid = shapeSolid Ball
      case traceRay solid (0.5,0.5,-2) (0,0,1) of
        Nothing -> assertFailure "missing centre hit"
        Just (_,_,z) -> assertBool "entry" (abs (z-0.07) < 0.001)
      assertEqual "miss" Nothing (traceRay solid (2,2,-2) (0,0,1))
  , testCase "Normals are outward on external and subtraction walls" $ do
      assertBool "sphere normal" (dot (normalAt (shapeSolid Ball) (0.5,0.5,0.07)) (0,0,-1) > 0.999)
      assertBool "octant wall normal" (dot (normalAt (shapeSolid CutSphere) (0.5,0.3,0.3)) (1,0,0) > 0.999)
  , testCase "Orbit centre rays aim at the same object-space centre" $ do
      mapM_ (\camera -> do
        let (origin,ray) = cameraRay camera 0.5 0.5
        assertBool "aim" (norm (cross (sub (0.5,0.5,0.5) origin) ray) < 1e-12))
        [defaultCamera,Camera 1.2 (-0.6) 3,Camera (-2) 0.8 1.5]
  , testCase "Slice planes map to the documented object coordinates" $ do
      assertEqual "xy" (0.2,0.3,0.7) (slicePoint XY 0.7 0.2 0.3)
      assertEqual "xz" (0.2,0.7,0.3) (slicePoint XZ 0.7 0.2 0.3)
      assertEqual "yz" (0.7,0.2,0.3) (slicePoint YZ 0.7 0.2 0.3)
  , testGroup "Golden scenes"
      [ case find ((== name) . exampleId) examples of
          Nothing -> testCase name (assertFailure "missing example")
          Just e -> either (\err -> testCase name (assertFailure err))
            (goldenViewTest (shapeName shape <> "-" <> name) (Scene shape defaultCamera))
            (resolveDocument library (exampleDocument e))
      | shape <- shapes, name <- ["checker","marble","malachite"]
      ]
  ]
