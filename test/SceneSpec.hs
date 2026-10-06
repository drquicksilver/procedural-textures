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
  , testCase "Profiles, lathes and extrusions have exact distances" $ do
      let square = Polygon [(0,0),(1,0),(1,1),(0,1)]
      assertBool "polygon inside" (abs (profileDistance square (0.5,0.25)+0.25) < 1e-12)
      assertBool "polygon outside" (abs (profileDistance square (2,0.5)-1) < 1e-12)
      assertBool "polygon corner" (abs (profileDistance square (2,2)-sqrt 2) < 1e-12)
      assertBool "rounded rect" (abs (profileDistance (Rect (0,0) (1,1) 0.5) (2,2)-(sqrt 4.5-0.5)) < 1e-12)
      mapM_ (\p -> assertBool "lathed disc is a torus"
          (abs (distance (Revolve (0,0,0) (Disc (2,0) 0.5)) p-distance (Torus (0,0,0) 2 0.5) p) < 1e-12))
        [(0,0,0),(2,0.3,0.1),(-1,2,3)]
      assertBool "lathe height is up" (distance (Revolve (0,0,0) (Disc (0,1) 0.5)) (0,-1,0) < 0)
      assertBool "extruded square is a box"
        (abs (distance (Extrude (0,0,0) 2 square) (0.5,-0.5,3)-1) < 1e-12)
      assertBool "turn" (distance (Turn (0,0,0) (pi/2) (Sphere (1,0,0) 0.1)) (0,0,1) < 0)
      assertEqual "radial repeat" (distance (Sphere (1,0,0) 0.1) (1,0,0))
        (distance (RadialRepeat (0,0,0) 4 (Sphere (1,0,0) 0.1)) (0,0,-1))
  , testCase "Chess bases: pawn smallest, rook knight bishop equal, queen and king largest" $ do
      -- The widest point of each piece along +x from its axis is its foot.
      let reach shape = maximum
            [ x-0.5
            | y <- [-0.2,-0.195..1.2], x <- [0.5,0.501..0.85], distance (shapeSolid shape) (x,y,0.5) < 0 ]
          near a b = abs (a-b) < 0.0025
      assertBool "pawn" (near (reach Pawn) (1.2*0.16))
      mapM_ (\s -> assertBool (show s) (near (reach s) (1.2*0.18))) [Rook,Knight,Bishop]
      mapM_ (\s -> assertBool (show s) (near (reach s) (1.2*0.2))) [Queen,King]
  , testCase "Chess piece bounds never overestimate the distance to the piece" $
      mapM_ (\shape -> case shapeSolid shape of
          Bounded bound solid -> assertBool (shapeName shape) (and
            [ distance bound p <= distance solid p+1e-9
            | x <- fine, y <- fine, z <- fine, let p = (x,y,z) ])
          _ -> assertFailure (shapeName shape <> " is not bounded"))
        [Pawn,Rook,Knight,Bishop,Queen,King]
  , testCase "Every shape fits inside the tracer's bounding sphere" $
      mapM_ (\shape -> assertBool (shapeName shape) (and
          [ norm (sub p (0.5,0.5,0.5)) < 0.75
          | x <- grid, y <- grid, z <- grid, let p = (x,y,z), distance (shapeSolid shape) p < 0 ]))
        shapes
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

grid :: [Double]
grid = [-0.3,-0.25..1.3]

fine :: [Double]
fine = [-0.15,-0.124..1.15]
