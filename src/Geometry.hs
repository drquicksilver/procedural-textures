-- | Exact signed distances and boolean solids, shared with future field nodes.
module Geometry (SDF(..), distance, Shape(..), shapes, shapeName, shapeSolid) where

import Vector3

data SDF
  = Sphere Vec3 Double
  | Box Vec3 Vec3
  | Cylinder Vec3 Double Double
  | Torus Vec3 Double Double
  | Plane Vec3 Double
  | Union SDF SDF
  | Intersection SDF SDF
  | Difference SDF SDF
  deriving (Eq, Show)

distance :: SDF -> Vec3 -> Double
distance solid p = case solid of
  Sphere c radius -> norm (sub p c)-radius
  Box c (hx,hy,hz) ->
    let (x,y,z) = sub p c
        q = (abs x-hx, abs y-hy, abs z-hz)
        (a,b,d) = q
    in norm (max 0 a,max 0 b,max 0 d) + min 0 (max a (max b d))
  Cylinder c radius halfHeight ->
    let (x,y,z) = sub p c
        a = sqrt (x*x+z*z)-radius
        b = abs y-halfHeight
    in sqrt (max 0 a ^ (2::Int)+max 0 b ^ (2::Int)) + min 0 (max a b)
  Torus c major minor ->
    let (x,y,z) = sub p c
        q = sqrt (x*x+z*z)-major
    in sqrt (q*q+y*y)-minor
  Plane n offset -> dot (normalise n) p-offset
  Union a b -> min (distance a p) (distance b p)
  Intersection a b -> max (distance a p) (distance b p)
  Difference a b -> max (distance a p) (negate (distance b p))

data Shape = Ball | Cube | Tube | Ring | BittenCube | CutSphere | CutCube
  deriving (Eq, Show, Enum, Bounded)

shapes :: [Shape]
shapes = [minBound..maxBound]

shapeName :: Shape -> String
shapeName s = case s of
  Ball -> "sphere"
  Cube -> "cube"
  Tube -> "cylinder"
  Ring -> "torus"
  BittenCube -> "bitten-cube"
  CutSphere -> "cut-sphere"
  CutCube -> "cut-cube"

shapeSolid :: Shape -> SDF
shapeSolid s = case s of
  Ball -> Sphere centre 0.43
  Cube -> cube
  Tube -> Cylinder centre 0.35 0.4
  Ring -> Torus centre 0.29 0.13
  BittenCube -> Difference cube (Sphere (0.83,0.18,0.08) 0.43)
  CutSphere -> Difference (Sphere centre 0.43) (Box (1,0,0) (0.5,0.5,0.5))
  CutCube -> Intersection cube (Plane (0.7,-0.2,-1) 0.05)
  where
    centre = (0.5,0.5,0.5)
    cube = Box centre (0.38,0.38,0.38)
