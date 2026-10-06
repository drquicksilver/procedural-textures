-- | Shared object-space vector operations for fields, geometry and cameras.
module Vector3 (Vec3, add, sub, mul, dot, cross, norm, normalise) where

type Vec3 = (Double, Double, Double)

add, sub, cross :: Vec3 -> Vec3 -> Vec3
add (x,y,z) (a,b,c) = (x+a,y+b,z+c)
sub (x,y,z) (a,b,c) = (x-a,y-b,z-c)
cross (x,y,z) (a,b,c) = (y*c-z*b,z*a-x*c,x*b-y*a)

mul :: Double -> Vec3 -> Vec3
mul k (x,y,z) = (k*x,k*y,k*z)

dot :: Vec3 -> Vec3 -> Double
dot (x,y,z) (a,b,c) = x*a+y*b+z*c

norm :: Vec3 -> Double
norm v = sqrt (dot v v)

normalise :: Vec3 -> Vec3
normalise v = if norm v < 1e-12 then (0,0,1) else mul (1 / norm v) v
