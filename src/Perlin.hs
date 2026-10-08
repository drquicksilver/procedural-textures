module Perlin
  ( perlin3Periodic
  , perlin3
  , perlin2
  ) where

import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (UArray, listArray)
import Data.Bits ((.&.))

-- | 2D gradient noise in [0, 1], 0.5 at every integer lattice point.
--
-- Each lattice point takes one of 16 unit gradients, evenly spaced and
-- turned half a step (11.25 degrees) off the axes. The classic 8 gradients
-- (axes and diagonals) line the noise's features up with the grid, which
-- shows as horizontal and vertical runs and right-angled turns, especially
-- in ridged and billowy noise.
perlin2 :: Double -> Double -> Double
perlin2 x y =
  let fx = floor x :: Int
      fy = floor y :: Int
      xi = fx .&. 255
      yi = fy .&. 255
      xf = x - fromIntegral fx
      yf = y - fromIntegral fy
      u = fade xf
      v = fade yf
      aa = permAt (permAt xi + yi)
      ab = permAt (permAt xi + yi + 1)
      ba = permAt (permAt (xi + 1) + yi)
      bb = permAt (permAt (xi + 1) + yi + 1)
      x1 = lerp u (grad aa xf yf) (grad ba (xf - 1.0) yf)
      x2 = lerp u (grad ab xf (yf - 1.0)) (grad bb (xf - 1.0) (yf - 1.0))
      -- With unit gradients the raw value lies within +/- sqrt 2 / 2.
      value = lerp v x1 x2 * sqrt2
  in (value + 1.0) / 2.0

-- | Improved Perlin interpolation in 3D, with 32 rotated unit gradients.
-- The larger direction set reduces lattice bias in axis-aligned slices.
perlin3 :: Double -> Double -> Double -> Double
perlin3 x y z =
  let fx = floor x :: Int
      fy = floor y :: Int
      fz = floor z :: Int
      xi = fx .&. 255
      yi = fy .&. 255
      zi = fz .&. 255
      a = x - fromIntegral fx
      b = y - fromIntegral fy
      c = z - fromIntegral fz
      u = fade a
      v = fade b
      w = fade c
      corner dx dy dz = grad3 (permAt (permAt (permAt (xi+dx)+yi+dy)+zi+dz))
                             (a-fromIntegral dx) (b-fromIntegral dy) (c-fromIntegral dz)
      plane dz = lerp v (lerp u (corner 0 0 dz) (corner 1 0 dz))
                       (lerp u (corner 0 1 dz) (corner 1 1 dz))
  in max 0 (min 1 (0.5 + 0.8 * lerp w (plane 0) (plane 1)))

-- | Explicit periodic lattice hashing; all eight corners wrap on each axis.
perlin3Periodic :: (Int,Int,Int) -> Double -> Double -> Double -> Double
perlin3Periodic (px,py,pz) x y z =
  let fx=floor x :: Int; fy=floor y :: Int; fz=floor z :: Int
      a=x-fromIntegral fx; b=y-fromIntegral fy; c=z-fromIntegral fz
      u=fade a; v=fade b; w=fade c
      ix d=(fx+d) `mod` max 1 px .&. 255
      iy d=(fy+d) `mod` max 1 py .&. 255
      iz d=(fz+d) `mod` max 1 pz .&. 255
      corner dx dy dz=grad3 (permAt (permAt (permAt (ix dx)+iy dy)+iz dz))
        (a-fromIntegral dx) (b-fromIntegral dy) (c-fromIntegral dz)
      plane dz=lerp v (lerp u (corner 0 0 dz) (corner 1 0 dz))
        (lerp u (corner 0 1 dz) (corner 1 1 dz))
  in max 0 (min 1 (0.5+0.8*lerp w (plane 0) (plane 1)))

{-# INLINE grad3 #-}
grad3 :: Int -> Double -> Double -> Double -> Double
grad3 hash x y z =
  let i = 3 * (hash .&. 31)
  in gradients3 `unsafeAt` i * x + gradients3 `unsafeAt` (i+1) * y + gradients3 `unsafeAt` (i+2) * z

gradients3 :: UArray Int Double
gradients3 = listArray (0,95) (concat [rotated k | k <- [0..31 :: Int]])
  where
    rotated k =
      let z = 1 - 2 * (fromIntegral k + 0.5) / 32
          r = sqrt (1-z*z)
          a = fromIntegral k * pi * (3-sqrt 5) + 0.37
          x = r*cos a
          y = r*sin a
          -- Fixed non-axis rotation of the spherical Fibonacci directions.
          u = x*cos 0.41-z*sin 0.41
          v = x*sin 0.41+z*cos 0.41
      in [u, y*cos 0.29-v*sin 0.29, y*sin 0.29+v*cos 0.29]

sqrt2 :: Double
sqrt2 = 1.4142135623730951

-- | Look up the doubled permutation table. In 'perlin2' the largest index is
-- 255 + 255 + 1 = 511, so lookups never leave the table.
permAt :: Int -> Int
permAt idx =
  permArray `unsafeAt` idx

fade :: Double -> Double
fade t =
  t * t * t * (t * (t * 6.0 - 15.0) + 10.0)

lerp :: Double -> Double -> Double -> Double
lerp t a b =
  a + t * (b - a)

grad :: Int -> Double -> Double -> Double
grad hash x y =
  let i = 2 * (hash .&. 15)
  in gradients `unsafeAt` i * x + gradients `unsafeAt` (i + 1) * y

-- | The 16 unit gradients, as consecutive (x, y) pairs.
gradients :: UArray Int Double
gradients =
  listArray (0, 31) (concat [[cos angle, sin angle] | k <- [0 .. 15 :: Int], let angle = (fromIntegral k + 0.5) * pi / 8])

permArray :: UArray Int Int
permArray =
  listArray (0, 511) (basePerm ++ basePerm)

basePerm :: [Int]
basePerm =
  [ 151, 160, 137, 91, 90, 15, 131, 13
  , 201, 95, 96, 53, 194, 233, 7, 225
  , 140, 36, 103, 30, 69, 142, 8, 99
  , 37, 240, 21, 10, 23, 190, 6, 148
  , 247, 120, 234, 75, 0, 26, 197, 62
  , 94, 252, 219, 203, 117, 35, 11, 32
  , 57, 177, 33, 88, 237, 149, 56, 87
  , 174, 20, 125, 136, 171, 168, 68, 175
  , 74, 165, 71, 134, 139, 48, 27, 166
  , 77, 146, 158, 231, 83, 111, 229, 122
  , 60, 211, 133, 230, 220, 105, 92, 41
  , 55, 46, 245, 40, 244, 102, 143, 54
  , 65, 25, 63, 161, 1, 216, 80, 73
  , 209, 76, 132, 187, 208, 89, 18, 169
  , 200, 196, 135, 130, 116, 188, 159, 86
  , 164, 100, 109, 198, 173, 186, 3, 64
  , 52, 217, 226, 250, 124, 123, 5, 202
  , 38, 147, 118, 126, 255, 82, 85, 212
  , 207, 206, 59, 227, 47, 16, 58, 17
  , 182, 189, 28, 42, 223, 183, 170, 213
  , 119, 248, 152, 2, 44, 154, 163, 70
  , 221, 153, 101, 155, 167, 43, 172, 9
  , 129, 22, 39, 253, 19, 98, 108, 110
  , 79, 113, 224, 232, 178, 185, 112, 104
  , 218, 246, 97, 228, 251, 34, 242, 193
  , 238, 210, 144, 12, 191, 179, 162, 241
  , 81, 51, 145, 235, 249, 14, 239, 107
  , 49, 192, 214, 31, 181, 199, 106, 157
  , 184, 84, 204, 176, 115, 121, 50, 45
  , 127, 4, 150, 254, 138, 236, 205, 93
  , 222, 114, 67, 29, 24, 72, 243, 141
  , 128, 195, 78, 66, 215, 61, 156, 180
  ]
