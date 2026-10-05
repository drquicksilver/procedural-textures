module Texture
  ( Texture(..)
  , NoiseStyle(..)
  , textureToImageFn
  , fbmFn
  ) where

import ColourRamps (ColourRamp, RampMode, compileRamp)
import Colours (Colour)
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (UArray, listArray)
import Perlin (perlin2)
import Render (ImageFn)

data Texture
  = Flat Colour
  | Linear (Double, Double) (Double, Double) RampMode ColourRamp
  | Radial (Double, Double) RampMode ColourRamp
  | Circular (Double, Double) Double RampMode ColourRamp
  | Perlin (Double, Double) RampMode ColourRamp
  | Fbm (Double, Double) Int Double Double NoiseStyle RampMode ColourRamp
  -- ^ Scale, octaves, persistence, lacunarity, style, and the ramp's mode and ramp.
  | Turbulence Double Int Double Double Texture
  | Tiled Int Int Texture Texture
  | Layer Texture Texture
  deriving (Eq, Show)

-- | How each octave of multi-octave noise is shaped before summing.
data NoiseStyle
  = Smooth
  -- ^ Plain noise: soft, rolling.
  | Billowy
  -- ^ Absolute value: puffy, with sharp creases at the low points.
  | Ridged
  -- ^ Inverted absolute value, squared: sharp ridges, like mountains or veins.
  deriving (Eq, Show)

textureToImageFn :: Texture -> ImageFn
textureToImageFn texture =
  case texture of
    Linear (x0, y0) (x1, y1) mode ramp ->
      let rampFn = compileRamp mode ramp
          dx = x1 - x0
          dy = y1 - y0
          len2 = dx * dx + dy * dy
      in \x y ->
          let t =
                if len2 <= 0.0
                  then 0.0
                  else ((x - x0) * dx + (y - y0) * dy) / len2
          in rampFn t
    Flat colour ->
      \_ _ -> colour
    Radial (cx, cy) mode ramp ->
      let rampFn = compileRamp mode ramp
      in \x y ->
        let dx = x - cx
            dy = y - cy
            len = sqrt (dx * dx + dy * dy)
            t =
              if len <= 0.0
                then 0.5
                else
                  let northDot = (-dy) / len
                  in (1.0 - northDot) / 2.0
        in rampFn t
    Circular (cx, cy) radius mode ramp ->
      let rampFn = compileRamp mode ramp
      in \x y ->
        let dx = x - cx
            dy = y - cy
            dist = sqrt (dx * dx + dy * dy)
            t =
              if radius <= 0.0
                then 0.0
                else dist / radius
        in rampFn t
    Perlin (sx, sy) mode ramp ->
      let rampFn = compileRamp mode ramp
      in \x y -> rampFn (perlin2 (x * sx) (y * sy))
    Fbm scale octaves persistence lacunarity style mode ramp ->
      let rampFn = compileRamp mode ramp
          noise = fbmFn scale octaves persistence lacunarity style
      in \x y -> rampFn (noise x y)
    Turbulence amount octaves omega lambda base ->
      let baseFn = textureToImageFn base
          turbulence = turbulenceFn octaves omega lambda
      in \x y ->
          let dx = amount * (turbulence x y - 0.5)
              dy = amount * (turbulence (x + 19.1) (y + 7.7) - 0.5)
          in baseFn (x + dx) (y + dy)
    Tiled columns rows a b ->
      let aFn = textureToImageFn a
          bFn = textureToImageFn b
          safeColumns = max 1 columns
          safeRows = max 1 rows
      in \x y ->
          let xi = floor (x * fromIntegral safeColumns) :: Int
              yi = floor (y * fromIntegral safeRows) :: Int
          in if (xi + yi) `mod` 2 == 0
               then aFn x y
               else bFn x y
    Layer top bottom ->
      let topFn = textureToImageFn top
          bottomFn = textureToImageFn bottom
      in \x y -> blend (topFn x y) (bottomFn x y)

blend :: Colour -> Colour -> Colour
blend (r1, g1, b1, a1) (r2, g2, b2, a2) =
  let a = a1 + a2 * (1.0 - a1)
      weightTop =
        if a <= 0.0
          then 0.0
          else a1 / a
      weightBottom = 1.0 - weightTop
  in ( lerp weightBottom r1 r2
     , lerp weightBottom g1 g2
     , lerp weightBottom b1 b2
     , a
     )

lerp :: Double -> Double -> Double -> Double
lerp t a b =
  a + (b - a) * t

-- | Multi-octave ("fractal Brownian motion") noise in [0, 1]. Octave @i@
-- samples Perlin noise at @lacunarity^i@ times the base frequency, weighted
-- by @persistence^i@, offset so that octaves do not line up at the noise
-- lattice points. Each style's sum is then stretched to use most of [0, 1]
-- (see 'spread') and clamped, so ramps designed for [0, 1] fit it.
fbmFn :: (Double, Double) -> Int -> Double -> Double -> NoiseStyle -> Double -> Double -> Double
fbmFn (sx, sy) octaves persistence lacunarity style =
  let safeOctaves = max 1 octaves
      weights = take safeOctaves (iterate (* persistence) 1.0)
      total = sum weights
      shape n =
        case style of
          Smooth -> n
          Billowy -> abs (2.0 * n - 1.0)
          Ridged -> let r = 1.0 - abs (2.0 * n - 1.0) in r * r
      octaves' = octaveTransforms safeOctaves lacunarity
  in \x y ->
      let sxx = x * sx
          syy = y * sy
          go :: Int -> Double -> Double -> Double
          go i amp acc
            | i >= safeOctaves = acc
            | otherwise =
                let offset = fromIntegral i
                    (rx, ry) = transformOctave octaves' i sxx syy
                    n = perlin2 (rx + 31.7 * offset) (ry + 17.3 * offset)
                in go (i + 1) (amp * persistence) (acc + amp * shape n)
          value = if total <= 0.0 then 0.5 else go 0 1.0 0.0 / total
      in clamp01 (spread style value)

-- | Stretch a style's raw sum over [0, 1]. Measured over many samples with
-- 3 to 6 octaves at persistence 0.5: the 1st and 99th percentiles land
-- between 0.03 and 0.18 and between 0.91 and 0.98, with at most about 1% of
-- values clamped.
spread :: NoiseStyle -> Double -> Double
spread style value =
  case style of
    Smooth -> 0.5 + (value - 0.5) * 2.0
    Billowy -> value * 1.75
    Ridged -> (value - 0.2) / 0.72

-- | Each octave's frequency and rotation as a 2x2 matrix, stored as
-- consecutive (cos, sin) pairs scaled by @lacunarity^i@: octave @i@ samples
-- the noise @lacunarity^i@ times finer, turned by @i@ times 'octaveRotation'
-- so the lattices of successive octaves don't line up with each other.
-- Computed once per texture node rather than per pixel.
octaveTransforms :: Int -> Double -> UArray Int Double
octaveTransforms octaves lacunarity =
  listArray
    (0, 2 * octaves - 1)
    (concat [[f * cos a, f * sin a] | i <- [0 .. octaves - 1], let a = fromIntegral i * octaveRotation, let f = lacunarity ^ i])

transformOctave :: UArray Int Double -> Int -> Double -> Double -> (Double, Double)
transformOctave transforms i x y =
  let c = transforms `unsafeAt` (2 * i)
      s = transforms `unsafeAt` (2 * i + 1)
  in (x * c - y * s, x * s + y * c)

-- | Radians between successive octaves (about 47.6 degrees).
octaveRotation :: Double
octaveRotation = 0.83

clamp01 :: Double -> Double
clamp01 v = max 0.0 (min 1.0 v)

-- | Sum of octaves of absolute centred noise, normalised to [0, 1]. The
-- normalisation depends only on the parameters, so it is computed once.
turbulenceFn :: Int -> Double -> Double -> Double -> Double -> Double
turbulenceFn octaves omega lambda =
  let safeOctaves = max 1 octaves
      norm =
        if omega == 1.0
          then fromIntegral safeOctaves
          else (1.0 - omega ** fromIntegral safeOctaves) / (1.0 - omega)
      octaves' = octaveTransforms safeOctaves lambda
  in \x y ->
      let go :: Int -> Double -> Double -> Double
          go n amp acc
            | n <= 0 = acc
            | otherwise =
                let (rx, ry) = transformOctave octaves' (safeOctaves - n) x y
                    noise = perlin2 rx ry
                    centered = abs (2.0 * noise - 1.0)
                in go (n - 1) (amp * omega) (acc + amp * centered)
          total = go safeOctaves 1.0 0.0
      in if norm <= 0.0 then 0.0 else total / norm
