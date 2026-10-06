module Texture
  ( Texture(..)
  , NoiseStyle(..)
  , textureToImageFn
  , textureToField
  , fbm3Fn
  , fbmFn
  ) where

import Data.List (nub)
import ColourRamps (ColourRamp, RampMode, compileRamp)
import Colours (Colour)
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (UArray, listArray)
import Perlin (perlin3)
import Vector3 (Vec3, sub, dot, mul, norm, normalise)
import Render (ImageFn)

data Texture
  = Flat Colour
  | Linear Vec3 Vec3 RampMode ColourRamp
  | Radial Vec3 Vec3 RampMode ColourRamp
  | Circular Vec3 Double RampMode ColourRamp
  | Perlin Vec3 RampMode ColourRamp
  | Fbm Vec3 Int Double Double NoiseStyle RampMode ColourRamp
  -- ^ Scale, octaves, persistence, lacunarity, style, and the ramp's mode and ramp.
  | Turbulence Double Int Double Double Texture
  | Tiled Int Int Int Texture Texture
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
  let field = textureToField texture
  in \x y -> field x y 0

textureToField :: Texture -> Double -> Double -> Double -> Colour
textureToField texture =
  case texture of
    Linear from to mode ramp ->
      let rampFn = compileRamp mode ramp
          direction = sub to from
          len2 = dot direction direction
      in \x y z -> rampFn (if len2 <= 0 then 0 else dot (sub (x,y,z) from) direction / len2)
    Flat colour ->
      \_ _ _ -> colour
    Radial centre axis mode ramp ->
      let rampFn = compileRamp mode ramp
          unit = normalise axis
          project v = sub v (mul (dot v unit) unit)
          northCandidate = project (0,-1,0)
          north = normalise (if norm northCandidate < 1e-9 then project (0,0,1) else northCandidate)
      in \x y z ->
          let radial = project (sub (x,y,z) centre)
              len = norm radial
          in rampFn (if len <= 0 then 0.5 else (1-dot north radial / len)/2)
    Circular centre radius mode ramp ->
      let rampFn = compileRamp mode ramp
      in \x y z -> rampFn (if radius <= 0 then 0 else norm (sub (x,y,z) centre) / radius)
    Perlin (sx,sy,sz) mode ramp ->
      let rampFn = compileRamp mode ramp
      in \x y z -> rampFn (perlin3 (x*sx) (y*sy) (z*sz))
    Fbm scale octaves persistence lacunarity style mode ramp ->
      let rampFn = compileRamp mode ramp
          noise = fbm3Fn scale octaves persistence lacunarity style
      in \x y z -> rampFn (noise x y z)
    Turbulence amount octaves omega lambda base ->
      let baseFn = textureToField base
          turbulence = turbulenceFn octaves omega lambda
      in \x y z ->
          let dx = amount * (turbulence x y z - 0.5)
              dy = amount * (turbulence (x + 19.1) (y + 7.7) (z + 3.3) - 0.5)
              dz = amount * (turbulence (x + 5.2) (y + 13.8) (z + 29.6) - 0.5)
          in baseFn (x + dx) (y + dy) (z + dz)
    Tiled columns rows depth a b ->
      let aFn = textureToField a
          bFn = textureToField b
          safeColumns = max 1 columns
          safeRows = max 1 rows
          safeDepth = max 1 depth
      in \x y z ->
          let xi = floor (x * fromIntegral safeColumns) :: Int
              yi = floor (y * fromIntegral safeRows) :: Int
              zi = floor (z * fromIntegral safeDepth) :: Int
          in if (xi + yi + zi) `mod` 2 == 0
               then aFn x y z
               else bFn x y z
    Layer top bottom ->
      case sharedLayerFn texture of
        Just fn -> fn
        Nothing ->
          let topFn = textureToField top
              bottomFn = textureToField bottom
          in \x y z -> blend (topFn x y z) (bottomFn x y z)


-- Reuse raw displacement values only within one layer domain. Stop collecting
-- at a warp: its child receives different coordinates and forms a new domain.
type WarpKey = (Int, Double, Double)

sharedLayerFn :: Texture -> Maybe (Double -> Double -> Double -> Colour)
sharedLayerFn texture
  | null repeated = Nothing
  | otherwise =
      let fields = [(key, turbulenceFn octaves omega lambda) | key@(octaves, omega, lambda) <- repeated]
          fn = compileShared repeated texture
      in Just $ \x y z ->
          let samples = [(key, (field x y z - 0.5, field (x + 19.1) (y + 7.7) (z + 3.3) - 0.5, field (x + 5.2) (y + 13.8) (z + 29.6) - 0.5)) | (key, field) <- fields]
          in fn x y z samples
  where
    keys = layerWarpKeys texture
    repeated = [key | key <- nub keys, length (filter (== key) keys) > 1]

layerWarpKeys :: Texture -> [WarpKey]
layerWarpKeys (Layer top bottom) = layerWarpKeys top <> layerWarpKeys bottom
layerWarpKeys (Turbulence _ octaves omega lambda _) = [(octaves, omega, lambda)]
layerWarpKeys _ = []

compileShared :: [WarpKey] -> Texture -> Double -> Double -> Double -> [(WarpKey, Vec3)] -> Colour
compileShared keys texture =
  case texture of
    Layer top bottom ->
      let topFn = compileShared keys top
          bottomFn = compileShared keys bottom
      in \x y z samples -> blend (topFn x y z samples) (bottomFn x y z samples)
    Turbulence amount octaves omega lambda base | (octaves, omega, lambda) `elem` keys ->
      let baseFn = textureToField base
          fallback = textureToField texture
      in \x y z samples ->
          case lookup (octaves, omega, lambda) samples of
            Just (dx, dy, dz) -> baseFn (x + amount * dx) (y + amount * dy) (z + amount * dz)
            Nothing -> fallback x y z
    _ -> let fn = textureToField texture in \x y z _ -> fn x y z

blend :: Colour -> Colour -> Colour
blend top@(_, _, _, a1) bottom
  | a1 == 1.0 = top
  | otherwise = blendGeneral top bottom

blendGeneral :: Colour -> Colour -> Colour
blendGeneral (r1, g1, b1, a1) (r2, g2, b2, a2) =
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
fbmFn (sx,sy) octaves persistence lacunarity style =
  let f = fbm3Fn (sx,sy,sqrt (abs (sx*sy))) octaves persistence lacunarity style
  in \x y -> f x y 0

fbm3Fn :: Vec3 -> Int -> Double -> Double -> NoiseStyle -> Double -> Double -> Double -> Double
fbm3Fn (sx, sy, sz) octaves persistence lacunarity style =
  let safeOctaves = max 1 octaves
      weights = take safeOctaves (iterate (* persistence) 1.0)
      total = sum weights
      shape n =
        case style of
          Smooth -> n
          Billowy -> abs (2.0 * n - 1.0)
          Ridged -> let r = 1.0 - abs (2.0 * n - 1.0) in r * r
      octaves' = octaveTransforms safeOctaves lacunarity
  in \x y z ->
      let sxx = x * sx
          syy = y * sy
          szz = z * sz
          go :: Int -> Double -> Double -> Double
          go i amp acc
            | i >= safeOctaves = acc
            | otherwise =
                let offset = fromIntegral i
                    (rx, ry, rz) = transformOctave octaves' i sxx syy szz
                    n = perlin3 (rx + 31.7 * offset) (ry + 17.3 * offset) (rz + 11.9 * offset)
                in go (i + 1) (amp * persistence) (acc + amp * shape n)
          value = if total <= 0.0 then 0.5 else go 0 1.0 0.0 / total
      in clamp01 (spread style value)

-- | Explicit artistic contrast mappings for material ramps. These are not
-- probability normalisations; the raw 3D distribution is recorded by
-- bench/NoiseStudy.hs and the Phase 2 decision log.
spread :: NoiseStyle -> Double -> Double
spread style value =
  case style of
    Smooth -> 0.5 + (value - 0.5) * 2.0
    Billowy -> value * 1.75
    Ridged -> (value - 0.2) / 0.72

-- | Each octave's frequency and rotation as a 3x3 matrix, stored as
-- consecutive columns scaled by @lacunarity^i@: octave @i@ samples
-- the noise @lacunarity^i@ times finer, turned by @i@ times 'octaveRotation'
-- so the lattices of successive octaves don't line up with each other.
-- Computed once per texture node rather than per pixel.
octaveTransforms :: Int -> Double -> UArray Int Double
octaveTransforms octaves lacunarity =
  listArray (0, 9 * octaves - 1) (concatMap matrix [0..octaves-1])
  where
    matrix i =
      let a = fromIntegral i * octaveRotation
          f = lacunarity ^ i
          rotate x y z =
            let u = x*cos a-y*sin a
                v = x*sin a+y*cos a
                w = v*cos (a*0.71)-z*sin (a*0.71)
                q = v*sin (a*0.71)+z*cos (a*0.71)
            in [f*(u*cos (a*0.53)+q*sin (a*0.53)), f*w, f*(q*cos (a*0.53)-u*sin (a*0.53))]
      in rotate 1 0 0 <> rotate 0 1 0 <> rotate 0 0 1

{-# INLINE transformOctave #-}
transformOctave :: UArray Int Double -> Int -> Double -> Double -> Double -> Vec3
transformOctave transforms i x y z =
  let at n = transforms `unsafeAt` (9*i+n)
  in (x*at 0+y*at 3+z*at 6, x*at 1+y*at 4+z*at 7, x*at 2+y*at 5+z*at 8)

-- | Radians between successive octaves (about 47.6 degrees).
octaveRotation :: Double
octaveRotation = 0.83

clamp01 :: Double -> Double
clamp01 v = max 0.0 (min 1.0 v)

-- | Sum of octaves of absolute centred noise, normalised to [0, 1]. The
-- normalisation depends only on the parameters, so it is computed once.
turbulenceFn :: Int -> Double -> Double -> Double -> Double -> Double -> Double
turbulenceFn octaves omega lambda =
  let safeOctaves = max 1 octaves
      amplitudeSum =
        if omega == 1.0
          then fromIntegral safeOctaves
          else (1.0 - omega ** fromIntegral safeOctaves) / (1.0 - omega)
      octaves' = octaveTransforms safeOctaves lambda
  in \x y z ->
      let go :: Int -> Double -> Double -> Double
          go n amp acc
            | n <= 0 = acc
            | otherwise =
                let (rx, ry, rz) = transformOctave octaves' (safeOctaves - n) x y z
                    noise = perlin3 rx ry rz
                    centered = abs (2.0 * noise - 1.0)
                in go (n - 1) (amp * omega) (acc + amp * centered)
          total = go safeOctaves 1.0 0.0
      in if amplitudeSum <= 0.0 then 0.0 else total / amplitudeSum
