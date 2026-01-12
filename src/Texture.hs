module Texture
  ( Texture(..)
  , textureToImageFn
  ) where

import ColourRamps (ColourRamp, evalRamp)
import Colours (Colour)
import Perlin (perlin2)
import Render (ImageFn)

data Texture
  = Flat Colour
  | Linear (Double, Double) (Double, Double) ColourRamp
  | Radial (Double, Double) ColourRamp
  | Circular (Double, Double) Double ColourRamp
  | Perlin (Double, Double) ColourRamp
  | Turbulence Double Int Double Double Texture
  | Tiled Int Int Texture Texture
  | Layer Texture Texture
  deriving (Eq, Show)

textureToImageFn :: Texture -> ImageFn
textureToImageFn texture =
  case texture of
    Linear (x0, y0) (x1, y1) ramp ->
      \x y ->
        let dx = x1 - x0
            dy = y1 - y0
            len2 = dx * dx + dy * dy
            t =
              if len2 <= 0.0
                then 0.0
                else ((x - x0) * dx + (y - y0) * dy) / len2
        in evalRamp ramp t
    Flat colour ->
      \_ _ -> colour
    Radial (cx, cy) ramp ->
      \x y ->
        let dx = x - cx
            dy = y - cy
            len = sqrt (dx * dx + dy * dy)
            t =
              if len <= 0.0
                then 0.5
                else
                  let northDot = (-dy) / len
                  in (1.0 - northDot) / 2.0
        in evalRamp ramp t
    Circular (cx, cy) radius ramp ->
      \x y ->
        let dx = x - cx
            dy = y - cy
            dist = sqrt (dx * dx + dy * dy)
            t =
              if radius <= 0.0
                then 0.0
                else dist / radius
        in evalRamp ramp t
    Perlin (sx, sy) ramp ->
      \x y ->
        evalRamp ramp (perlin2 (x * sx) (y * sy))
    Turbulence amount octaves omega lambda base ->
      let baseFn = textureToImageFn base
      in \x y ->
          let dx = amount * (turbulenceValue octaves omega lambda x y - 0.5)
              dy =
                amount
                  * (turbulenceValue octaves omega lambda (x + 19.1) (y + 7.7) - 0.5)
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

turbulenceValue :: Int -> Double -> Double -> Double -> Double -> Double
turbulenceValue octaves omega lambda x y =
  let safeOctaves = max 1 octaves
      go n amp freq acc
        | n <= 0 = acc
        | otherwise =
            let noise = perlin2 (x * freq) (y * freq)
                centered = abs (2.0 * noise - 1.0)
            in go (n - 1) (amp * omega) (freq * lambda) (acc + amp * centered)
      total = go safeOctaves 1.0 1.0 0.0
      norm =
        if omega == 1.0
          then fromIntegral safeOctaves
          else (1.0 - omega ** fromIntegral safeOctaves) / (1.0 - omega)
  in if norm <= 0.0 then 0.0 else total / norm
