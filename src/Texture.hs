module Texture
  ( Texture(..)
  , textureToImageFn
  ) where

import ColourRamps (ColourRamp)
import Colours (Colour)
import Render (ImageFn)

data Texture
  = Flat Colour
  | Linear (Double, Double) (Double, Double) ColourRamp
  | Radial (Double, Double) ColourRamp
  | Circular (Double, Double) Double ColourRamp
  | Tiled Int Int Texture Texture
  | Layer Texture Texture

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
        in ramp t
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
        in ramp t
    Circular (cx, cy) radius ramp ->
      \x y ->
        let dx = x - cx
            dy = y - cy
            dist = sqrt (dx * dx + dy * dy)
            t =
              if radius <= 0.0
                then 0.0
                else dist / radius
        in ramp t
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
      weight =
        if a <= 0.0
          then 0.0
          else a1 / a
  in ( lerp weight r1 r2
     , lerp weight g1 g2
     , lerp weight b1 b2
     , a
     )

lerp :: Double -> Double -> Double -> Double
lerp t a b =
  a + (b - a) * t
