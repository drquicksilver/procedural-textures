-- | Blending colours in the OKLab colour space (Björn Ottosson, 2020), so
-- gradients change evenly in perceived lightness, hue and colourfulness,
-- without the muddy or dark middles of blending sRGB values directly.
--
-- Blending follows CSS Color 4's interpolation "in oklab": colours are
-- converted from sRGB to linear light to OKLab, premultiplied by alpha,
-- interpolated, then converted back and clamped to the sRGB gamut. With
-- premultiplication, fading a colour to transparent keeps its colour.
module OkLab
  ( Lab (..)
  , toLab
  , fromLab
  , mixLab
  ) where

import Colours (Colour)

-- | A colour in OKLab with straight (not premultiplied) alpha.
data Lab = Lab !Double !Double !Double !Double
  deriving (Eq, Show)

toLab :: Colour -> Lab
toLab (r, g, b, a) =
  let lr = toLinear r
      lg = toLinear g
      lb = toLinear b
      l = cbrt (0.4122214708 * lr + 0.5363325363 * lg + 0.0514459929 * lb)
      m = cbrt (0.2119034982 * lr + 0.6806995451 * lg + 0.1073969566 * lb)
      s = cbrt (0.0883024619 * lr + 0.2817188376 * lg + 0.6299787005 * lb)
  in Lab
       (0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s)
       (1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s)
       (0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s)
       a

fromLab :: Lab -> Colour
fromLab (Lab bigL aa bb alpha) =
  let l = cube (bigL + 0.3963377774 * aa + 0.2158037573 * bb)
      m = cube (bigL - 0.1055613458 * aa - 0.0638541728 * bb)
      s = cube (bigL - 0.0894841775 * aa - 1.2914855480 * bb)
      r = 4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s
      g = -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s
      b = -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s
  in (unit (toSrgb r), unit (toSrgb g), unit (toSrgb b), unit alpha)

-- | Interpolate a fraction @t@ of the way from one colour to another,
-- premultiplied by alpha. When both are fully transparent there is no
-- colour to weight by, so the colours are blended directly.
mixLab :: Double -> Lab -> Lab -> Lab
mixLab t (Lab l1 a1 b1 alpha1) (Lab l2 a2 b2 alpha2)
  | alpha <= 0.0 = Lab (lerp l1 l2) (lerp a1 a2) (lerp b1 b2) 0.0
  | otherwise =
      Lab
        (premultiplied l1 l2 / alpha)
        (premultiplied a1 a2 / alpha)
        (premultiplied b1 b2 / alpha)
        alpha
  where
    alpha = lerp alpha1 alpha2
    lerp x y = x + (y - x) * t
    premultiplied x y = lerp (x * alpha1) (y * alpha2)

-- | The sRGB transfer function, from encoded values to linear light.
toLinear :: Double -> Double
toLinear c
  | c <= 0.04045 = c / 12.92
  | otherwise = ((c + 0.055) / 1.055) ** 2.4

toSrgb :: Double -> Double
toSrgb l
  | l <= 0.0031308 = 12.92 * l
  | otherwise = 1.055 * l ** (1.0 / 2.4) - 0.055

cbrt :: Double -> Double
cbrt x = signum x * abs x ** (1.0 / 3.0)

cube :: Double -> Double
cube x = x * x * x

unit :: Double -> Double
unit = max 0.0 . min 1.0
