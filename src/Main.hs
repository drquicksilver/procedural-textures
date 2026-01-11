module Main (main) where

import ColourRamps (RampMode (Clamp, Mirror, Wrap), colourRamp, sinusoidalColourRamp, twoStopRamp)
import Colours (blue, green, lightGrey, pink, red, skyBlue, slateGrey, transparent, white)
import Render (writeImage)
import Texture (Texture (..), textureToImageFn)

main :: IO ()
main = do
  let width = 128
      height = 128
      textures =
        [ ("gradient.png", gradient)
        , ("rings.png", rings)
        , ("radial.png", radial)
        , ("layered.png", layered)
        , ("stripes.png", stripes)
        , ("clouds.png", clouds)
        , ("wobbly-stripes.png", wobblyStripes)
        , ("swirly-stripes.png", swirlyStripes)
        , ("marble.png", marble)
        , ("checker.png", checker)
        , ("redgreensaw.png", redGreenSaw)
        , ("redgreensine.png", redGreenSine)
        ]
  mapM_ (writeImage width height . toImageFn) textures
  where
    toImageFn (path, texture) = (path, textureToImageFn texture)

-- some basic examples using ImageFn directly
gradient :: Texture
gradient =
  Linear (0.0, 0.5) (1.0, 0.5) (twoStopRamp Clamp red blue)

rings :: Texture
rings =
  Circular (0.5, 0.5) 0.08 (twoStopRamp Mirror white blue)

radial :: Texture
radial =
  Radial (0.5, 0.5) (twoStopRamp Clamp red green)

layered :: Texture
layered =
  Layer
    (Linear (0.0, 0.5) (1.0, 0.5) (twoStopRamp Clamp transparent blue))
    (Linear (0.5, 0.0) (0.5, 1.0) (twoStopRamp Clamp red green))

stripes :: Texture
stripes =
  let ramp = colourRamp Wrap [(0.0, red), (0.5, pink), (0.5, white), (1.0, lightGrey)]
  in Linear (0.0, 0.5) (1.0 / 6.0, 0.5) ramp

clouds :: Texture
clouds =
  let ramp = colourRamp Clamp [(0.0, skyBlue), (0.6, white), (1.0, lightGrey)]
  in Perlin (4.0, 4.0) ramp

baseStripes :: Texture
baseStripes =
  let ramp =
        colourRamp Wrap
          [ (0.0, (0.15, 0.2, 0.6, 1.0))
          , (0.5, (0.15, 0.2, 0.6, 1.0))
          , (0.5, (0.85, 0.9, 1.0, 1.0))
          , (1.0, (0.85, 0.9, 1.0, 1.0))
          ]
  in Linear (0.0, 0.5) (1.0 / 4.0, 0.5) ramp

wobblyStripes :: Texture
wobblyStripes =
  Turbulence 0.08 3 0.5 2.0 baseStripes

swirlyStripes :: Texture
swirlyStripes =
  Turbulence 0.80 8 0.4 2.2 baseStripes

marble :: Texture
marble =
  let bandRamp =
        colourRamp Wrap
          [ (0.0, white)
          , (0.65, white)
          , (0.75, slateGrey)
          , (1.0, slateGrey)
          ]
      bands = Linear (0.0, 0.5) (1.0 / 4.0, 0.5) bandRamp
  in Turbulence 0.6 9 0.65 2.5 bands

checker :: Texture
checker =
  Tiled 8 8 (Flat (0.9, 0.9, 0.9, 1.0)) (Flat (0.1, 0.1, 0.1, 1.0))

redGreenSaw :: Texture
redGreenSaw =
  Linear (0.0, 0.5) (1.0 / 6.0, 0.5) (twoStopRamp Mirror red green)

redGreenSine :: Texture
redGreenSine =
  Linear (0.0, 0.5) (1.0 / 6.0, 0.5) (sinusoidalColourRamp red green)
