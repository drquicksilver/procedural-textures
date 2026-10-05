module Examples
  ( examples
  ) where

import ColourRamps (RampMode (Clamp, Mirror, Wrap), colourRamp, sinusoidalColourRamp, twoStopRamp)
import Colours (black, blue, darkBlue, darkGrey, darkOrange, dimGrey, gold, green, indigo, lightGrey, pink, red, rgba, skyBlue, slateGrey, transparent, white)
import Texture (Texture (..))

examples :: [(FilePath, Texture)]
examples =
  [ ("gradient.png", gradient)
  , ("rings.png", rings)
  , ("radial.png", radial)
  , ("layered.png", layered)
  , ("sunset.png", sunset)
  , ("smiley.png", smiley)
  , ("stripes.png", stripes)
  , ("clouds.png", clouds)
  , ("wobbly-stripes.png", wobblyStripes)
  , ("swirly-stripes.png", swirlyStripes)
  , ("marble.png", marble)
  , ("checker.png", checker)
  , ("redgreensaw.png", redGreenSaw)
  , ("redgreensine.png", redGreenSine)
  ]

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

sunset :: Texture
sunset =
  let skyRamp =
        colourRamp Clamp
          [ (0.0, darkBlue)
          , (0.5, indigo)
          , (0.75, darkOrange)
          , (0.9, gold)
          , (1.0, rgba 255 232 200 255)
          ]
      sky = Linear (0.5, 0.0) (0.5, 1.0) skyRamp
      sunRamp =
        colourRamp Clamp
          [ (0.0, rgba 255 232 200 255)
          , (0.6, rgba 255 172 92 200)
          , (1.0, transparent)
          ]
      sun = Circular (0.5, 0.55) 0.22 sunRamp
      seaRamp =
        colourRamp Clamp
          [ (0.0, transparent)
          , (0.49, transparent)
          , (0.5, rgba 10 25 60 255)
          , (1.0, rgba 4 10 30 255)
          ]
      sea = Linear (0.5, 0.0) (0.5, 1.0) seaRamp
  in Layer sea (Layer sun sky)

-- sketch: use transparent ramps as clipping masks to build a smiley face
smiley :: Texture
smiley =
  let faceRamp =
        colourRamp Clamp
          [ (0.0, gold)
          , (0.7, gold)
          , (0.9, darkOrange)
          , (1.0, transparent)
          ]
      face = Circular (0.5, 0.5) 0.45 faceRamp
      eyeRamp =
        colourRamp Clamp
          [ (0.0, darkGrey)
          , (0.5, dimGrey)
          , (0.85, black)
          , (1.0, transparent)
          ]
      leftEye = Circular (0.35, 0.4) 0.065 eyeRamp
      rightEye = Circular (0.65, 0.4) 0.065 eyeRamp
      mouthRingRamp =
        colourRamp Clamp
          [ (0.0, transparent)
          , (0.6, transparent)
          , (0.66, rgba 120 30 140 255)
          , (0.84, rgba 90 15 110 255)
          , (1.0, transparent)
          ]
      mouthRing = Circular (0.5, 0.6) 0.25 mouthRingRamp
      mouthCoverRamp =
        colourRamp Clamp
          [ (0.0, gold)
          , (0.45, gold)
          , (0.46, transparent)
          , (1.0, transparent)
          ]
      mouthCover = Linear (0.5, 0.0) (0.5, 1.0) mouthCoverRamp
      mouth = Layer mouthCover mouthRing
      backgroundRamp =
        colourRamp Clamp
          [ (0.0, darkBlue)
          , (0.45, slateGrey)
          , (0.7, skyBlue)
          , (1.0, blue)
          ]
      background = Perlin (9.0, 9.0) backgroundRamp
  in Layer leftEye (Layer rightEye (Layer mouth (Layer face background)))

stripes :: Texture
stripes =
  let ramp = colourRamp Wrap [(0.0, red), (0.5, pink), (0.5, white), (1.0, lightGrey)]
  in Linear (0.0, 0.5) (1.0 / 6.0, 0.5) ramp

clouds :: Texture
clouds =
  let ramp = colourRamp Clamp [(0.0, skyBlue), (0.4, lightGrey), (1.0, white)]
      base = Perlin (7.0, 7.0) ramp
  in Turbulence 0.12 4 0.55 2.1 base

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
          [ (0.0, transparent)
          , (0.66, transparent)
          , (0.72, slateGrey)
          , (0.78, slateGrey)
          , (0.84, transparent)
          , (1.0, transparent)
          ]
      bands = Linear (0.0, 0.5) (1.0 / 4.0, 0.5) bandRamp
      veins = Turbulence 1.2 10 0.45 1.9 bands
      noiseRamp = colourRamp Clamp [(0.0, white), (0.9, lightGrey), (1.0, black)]
      noise = Perlin (40.0, 40.0) noiseRamp
  in Layer veins noise

checker :: Texture
checker =
  Tiled 8 8 (Flat (0.9, 0.9, 0.9, 1.0)) (Flat (0.1, 0.1, 0.1, 1.0))

redGreenSaw :: Texture
redGreenSaw =
  Linear (0.0, 0.5) (1.0 / 6.0, 0.5) (twoStopRamp Mirror red green)

redGreenSine :: Texture
redGreenSine =
  Linear (0.0, 0.5) (1.0 / 6.0, 0.5) (sinusoidalColourRamp red green)
