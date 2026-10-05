{-# LANGUAGE OverloadedStrings #-}

-- | A description of the texture language for the editor: which node types
-- exist, what their fields are, what kind of widget suits each field, sensible
-- ranges and defaults. The frontend builds its editing UI from this, so adding
-- a primitive is mostly a matter of extending the texture types, the JSON
-- encoding and this schema.
module Schema
  ( Schema (..)
  , Variant (..)
  , Field (..)
  , FieldKind (..)
  , Range (..)
  , Handle (..)
  , Guide (..)
  , schema
  , schemaToValue
  , defaultTexture
  ) where

import ColourRamps (ColourRamp (..), RampMode (..))
import Colours (Colour)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import Texture (NoiseStyle (..), Texture (..))
import TextureJson (currentVersion, rampToValue, textureToValue)

data Schema = Schema
  { textureVariants :: [Variant]
  , rampVariants :: [Variant]
  }

-- | One constructor of a texture or ramp: its JSON @"type"@ tag, a label and
-- description for people, its fields, and a complete default value.
data Variant = Variant
  { variantType :: Text
  , variantLabel :: Text
  , variantDescription :: Text
  , variantFields :: [Field]
  , variantGuides :: [Guide]
  , variantDefault :: Value
  }

data Field = Field
  { fieldKey :: Text
  , fieldLabel :: Text
  , fieldHelp :: Text
  , fieldKind :: FieldKind
  , fieldHandle :: Maybe Handle
  }

-- | Ranges are for sliders: numeric entry may go beyond them.
data Range = Range
  { rangeMin :: Double
  , rangeMax :: Double
  , rangeStep :: Double
  }

data FieldKind
  = ScalarField Range
  | IntField Int Int
  -- ^ Slider range for whole numbers.
  | PointField Range
  -- ^ A position in texture space, where the visible square is [0, 1]².
  | VectorField Range
  -- ^ A pair of numbers that is not a position, such as a scale.
  | ColourField
  | EnumField [(Text, Text)]
  -- ^ Allowed values and their labels.
  | StopsField
  | TextField
  | RampField
  | TextureField

-- | How a field can be manipulated directly on the preview.
data Handle
  = PointHandle
  | RadiusHandle Text
  -- ^ A distance from the point field with the given key.

-- | Decorations drawn on the preview while a node is selected.
data Guide = LineGuide Text Text
  -- ^ A line between two point fields.

schema :: Schema
schema =
  Schema
    { textureVariants =
        [ Variant "flat" "Flat" "A single colour everywhere." [colourField "colour" "Colour" ""] [] (textureToValue (Flat grey))
        , Variant
            "linear"
            "Linear gradient"
            "A ramp laid along the line from one point to another. Positions before the start and beyond the end follow the ramp's mode."
            [ pointField "from" "From" "Where the ramp starts (position 0)."
            , pointField "to" "To" "Where the ramp reaches position 1."
            , modeField
            , rampField
            ]
            [LineGuide "from" "to"]
            (textureToValue (Linear (0.0, 0.5) (1.0, 0.5) Clamp blackToWhite))
        , Variant
            "radial"
            "Radial sweep"
            "A ramp swept by angle around a centre, from position 0 straight up to position 1 straight down; the left and right halves mirror each other."
            [pointField "centre" "Centre" "", modeField, rampField]
            []
            (textureToValue (Radial (0.5, 0.5) Clamp blackToWhite))
        , Variant
            "circular"
            "Circular gradient"
            "A ramp laid outwards from a centre, reaching position 1 at the radius."
            [ pointField "centre" "Centre" ""
            , (scalarField "radius" "Radius" "Distance at which the ramp reaches position 1." (Range 0.0 1.0 0.005))
                { fieldHandle = Just (RadiusHandle "centre")
                }
            , modeField
            , rampField
            ]
            []
            (textureToValue (Circular (0.5, 0.5) 0.5 Clamp blackToWhite))
        , Variant
            "perlin"
            "Perlin noise"
            "Smooth gradient noise between 0 and 1, coloured by a ramp."
            [ Field "scale" "Scale" "Noise features per unit, horizontally and vertically." (VectorField (Range 0.5 64.0 0.5)) Nothing
            , modeField
            , rampField
            ]
            []
            (textureToValue (Perlin (8.0, 8.0) Clamp blackToWhite))
        , Variant
            "fbm"
            "Fractal noise"
            "Several octaves of Perlin noise added together, finer and fainter each time: the basis of clouds, stone, terrain and most natural textures."
            [ Field "scale" "Scale" "Size of the largest features, per unit, horizontally and vertically." (VectorField (Range 0.5 32.0 0.5)) Nothing
            , Field "octaves" "Octaves" "Number of noise layers; more adds finer detail." (IntField 1 12) Nothing
            , scalarField "persistence" "Persistence" "Strength of each layer relative to the one before." (Range 0.0 1.0 0.01)
            , scalarField "lacunarity" "Lacunarity" "Frequency of each layer relative to the one before." (Range 1.0 4.0 0.05)
            , Field
                "style"
                "Style"
                "Smooth rolls gently, billowy puffs up like cloud, ridged forms sharp crests."
                (EnumField [("smooth", "Smooth"), ("billowy", "Billowy"), ("ridged", "Ridged")])
                Nothing
            , modeField
            , rampField
            ]
            []
            (textureToValue (Fbm (4.0, 4.0) 5 0.5 2.0 Smooth Clamp blackToWhite))
        , Variant
            "turbulence"
            "Turbulence"
            "Displaces another texture by layered noise."
            [ scalarField "amount" "Amount" "How far points are displaced." (Range 0.0 2.0 0.01)
            , Field "octaves" "Octaves" "Number of noise layers." (IntField 1 12) Nothing
            , scalarField "persistence" "Persistence" "Strength of each layer relative to the one before." (Range 0.0 1.0 0.01)
            , scalarField "lacunarity" "Lacunarity" "Frequency of each layer relative to the one before." (Range 1.0 4.0 0.05)
            , textureField "base" "Base" "The texture being displaced."
            ]
            []
            (textureToValue (Turbulence 0.1 4 0.5 2.0 stripes))
        , Variant
            "tiled"
            "Checkerboard"
            "Alternates between two textures on a grid."
            [ Field "columns" "Columns" "" (IntField 1 64) Nothing
            , Field "rows" "Rows" "" (IntField 1 64) Nothing
            , textureField "a" "A" "Shown in the top-left tile."
            , textureField "b" "B" ""
            ]
            []
            (textureToValue (Tiled 4 4 (Flat white) (Flat black)))
        , Variant
            "layer"
            "Layer"
            "Draws one texture over another, blending by the top texture's transparency."
            [ textureField "top" "Top" ""
            , textureField "bottom" "Bottom" ""
            ]
            []
            (textureToValue (Layer (Circular (0.5, 0.5) 0.35 Clamp (Ramp [(0.0, white), (0.8, white), (1.0, clear)])) (Flat grey)))
        ]
    , rampVariants =
        [ Variant
            "stops"
            "Colour stops"
            "Interpolates between colours at given positions. Two stops at the same position make a hard edge."
            [Field "stops" "Stops" "" StopsField Nothing]
            []
            (rampToValue blackToWhite)
        , Variant
            "sinusoidal"
            "Sinusoidal"
            "Eases from one colour to the other along half a cosine wave; use the mirror mode to ease back and forth."
            [colourField "from" "From" "", colourField "to" "To" ""]
            []
            (rampToValue (Sinusoidal black white))
        , Variant
            "named"
            "Shared ramp"
            "A ramp defined once in this texture's ramps and used wherever it is named."
            [Field "name" "Name" "" TextField Nothing]
            []
            (rampToValue (NamedRamp "ramp"))
        , Variant
            "builtin"
            "Library ramp"
            "A read-only ramp from the built-in library."
            [Field "name" "Name" "" TextField Nothing]
            []
            (rampToValue (BuiltinRamp "greyscale"))
        ]
    }
  where
    rampField = Field "ramp" "Ramp" "" RampField Nothing
    modeField =
      Field
        "mode"
        "Beyond the ends"
        "What happens to values outside the ramp: keep the end colours, repeat the ramp, or repeat it reversing every other copy."
        (EnumField [("clamp", "Clamp"), ("wrap", "Repeat"), ("mirror", "Mirror")])
        Nothing
    pointField key label help = Field key label help (PointField (Range 0.0 1.0 0.01)) (Just PointHandle)
    scalarField key label help range = Field key label help (ScalarField range) Nothing
    colourField key label help = Field key label help ColourField Nothing
    textureField key label help = Field key label help TextureField Nothing
    blackToWhite = Ramp [(0.0, black), (1.0, white)]
    stripes = Linear (0.0, 0.5) (0.25, 0.5) Mirror (Ramp [(0.0, black), (1.0, white)])

-- | What a node becomes when it is deleted or a blank document is created.
defaultTexture :: Texture
defaultTexture = Flat grey

black, white, grey, clear :: Colour
black = (0.0, 0.0, 0.0, 1.0)
white = (1.0, 1.0, 1.0, 1.0)
grey = (128 / 255, 128 / 255, 128 / 255, 1.0)
clear = (1.0, 1.0, 1.0, 0.0)

schemaToValue :: Schema -> Value
schemaToValue s =
  object
    [ "version" .= currentVersion
    , "texture" .= map variantToValue (textureVariants s)
    , "ramp" .= map variantToValue (rampVariants s)
    , "defaultTexture" .= textureToValue defaultTexture
    ]

variantToValue :: Variant -> Value
variantToValue variant =
  object
    [ "type" .= variantType variant
    , "label" .= variantLabel variant
    , "description" .= variantDescription variant
    , "fields" .= map fieldToValue (variantFields variant)
    , "guides" .= map guideToValue (variantGuides variant)
    , "default" .= variantDefault variant
    ]

fieldToValue :: Field -> Value
fieldToValue field =
  object
    ( [ "key" .= fieldKey field
      , "label" .= fieldLabel field
      , "help" .= fieldHelp field
      ]
        <> kindPairs (fieldKind field)
        <> maybe [] (\h -> ["handle" .= handleToValue h]) (fieldHandle field)
    )

kindPairs :: FieldKind -> [Pair]
kindPairs kind =
  case kind of
    ScalarField range -> ("kind" .= ("scalar" :: Text)) : rangePairs range
    IntField lo hi -> ["kind" .= ("int" :: Text), "min" .= lo, "max" .= hi, "step" .= (1 :: Int)]
    PointField range -> ("kind" .= ("point" :: Text)) : rangePairs range
    VectorField range -> ("kind" .= ("vector" :: Text)) : rangePairs range
    ColourField -> ["kind" .= ("colour" :: Text)]
    EnumField options ->
      [ "kind" .= ("enum" :: Text)
      , "options" .= [object ["value" .= v, "label" .= l] | (v, l) <- options]
      ]
    StopsField -> ["kind" .= ("stops" :: Text)]
    TextField -> ["kind" .= ("text" :: Text)]
    RampField -> ["kind" .= ("ramp" :: Text)]
    TextureField -> ["kind" .= ("texture" :: Text)]

rangePairs :: Range -> [Pair]
rangePairs range =
  ["min" .= rangeMin range, "max" .= rangeMax range, "step" .= rangeStep range]

handleToValue :: Handle -> Value
handleToValue handle =
  case handle of
    PointHandle -> object ["kind" .= ("point" :: Text)]
    RadiusHandle centre -> object ["kind" .= ("radius" :: Text), "centre" .= centre]

guideToValue :: Guide -> Value
guideToValue (LineGuide from to) =
  object ["kind" .= ("line" :: Text), "from" .= from, "to" .= to]
