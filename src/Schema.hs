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

import qualified Reaction as R
import qualified Cellular as C
import ColourRamps (ColourRamp (..), RampMode (..))
import Colours (Colour)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import Texture (NoiseStyle (..), Texture (..), Scalar(..), Vector(..), Domain(..), Arithmetic(..), SdfOperation(..), BlendMode(..))
import TextureJson (currentVersion, rampToValue, textureToValue, scalarToValue, vectorToValue, domainToValue)

data Schema = Schema
  { textureVariants :: [Variant]
  , scalarVariants :: [Variant]
  , vectorVariants :: [Variant]
  , domainVariants :: [Variant]
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
  | ScalarNodeField
  | VectorNodeField
  | DomainField

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
        [ Variant "blend" "Colour blend" "Blend source RGB with a backdrop, then source-over composite their alpha. Input channels and opacity are clamped." [exprField "mode" "Blend mode" (EnumField [("normal","Normal"),("multiply","Multiply"),("screen","Screen"),("overlay","Overlay"),("soft-light","Soft light"),("darken","Darken"),("lighten","Lighten"),("difference","Difference"),("exclusion","Exclusion")]),numberHint "opacity" "Opacity" 0 1,exprField "top" "Source" TextureField,exprField "bottom" "Backdrop" TextureField] [] (textureToValue (BlendTexture MultiplyBlend 1 (Flat (0.8,0.6,0.3,1)) (Flat (0.2,0.5,0.8,1))))
        , Variant "vector-colour" "Vector colour" "Map vector components from [-1,1] into RGB." [exprField "field" "Vector" VectorNodeField] [] (textureToValue (VectorColour Position))
        , Variant "colourise" "Colour map" "Map any scalar field through a colour ramp." [exprField "field" "Field" ScalarNodeField, modeHint, Field "ramp" "Ramp" "" RampField Nothing] [] (textureToValue (Colourise Noise Clamp greyRamp))
        , Variant "domain" "Apply domain" "Evaluate the base texture at transformed coordinates." [exprField "domain" "Coordinates" DomainField, exprField "base" "Base" TextureField] [] (textureToValue (InDomain (Translate (0,0,0)) (Flat grey)))
        , Variant "mix" "Scalar mask" "Mix two textures by a scalar mask clamped to [0,1]." [exprField "mask" "Mask" ScalarNodeField,exprField "a" "A (mask 1)" TextureField,exprField "b" "B (mask 0)" TextureField] [] (textureToValue (Mix Noise (Flat white) (Flat black)))
        , Variant "flat" "Flat" "A single colour everywhere." [colourField "colour" "Colour" ""] [] (textureToValue (Flat grey))
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
            (textureToValue (Linear (0.0, 0.5, 0) (1.0, 0.5, 0) Clamp blackToWhite))
        , Variant
            "radial"
            "Cylindrical sweep"
            "A mirrored angular ramp around a cylinder axis. The default axis is z; its z=0 slice preserves the original sweep."
            [pointField "centre" "Centre" "", Field "axis" "Axis" "Cylinder direction." (VectorField (Range (-1) 1 0.01)) Nothing, modeField, rampField]
            []
            (textureToValue (Radial (0.5, 0.5, 0) (0,0,1) Clamp blackToWhite))
        , Variant
            "circular"
            "Spherical shells"
            "A ramp through spherical shells, reaching position 1 at the radius."
            [ pointField "centre" "Centre" ""
            , (scalarField "radius" "Radius" "Distance at which the ramp reaches position 1." (Range 0.0 1.0 0.005))
                { fieldHandle = Just (RadiusHandle "centre")
                }
            , modeField
            , rampField
            ]
            []
            (textureToValue (Circular (0.5, 0.5, 0) 0.5 Clamp blackToWhite))
        , Variant
            "perlin"
            "Perlin noise"
            "Smooth gradient noise between 0 and 1, coloured by a ramp."
            [ Field "scale" "Scale" "Noise features per unit, along x, y and z." (VectorField (Range 0.5 64.0 0.5)) Nothing
            , modeField
            , rampField
            ]
            []
            (textureToValue (Perlin (8.0, 8.0, sqrt (abs (8.0 * 8.0))) Clamp blackToWhite))
        , Variant
            "fbm"
            "Fractal noise"
            "Several octaves of Perlin noise added together, finer and fainter each time: the basis of clouds, stone, terrain and most natural textures."
            [ Field "scale" "Scale" "Size of the largest features, per unit, along x, y and z." (VectorField (Range 0.5 32.0 0.5)) Nothing
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
            (textureToValue (Fbm (4.0, 4.0, sqrt (abs (4.0 * 4.0))) 5 0.5 2.0 Smooth Clamp blackToWhite))
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
            , Field "depth" "Depth" "Tiles along z." (IntField 1 64) Nothing
            , textureField "a" "A" "Shown in the top-left tile."
            , textureField "b" "B" ""
            ]
            []
            (textureToValue (Tiled 4 4 1 (Flat white) (Flat black)))
        , Variant
            "layer"
            "Layer"
            "Draws one texture over another, blending by the top texture's transparency."
            [ textureField "top" "Top" ""
            , textureField "bottom" "Bottom" ""
            ]
            []
            (textureToValue (Layer (Circular (0.5, 0.5, 0) 0.35 Clamp (Ramp [(0.0, white), (0.8, white), (1.0, clear)])) (Flat grey)))
        ]
    , scalarVariants = scalarSchema
    , vectorVariants = vectorSchema
    , domainVariants = domainSchema
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
    stripes = Linear (0.0, 0.5, 0) (0.25, 0.5, 0) Mirror (Ramp [(0.0, black), (1.0, white)])

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
    , "scalar" .= map variantToValue (scalarVariants s)
    , "vector" .= map variantToValue (vectorVariants s)
    , "domain" .= map variantToValue (domainVariants s)
    , "ramp" .= map variantToValue (rampVariants s)
    , "validation" .= object ["texture" .= map validationVariant (textureVariants s), "ramp" .= map validationVariant (rampVariants s), "scalar" .= map validationVariant (scalarVariants s), "vector" .= map validationVariant (vectorVariants s), "domain" .= map validationVariant (domainVariants s)]
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
    ScalarNodeField -> ["kind" .= ("scalarNode" :: Text)]
    VectorNodeField -> ["kind" .= ("vectorNode" :: Text)]
    DomainField -> ["kind" .= ("domain" :: Text)]

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

-- | Structural rules deliberately omit slider bounds and other presentation
-- hints. TextureJson remains the semantic reference; shared document fixtures
-- verify the client consumes these rules with matching migrations/defaults.
validationVariant :: Variant -> Value
validationVariant variant = object
  [ "type" .= variantType variant
  , "fields" .= map validationField (variantFields variant)
  ]

validationField :: Field -> Value
validationField field = object
  (["key" .= fieldKey field] <> rule (fieldKind field)
   <> ["default" .= ("clamp" :: Text) | fieldKey field == "mode"])
  where
    kind name = ["kind" .= (name :: Text)]
    rule value = case value of
      ScalarField _ -> kind "number"
      IntField _ _ -> kind "integer"
      PointField _ -> kind "vector3"
      VectorField _ -> kind "vector3"
      ColourField -> kind "colour"
      EnumField options -> kind "enum" <> ["choices" .= map fst options]
      StopsField -> kind "stops"
      TextField -> kind "string"
      RampField -> kind "ramp"
      TextureField -> kind "texture"
      ScalarNodeField -> kind "scalarNode"
      VectorNodeField -> kind "vectorNode"
      DomainField -> kind "domain"

-- Expression widgets describe typed child edges, independently of numeric hints.
exprField :: Text -> Text -> FieldKind -> Field
exprField key label kind = Field key label "" kind Nothing
numberHint :: Text -> Text -> Double -> Double -> Field
numberHint key label lo hi = exprField key label (ScalarField (Range lo hi 0.01))
vectorHint :: Text -> Text -> Double -> Double -> Field
vectorHint key label lo hi = exprField key label (VectorField (Range lo hi 0.01))
pointHint :: Text -> Field
pointHint key = (exprField key "Centre" (PointField (Range 0 1 0.01))) {fieldHandle=Just PointHandle}
modeHint :: Field
modeHint = exprField "mode" "Beyond the ends" (EnumField [("clamp","Clamp"),("wrap","Repeat"),("mirror","Mirror")])
greyRamp :: ColourRamp
greyRamp = Ramp [(0,black),(1,white)]
fractalHints :: [Field]
fractalHints = [exprField "octaves" "Octaves" (IntField 1 12),numberHint "persistence" "Persistence" 0 1,numberHint "lacunarity" "Lacunarity" 1 4,exprField "source" "Noise source" ScalarNodeField]
scalarSchema :: [Variant]
scalarSchema =
  [ scalar "constant" "Constant" "A scalar value everywhere." [numberHint "value" "Value" (-1) 1] (Constant 0.5)
  , scalar "reaction-diffusion" "Reaction–diffusion" "Precomputed 3D Gray–Scott concentrations, sampled periodically with trilinear interpolation. Chemistry edits resimulate; colour and domain edits reuse the volume." reactionHints (ReactionField (R.Config 24 1200 0.022 0.051 0.9 0.45 1 42 R.SeedSpots) R.V)
  , scalar "worley" "Worley noise" "Distances to the first/second seeded feature point. Gap is F2 minus F1, not edge distance." (cellHints <> [exprField "metric" "Distance metric" (EnumField [("euclidean","Euclidean"),("manhattan","Manhattan"),("chebyshev","Chebyshev")]),exprField "output" "Output" (EnumField [("f1","F1"),("f2","F2"),("gap","F2 − F1")])]) (Worley 3 1 0 C.Euclidean C.F1)
  , scalar "cell-value" "Cell random value" "A seeded value in [0,1) for each Euclidean Voronoi cell." cellHints (CellValue 3 1 0)
  , scalar "cell-edge" "Voronoi edge distance" "True Euclidean distance to the closest cell bisector; zero on cell boundaries." cellHints (CellEdge 3 1 0)
  , scalar "sphere" "Sphere SDF" "Signed distance: negative inside, zero on the surface." [pointHint "centre",numberHint "radius" "Radius" 0 1] (SdfSphere (0.5,0.5,0.5) 0.3)
  , scalar "box" "Box SDF" "Exact signed distance to an axis-aligned box." [pointHint "centre",vectorHint "half" "Half extents" 0 1] (SdfBox (0.5,0.5,0.5) (0.3,0.2,0.25))
  , scalar "cylinder" "Cylinder SDF" "Capped cylinder along Y; rotate its domain to change axis." [pointHint "centre",numberHint "radius" "Radius" 0 1,numberHint "height" "Half height" 0 1] (SdfCylinder (0.5,0.5,0.5) 0.3 0.4)
  , scalar "torus" "Torus SDF" "Ring around Y, with major and tube radii." [pointHint "centre",numberHint "major" "Major radius" 0 1,numberHint "minor" "Tube radius" 0 1] (SdfTorus (0.5,0.5,0.5) 0.3 0.1)
  , scalar "plane" "Plane SDF" "Signed distance along a normal (normalised automatically)." [vectorHint "normal" "Normal" (-1) 1,numberHint "offset" "Offset" (-1) 1] (SdfPlane (1,1,0) 0.5)
  , sdf "sdf-union" "SDF union" SdfUnion
  , sdf "sdf-intersection" "SDF intersection" SdfIntersection
  , sdf "sdf-difference" "SDF difference" SdfDifference
  , scalar "planar" "Planar distance" "Projected distance from the start to the end." [vectorHint "from" "From" 0 1,vectorHint "to" "To" 0 1] (Planar (0,0,0) (1,0,0))
  , scalar "distance" "Point distance" "Distance from the centre divided by radius." [pointHint "centre",numberHint "radius" "Radius" 0 1] (Distance (0.5,0.5,0) 0.5)
  , scalar "angular" "Cylindrical angle" "Mirrored sweep around an axis." [pointHint "centre",vectorHint "axis" "Axis" (-1) 1] (Angular (0.5,0.5,0) (0,0,1))
  , scalar "noise" "Perlin source" "Uncoloured Perlin noise; transform its coordinates to change frequency." [] Noise
  , scalar "fractal" "Fractal sum" "Rotated, offset octaves of any scalar noise source, with artistic contrast." (fractalHints <> [exprField "style" "Style" (EnumField [("smooth","Smooth"),("billowy","Billowy"),("ridged","Ridged")])]) (Fractal 5 0.5 2 Smooth Noise)
  , scalar "absolute-fractal" "Turbulence sum" "Absolute centred octaves of any scalar source." fractalHints (AbsoluteFractal 4 0.5 2 Noise)
  , scalar "scalar-domain" "Transform scalar" "Sample a scalar in another coordinate system." [exprField "domain" "Coordinates" DomainField,exprField "source" "Source" ScalarNodeField] (ScalarDomain (Scale (0.25,0.25,0.25)) Noise)
  , binary "add" "Add" Add, binary "multiply" "Multiply" Multiply, binary "min" "Minimum" Minimum, binary "max" "Maximum" Maximum
  , scalar "remap" "Remap" "Map one interval to another, without clamping." [numberHint "low" "Input low" (-1) 1,numberHint "high" "Input high" (-1) 1,numberHint "outLow" "Output low" (-1) 1,numberHint "outHigh" "Output high" (-1) 1,exprField "source" "Source" ScalarNodeField] (Remap 0 1 (-1) 1 Noise)
  , scalar "threshold" "Smooth threshold" "A smooth mask between two thresholds; equal thresholds make a hard step." [numberHint "low" "Low" 0 1,numberHint "high" "High" 0 1,exprField "source" "Source" ScalarNodeField] (Threshold 0.4 0.6 Noise)
  ]
  where scalar tag label help fields value = Variant tag label help fields [] (scalarToValue value)
        sdf tag label op = scalar tag label "Combine distance fields. Amount zero is hard; positive amount rounds the join." [numberHint "amount" "Smoothing radius" 0 0.5,exprField "a" "A" ScalarNodeField,exprField "b" "B" ScalarNodeField] (SdfCombine op 0.1 (SdfSphere (0.4,0.5,0.5) 0.3) (SdfBox (0.6,0.5,0.5) (0.2,0.2,0.2)))
        binary tag label op = scalar tag label "Combine two scalar fields." [exprField "a" "A" ScalarNodeField,exprField "b" "B" ScalarNodeField] (Arithmetic op Noise (Constant 0.5))
vectorSchema :: [Variant]
vectorSchema =
  [ vector "vector-constant" "Constant vector" "A fixed three-coordinate vector." [vectorHint "value" "Value" (-1) 1] (VectorConstant (0.1,0,0))
  , vector "cell-id" "Cell identity" "Integer lattice coordinates of the nearest Euclidean feature point. Stable identity, not a scalar noise value." cellHints (CellIdentity 3 1 0)
  , vector "cell-colour" "Cell random colour" "Seeded RGB per Euclidean cell, represented as a vector in [-1,1] for Vector colour." cellHints (CellColour 3 1 0)
  , vector "position" "Position vector" "The current sampling coordinates." [] Position
  , vector "components" "Vector components" "Three independently editable scalar fields." [exprField "x" "X" ScalarNodeField,exprField "y" "Y" ScalarNodeField,exprField "z" "Z" ScalarNodeField] (Components Noise (Constant 0) (Constant 0))
  , vector "vector-add" "Add vectors" "Add two displacement fields." [exprField "a" "A" VectorNodeField,exprField "b" "B" VectorNodeField] (VectorAdd Position (VectorConstant (0,0,0)))
  , vector "vector-scale" "Scale vector by field" "Attenuate a displacement by a scalar mask." [exprField "amount" "Amount" ScalarNodeField,exprField "source" "Source" VectorNodeField] (VectorScale (Constant 0.1) Position)
  , vector "vector-domain" "Transform vector" "Sample vector components at transformed coordinates; does not rotate the output vector." [exprField "domain" "Coordinates" DomainField,exprField "source" "Source" VectorNodeField] (VectorDomain (Translate (0,0,0)) Position)
  ] where vector tag label help fields value = Variant tag label help fields [] (vectorToValue value)
domainSchema :: [Variant]
domainSchema =
  [ domain "translate" "Translate" "Subtract the offset from sampling coordinates." [vectorHint "offset" "Offset" (-1) 1] (Translate (0,0,0))
  , domain "rotate" "Rotate" "Inverse Euler rotation: undo Z, Y, then X. Angles are degrees." [vectorHint "rotation" "Degrees" (-180) 180] (Rotate (0,0,30))
  , domain "scale" "Scale" "Divide coordinates by scale; zero collapses that axis." [vectorHint "scale" "Scale" 0.01 2] (Scale (1,1,1))
  , domain "repeat" "Repeat cells" "Centred modulo cells; nonpositive periods disable an axis." [vectorHint "period" "Period" 0 1] (Repeat (0.25,0.25,0))
  , domain "mirror" "Mirror" "Fold axes whose selection is at least 0.5 around the centre." [pointHint "centre",vectorHint "axes" "Axes" 0 1] (MirrorDomain (0.5,0.5,0) (1,0,0))
  , domain "polar-repeat" "Polar repeat" "Fold XY into angular sectors around the centre, preserving Z." [pointHint "centre",exprField "count" "Sectors" (IntField 1 32)] (PolarRepeat (0.5,0.5,0) 8)
  , domain "radial-repeat" "Radial repeat" "Wrap XY radius into rings, preserving angle and Z." [pointHint "centre",numberHint "period" "Period" 0 1] (RadialRepeat (0.5,0.5,0) 0.15)
  , domain "twist" "Twist" "Rotate XY about Z by height. Amount is degrees per unit." [pointHint "centre",numberHint "amount" "Degrees per unit" (-720) 720] (Twist (0.5,0.5,0) 240)
  , domain "bend" "Bend" "Rotate XY by horizontal position. Amount is degrees per unit." [pointHint "centre",numberHint "amount" "Degrees per unit" (-360) 360] (Bend (0.5,0.5,0) 120)
  , domain "compose" "Compose domains" "Apply First to coordinates, then Second. Order matters." [exprField "first" "First" DomainField,exprField "second" "Second" DomainField] (Compose (Translate (0.5,0.5,0)) (Rotate (0,0,30)))
  , domain "warp" "Vector warp" "Add Amount times an arbitrary vector field to coordinates." [numberHint "amount" "Amount" (-1) 1,exprField "field" "Displacement" VectorNodeField] (Warp 0.1 (Components Noise (Constant 0) (Constant 0)))
  ] where domain tag label help fields value = Variant tag label help fields [] (domainToValue value)

cellHints :: [Field]
cellHints = [exprField "dimensions" "Dimensions (2 or 3)" (IntField 2 3),numberHint "jitter" "Jitter" 0 1,exprField "seed" "Seed" (IntField 0 65535)]

reactionHints :: [Field]
reactionHints = [exprField "resolution" "Voxel resolution" (IntField 8 64),exprField "iterations" "Iterations" (IntField 0 2000)
  , exprField "feed" "Feed" (ScalarField (Range 0 0.1 0.001)),exprField "kill" "Kill" (ScalarField (Range 0 0.1 0.001))
  , numberHint "diffusionU" "Diffusion U" 0 1,numberHint "diffusionV" "Diffusion V" 0 1,numberHint "timeStep" "Time step" 0 1
  , exprField "seed" "Seed" (IntField 0 65535),exprField "initial" "Initial state" (EnumField [("noise","Seeded patches"),("spots","Regular spots"),("slab","Slab")])
  , exprField "output" "Concentration" (EnumField [("u","U"),("v","V")])]
