{-# LANGUAGE OverloadedStrings #-}

-- | JSON encoding of textures and texture documents.
--
-- A document is the unit that is saved, loaded and sent to the server:
--
-- > {"version": 5, "name": "Marble", "description": "...", "texture": {...}}
--
-- Textures and ramps are tagged objects (@{"type": "linear", ...}@). Colours
-- are written as @"#rrggbbaa"@ hex strings when they are exactly representable
-- with 8-bit channels and as @[r, g, b, a]@ arrays of numbers in [0, 1]
-- otherwise, so encoding never loses precision. Both forms (and @"#rrggbb"@)
-- are accepted when parsing.
module TextureJson
  ( Document (..)
  , ExampleGuide (..), guideToValue, parseGuide
  , simpleDocument
  , version2WrappingLibraryRamps
  , currentVersion
  , documentToValue
  , parseDocument
  , decodeDocument
  , encodeDocumentPretty
  , encodeValuePretty
  , migrateDocument
  , textureToValue
  , parseTexture
  , scalarToValue, vectorToValue, domainToValue
  , parseScalar, parseVector, parseDomain
  , rampToValue
  , parseRamp
  , colourToValue
  , parseColour
  ) where

import ColourRamps (ColourRamp (..), RampMode (..))
import Colours (Colour)
import Data.Aeson
  ( Value (..)
  , eitherDecode
  , encode
  , object
  , parseJSON
  , withArray
  , withObject
  , withText
  , (.:)
  , (.:?)
  , (.=)
  )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Control.Monad (zipWithM)
import Data.Aeson.Types (JSONPathElement (Index, Key), Key, Object, Pair, Parser, explicitParseField, explicitParseFieldMaybe, parseEither, typeMismatch, (<?>))
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Char (intToDigit, isHexDigit, digitToInt)
import Data.Foldable (toList)
import Data.List (intersperse, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Vector3 (Vec3)
import qualified Reaction as R
import qualified Cellular as C
import Texture (NoiseStyle (..), Texture (..), Scalar (..), Vector (..), Domain (..), Arithmetic (..), SdfOperation (..), BlendMode (..))

data Document = Document
  { documentName :: Text
  , documentDescription :: Text
  , documentCategory :: Text
  -- ^ Browsing category; empty if none. Unknown/legacy categories are retained.
  , documentGuide :: Maybe ExampleGuide
  -- ^ Optional library guidance. Independent of rendering and document version.
  , documentRamps :: Map Text ColourRamp
  -- ^ Named ramps, referred to from the texture as 'NamedRamp'. Definitions
  -- are always concrete: never themselves references.
  , documentTexture :: Texture
  }
  deriving (Eq, Show)

-- | A document with just a name and a texture.
simpleDocument :: Text -> Texture -> Document
simpleDocument name =
  Document name "" "" Nothing Map.empty

data ExampleGuide = ExampleGuide
  { guideRole :: Text
  , guideTags :: [Text]
  , guideFamily :: Text
  , guideOrder :: Int
  , guideHint :: Text
  , guidePreview :: Maybe (Text, Double)
  } deriving (Eq, Show)

guideToValue :: ExampleGuide -> Value
guideToValue guide = object $
  [ "role" .= guideRole guide, "tags" .= guideTags guide, "order" .= guideOrder guide ]
  <> ["family" .= guideFamily guide | not (T.null (guideFamily guide))]
  <> ["hint" .= guideHint guide | not (T.null (guideHint guide))]
  <> ["preview" .= object ["axis" .= axis, "position" .= position] | Just (axis, position) <- [guidePreview guide]]

parseGuide :: Value -> Parser ExampleGuide
parseGuide = withObject "Example guide" $ \o -> do
  role <- o .: "role"
  if role `elem` (["preset", "study", "comparison", "composition"] :: [Text])
    then pure () else fail "Unknown example role" <?> Key "role"
  order <- fromMaybe 100 <$> o .:? "order"
  if order >= 0 && order <= 9999 then pure () else fail "Example order must be 0–9999" <?> Key "order"
  preview <- explicitParseFieldMaybe (withObject "Preview slice" $ \p -> do
    axis <- fromMaybe "xy" <$> p .:? "axis"
    if axis `elem` (["xy", "xz", "yz"] :: [Text]) then pure () else fail "Preview axis must be xy, xz or yz" <?> Key "axis"
    position <- fromMaybe 0 <$> p .:? "position"
    if not (isNaN position || isInfinite position) && position >= (-2) && position <= 2
      then pure (axis, position) else fail "Preview position must be finite and between -2 and 2" <?> Key "position") o "preview"
  ExampleGuide role <$> (fromMaybe [] <$> o .:? "tags")
    <*> (fromMaybe "" <$> o .:? "family") <*> pure order
    <*> (fromMaybe "" <$> o .:? "hint") <*> pure preview

-- | Version history:
--
-- 1. The first format.
-- 2. Adds named ramps (a @ramps@ map and @named@ references to it),
--    @builtin@ references to the ramp library, and @category@.
-- 3. Moves each ramp's @mode@ (clamp, wrap, mirror) onto the texture node
--    that uses the ramp; ramps are just colours. Sinusoidal ramps no longer
--    mirror by themselves. Colours blend in OKLab.
-- 4. Three-coordinate points/scales, cylindrical Radial axis and checker depth.
-- 5. Composable scalar/vector fields and domain maps; old nodes remain conveniences.
currentVersion :: Int
currentVersion = 5

documentToValue :: Document -> Value
documentToValue document =
  object
    ( [ "version" .= currentVersion
      , "name" .= documentName document
      , "description" .= documentDescription document
      ]
        <> ["category" .= documentCategory document | not (T.null (documentCategory document))]
        <> ["guide" .= guideToValue guide | Just guide <- [documentGuide document]]
        <> [ "ramps" .= object [(Key.fromText name, rampToValue ramp) | (name, ramp) <- Map.toList (documentRamps document)]
           | not (Map.null (documentRamps document))
           ]
        <> ["texture" .= textureToValue (documentTexture document)]
    )

-- | Parse a document of any supported version, migrating it first.
parseDocument :: Value -> Either String Document
parseDocument value =
  migrateDocument value >>= parseEither documentParser

decodeDocument :: BL.ByteString -> Either String Document
decodeDocument bytes =
  eitherDecode bytes >>= parseDocument

documentParser :: Value -> Parser Document
documentParser =
  withObject "Document" $ \o ->
    Document
      <$> o .: "name"
      <*> (fromMaybe "" <$> o .:? "description")
      <*> (fromMaybe "" <$> o .:? "category")
      <*> explicitParseFieldMaybe parseGuide o "guide"
      <*> (fromMaybe Map.empty <$> explicitParseFieldMaybe parseRampDefinitions o "ramps")
      <*> explicitParseField parseTexture o "texture"

parseRampDefinitions :: Value -> Parser (Map Text ColourRamp)
parseRampDefinitions =
  withObject "ramps" $ \o ->
    Map.fromList <$> traverse definition (KeyMap.toList o)
  where
    definition (key, value) = do
      ramp <- parseRamp value <?> Key key
      case ramp of
        NamedRamp _ -> fail "Named ramp definitions must be concrete ramps, not references" <?> Key key
        BuiltinRamp _ -> fail "Named ramp definitions must be concrete ramps, not references" <?> Key key
        _ -> pure (Key.toText key, ramp)

-- | Bring a document of any earlier version up to 'currentVersion', one
-- version at a time.
migrateDocument :: Value -> Either String Value
migrateDocument value =
  case value of
    Object o ->
      case KeyMap.lookup "version" o of
        Nothing -> Left "Document has no \"version\" field"
        Just (Number n)
          | n == fromIntegral currentVersion -> Right value
          | n > fromIntegral currentVersion ->
              Left ("Document version " <> show n <> " is newer than this program supports (" <> show currentVersion <> ")")
          | n == 4 -> migrateDocument (Object (KeyMap.insert "version" (Number 5) o))
          | n == 1 -> migrateDocument (Object (KeyMap.insert "version" (Number 2) o))
          | n == 3 -> migrateDocument (Object (KeyMap.insert "version" (Number 4) (liftCoordinates o)))
          | n == 2 -> migrateDocument (Object (KeyMap.insert "version" (Number 3) (moveRampModes o)))
          | otherwise -> Left ("Unknown document version " <> show n)
        Just _ -> Left "Document \"version\" must be a number"
    _ -> Left "Document must be a JSON object"

adjust :: (Value -> Value) -> Key -> Object -> Object
adjust f key o = maybe o (\v -> KeyMap.insert key (f v) o) (KeyMap.lookup key o)

-- | Lift only coordinate fields, never colour arrays or ramp definitions.
liftCoordinates :: Object -> Object
liftCoordinates document = adjust liftNode "texture" document
  where
    liftNode (Object node) =
      let kind = KeyMap.lookup "type" node
          point (Array a) | length a == 2 = toJSON3 (toList a <> [Number 0])
          point v = v
          scale (Array a) | length a == 2 =
            case toList a of
              [Number x, Number y] -> toJSON3 [Number x,Number y,Number (realToFrac (depthScale x y))]
              _ -> Array a
          scale v = v
          children = foldr (adjust liftNode) node ["base","top","bottom","a","b"]
          lifted = foldr (adjust point) children ["from","to","centre"]
          scaled = adjust scale "scale" lifted
      in Object $ case kind of
           Just (String "radial") -> KeyMap.insert "axis" (toJSON3 [Number 0,Number 0,Number 1]) scaled
           Just (String "tiled") -> KeyMap.insert "depth" (Number 1) scaled
           _ -> scaled
    liftNode v = v
    toJSON3 = Array . V.fromList
    depthScale a b =
      let x = Sci.toRealFloat a :: Double
          y = Sci.toRealFloat b :: Double
          productXY = x*y
      in if isInfinite x || isInfinite y then 0 -- parseVec3 reports the original coordinate path.
         else if isInfinite productXY then sqrt (abs x) * sqrt (abs y)
         else sqrt (abs productXY)

-- | The version 2 to 3 migration: take the mode out of every ramp and put it
-- on the texture node using the ramp, so the document renders as before.
-- Inline ramps carry their own mode; named ramps take their definition's;
-- library ramps take the mode they had in version 2. Sinusoidal ramps,
-- which used to mirror by themselves, get the mirror mode.
moveRampModes :: Object -> Object
moveRampModes document =
  KeyMap.mapWithKey migrateTop document
  where
    definitionModes =
      case KeyMap.lookup "ramps" document of
        Just (Object definitions) -> KeyMap.map oldMode definitions
        _ -> KeyMap.empty
    migrateTop key value =
      case (key, value) of
        ("ramps", Object definitions) -> Object (KeyMap.map stripMode definitions)
        ("texture", node) -> migrateNode node
        _ -> value
    migrateNode value =
      case value of
        Object node ->
          let migrated = KeyMap.map migrateNode (KeyMap.delete "ramp" node)
          in case KeyMap.lookup "ramp" node of
               Just ramp -> Object (KeyMap.insert "mode" (useMode ramp) (KeyMap.insert "ramp" (stripMode ramp) migrated))
               Nothing -> Object migrated
        _ -> value
    useMode ramp =
      case ramp of
        Object r ->
          case KeyMap.lookup "type" r of
            Just (String "named") ->
              case KeyMap.lookup "name" r >>= \n -> case n of String t -> KeyMap.lookup (Key.fromText t) definitionModes; _ -> Nothing of
                Just m -> m
                Nothing -> String "clamp"
            Just (String "builtin") ->
              case KeyMap.lookup "name" r of
                Just (String name) | name `elem` version2WrappingLibraryRamps -> String "wrap"
                _ -> String "clamp"
            _ -> oldMode ramp
        _ -> String "clamp"
    oldMode ramp =
      case ramp of
        Object r ->
          case KeyMap.lookup "type" r of
            Just (String "sinusoidal") -> String "mirror"
            _ -> fromMaybe (String "clamp") (KeyMap.lookup "mode" r)
        _ -> String "clamp"
    stripMode ramp =
      case ramp of
        Object r -> Object (KeyMap.delete "mode" r)
        _ -> ramp

-- | The built-in ramps whose version 2 definitions used the wrap mode.
version2WrappingLibraryRamps :: [Text]
version2WrappingLibraryRamps =
  ["sandstone", "pine", "walnut", "marble-veins", "stripes", "rainbow", "candy"]

-- | Human-friendly formatting used for files on disk: a stable key order,
-- decimal numbers, and arrays of scalars (points, colours) kept on one line.
encodeDocumentPretty :: Document -> BL.ByteString
encodeDocumentPretty =
  encodeValuePretty . documentToValue

encodeValuePretty :: Value -> BL.ByteString
encodeValuePretty value =
  BB.toLazyByteString (prettyValue 0 value <> "\n")

prettyValue :: Int -> Value -> BB.Builder
prettyValue depth value =
  case value of
    Object o
      | KeyMap.null o -> "{}"
      | KeyMap.size o <= 3 && all isFlat (KeyMap.elems o) ->
          "{" <> mconcat (intersperse ", " (map inlineEntry (entries o))) <> "}"
      | otherwise ->
          let entry field = indentTo (depth + 1) <> inlineEntry' field
              inlineEntry' (key, v) = scalar (String (Key.toText key)) <> ": " <> prettyValue (depth + 1) v
          in "{\n" <> commaLines (map entry (entries o)) <> "\n" <> indentTo depth <> "}"
    Array items
      | null items -> "[]"
      | all isScalar items -> "[" <> mconcat (intersperse ", " (map scalar (toList items))) <> "]"
      | otherwise ->
          "[\n" <> commaLines [indentTo (depth + 1) <> prettyValue (depth + 1) v | v <- toList items] <> "\n" <> indentTo depth <> "]"
    _ -> scalar value
  where
    commaLines = mconcat . intersperse ",\n"
    entries o = sortOn (keyRank . fst) (KeyMap.toList o)
    inlineEntry (key, v) = scalar (String (Key.toText key)) <> ": " <> prettyValue depth v
    indentTo n = BB.string7 (replicate (2 * n) ' ')

-- | Scalars and arrays of scalars, which always print on one line.
isFlat :: Value -> Bool
isFlat value =
  case value of
    Array items -> all isScalar items
    _ -> isScalar value

isScalar :: Value -> Bool
isScalar value =
  case value of
    Object _ -> False
    Array _ -> False
    _ -> True

scalar :: Value -> BB.Builder
scalar value =
  case value of
    Number n
      | Sci.isInteger n -> BB.string7 (show (truncate n :: Integer))
      | otherwise -> BB.string7 (Sci.formatScientific Sci.Fixed Nothing n)
    _ -> BB.lazyByteString (encode value)

keyRank :: Key -> (Int, Text)
keyRank key =
  (fromMaybe (length keyOrdering) (lookup name (zip keyOrdering [0 ..])), name)
  where
    name = Key.toText key

keyOrdering :: [Text]
keyOrdering =
  [ "version", "name", "description", "category", "guide", "role", "tags", "family", "order", "hint", "preview", "axis", "ramps", "texture", "type"
  , "position", "colour", "from", "to", "centre", "radius", "scale"
  , "amount", "octaves", "persistence", "lacunarity", "style", "base"
  , "columns", "rows", "a", "b", "top", "bottom"
  , "mode", "stops", "ramp"
  ]

textureToValue :: Texture -> Value
textureToValue texture =
  case texture of
    Flat colour ->
      tagged "flat" ["colour" .= colourToValue colour]
    Linear from to mode ramp ->
      tagged "linear" (["from" .= from, "to" .= to] <> rampFields mode ramp)
    Radial centre axis mode ramp ->
      tagged "radial" (["centre" .= centre, "axis" .= axis] <> rampFields mode ramp)
    Circular centre radius mode ramp ->
      tagged "circular" (["centre" .= centre, "radius" .= radius] <> rampFields mode ramp)
    Perlin scale mode ramp ->
      tagged "perlin" (["scale" .= scale] <> rampFields mode ramp)
    Fbm scale octaves persistence lacunarity style mode ramp ->
      tagged
        "fbm"
        ( [ "scale" .= scale
        , "octaves" .= octaves
        , "persistence" .= persistence
        , "lacunarity" .= lacunarity
        , "style" .= noiseStyleName style
        ]
          <> rampFields mode ramp
        )
    Turbulence amount octaves persistence lacunarity base ->
      tagged
        "turbulence"
        [ "amount" .= amount
        , "octaves" .= octaves
        , "persistence" .= persistence
        , "lacunarity" .= lacunarity
        , "base" .= textureToValue base
        ]
    Tiled columns rows depth a b ->
      tagged "tiled" ["columns" .= columns, "rows" .= rows, "depth" .= depth, "a" .= textureToValue a, "b" .= textureToValue b]
    BlendTexture mode opacity top bottom -> tagged "blend" ["mode" .= blendModeName mode,"opacity" .= opacity,"top" .= textureToValue top,"bottom" .= textureToValue bottom]
    Layer top bottom ->
      tagged "layer" ["top" .= textureToValue top, "bottom" .= textureToValue bottom]
    VectorColour vector -> tagged "vector-colour" ["field" .= vectorToValue vector]
    Colourise field mode ramp -> tagged "colourise" (["field" .= scalarToValue field] <> rampFields mode ramp)
    InDomain domain base -> tagged "domain" ["domain" .= domainToValue domain, "base" .= textureToValue base]
    Mix mask a b -> tagged "mix" ["mask" .= scalarToValue mask, "a" .= textureToValue a, "b" .= textureToValue b]

parseTexture :: Value -> Parser Texture
parseTexture =
  withObject "Texture" $ \o -> do
    kind <- o .: "type"
    case kind :: Text of
      "blend" -> BlendTexture <$> explicitParseField parseBlendMode o "mode" <*> finiteField o "opacity" <*> child o "top" <*> child o "bottom"
      "vector-colour" -> VectorColour <$> explicitParseField parseVector o "field"
      "colourise" -> Colourise <$> explicitParseField parseScalar o "field" <*> mode o <*> ramp o
      "domain" -> InDomain <$> explicitParseField parseDomain o "domain" <*> child o "base"
      "mix" -> Mix <$> explicitParseField parseScalar o "mask" <*> child o "a" <*> child o "b"
      "flat" -> Flat <$> explicitParseField parseColour o "colour"
      "linear" -> Linear <$> vector o "from" <*> vector o "to" <*> mode o <*> ramp o
      "radial" -> Radial <$> vector o "centre" <*> vector o "axis" <*> mode o <*> ramp o
      "circular" -> Circular <$> vector o "centre" <*> o .: "radius" <*> mode o <*> ramp o
      "perlin" -> Perlin <$> vector o "scale" <*> mode o <*> ramp o
      "fbm" ->
        Fbm
          <$> vector o "scale"
          <*> o .: "octaves"
          <*> o .: "persistence"
          <*> o .: "lacunarity"
          <*> explicitParseField parseNoiseStyle o "style"
          <*> mode o
          <*> ramp o
      "turbulence" ->
        Turbulence
          <$> o .: "amount"
          <*> o .: "octaves"
          <*> o .: "persistence"
          <*> o .: "lacunarity"
          <*> child o "base"
      "tiled" -> Tiled <$> o .: "columns" <*> o .: "rows" <*> o .: "depth" <*> child o "a" <*> child o "b"
      "layer" -> Layer <$> child o "top" <*> child o "bottom"
      _ -> fail ("Unknown texture type " <> show kind)
  where
    vector o = explicitParseField parseVec3 o
    ramp o = explicitParseField parseRamp o "ramp"
    mode o = fromMaybe Clamp <$> explicitParseFieldMaybe parseMode o "mode"
    child o key = explicitParseField parseTexture o key

-- | Infinity cannot safely be hashed onto a lattice or projected onto an axis.
parseVec3 :: Value -> Parser Vec3
parseVec3 value = do
  vector@(x,y,z) <- parseJSON value
  if all (\n -> not (isNaN n || isInfinite n)) [x,y,z]
    then pure vector
    else fail "Coordinates must be finite numbers"

-- | A ramp and the mode it is used with, as fields of a texture node.
rampFields :: RampMode -> ColourRamp -> [Pair]
rampFields mode ramp =
  ["mode" .= modeName mode, "ramp" .= rampToValue ramp]

rampToValue :: ColourRamp -> Value
rampToValue ramp =
  case ramp of
    Ramp stops ->
      tagged
        "stops"
        ["stops" .= [object ["position" .= position, "colour" .= colourToValue colour] | (position, colour) <- stops]]
    Sinusoidal from to ->
      tagged "sinusoidal" ["from" .= colourToValue from, "to" .= colourToValue to]
    NamedRamp name ->
      tagged "named" ["name" .= name]
    BuiltinRamp name ->
      tagged "builtin" ["name" .= name]

parseRamp :: Value -> Parser ColourRamp
parseRamp =
  withObject "ColourRamp" $ \o -> do
    kind <- o .: "type"
    case kind :: Text of
      "stops" ->
        Ramp <$> explicitParseField (withArray "stops" (zipWithM indexed [0 ..] . toList)) o "stops"
      "sinusoidal" ->
        Sinusoidal
          <$> explicitParseField parseColour o "from"
          <*> explicitParseField parseColour o "to"
      "named" -> NamedRamp <$> o .: "name"
      "builtin" -> BuiltinRamp <$> o .: "name"
      _ -> fail ("Unknown ramp type " <> show kind)
  where
    indexed i v = parseStop v <?> Index i
    parseStop =
      withObject "Stop" $ \o ->
        (,) <$> o .: "position" <*> explicitParseField parseColour o "colour"

noiseStyleName :: NoiseStyle -> Text
noiseStyleName style =
  case style of
    Smooth -> "smooth"
    Billowy -> "billowy"
    Ridged -> "ridged"

parseNoiseStyle :: Value -> Parser NoiseStyle
parseNoiseStyle =
  withText "NoiseStyle" $ \name ->
    case name of
      "smooth" -> pure Smooth
      "billowy" -> pure Billowy
      "ridged" -> pure Ridged
      _ -> fail ("Unknown noise style " <> show name <> " (expected smooth, billowy or ridged)")

modeName :: RampMode -> Text
modeName mode =
  case mode of
    Clamp -> "clamp"
    Wrap -> "wrap"
    Mirror -> "mirror"

parseMode :: Value -> Parser RampMode
parseMode =
  withText "RampMode" $ \name ->
    case name of
      "clamp" -> pure Clamp
      "wrap" -> pure Wrap
      "mirror" -> pure Mirror
      _ -> fail ("Unknown ramp mode " <> show name <> " (expected clamp, wrap or mirror)")

colourToValue :: Colour -> Value
colourToValue (r, g, b, a) =
  case traverse toByte [r, g, b, a] of
    Just bytes -> String (T.pack ('#' : concatMap hexByte bytes))
    Nothing -> toJSONList [r, g, b, a]
  where
    toByte channel =
      let scaled = channel * 255.0
          rounded = round scaled :: Int
      in if channel >= 0.0 && channel <= 1.0 && fromIntegral rounded / 255.0 == channel
           then Just rounded
           else Nothing
    hexByte n = [intToDigit (n `div` 16), intToDigit (n `mod` 16)]
    toJSONList = Array . foldMap (pure . Number . Sci.fromFloatDigits)

parseColour :: Value -> Parser Colour
parseColour value =
  case value of
    String text -> parseHex (T.unpack text)
    Array _ -> withArray "Colour" (parseChannels . toList) value
    _ -> typeMismatch "Colour (\"#rrggbbaa\" or [r, g, b, a])" value
  where
    parseHex ('#' : digits)
      | all isHexDigit digits =
          case bytes digits of
            [r, g, b] -> pure (channel r, channel g, channel b, 1.0)
            [r, g, b, a] -> pure (channel r, channel g, channel b, channel a)
            _ -> badHex
    parseHex _ = badHex
    badHex = fail "Colour strings must look like \"#rrggbb\" or \"#rrggbbaa\""
    bytes (hi : lo : rest) = (digitToInt hi * 16 + digitToInt lo) : bytes rest
    bytes [_] = [-1, -1, -1, -1, -1]
    bytes [] = []
    channel n = fromIntegral n / 255.0
    parseChannels channels = do
      numbers <- traverse parseUnit channels
      case numbers of
        [r, g, b] -> pure (r, g, b, 1.0)
        [r, g, b, a] -> pure (r, g, b, a)
        _ -> fail "Colour arrays must have 3 or 4 numbers"
    parseUnit v =
      case v of
        Number n -> pure (realToFrac n)
        _ -> typeMismatch "Number" v

tagged :: Text -> [(Key, Value)] -> Value
tagged kind fields =
  Object (KeyMap.fromList (("type", String kind) : fields))

-- Typed expression edges are parsed independently: a colour node cannot stand
-- in for a scalar source, vector component or domain map.
scalarToValue :: Scalar -> Value
scalarToValue field = case field of
  ReactionField c chemical -> tagged "reaction-diffusion" (reactionFields c <> ["output" .= (if chemical==R.U then "u" else "v" :: Text)])
  Worley d j s m out -> tagged "worley" (cellFields d j s <> ["metric" .= metricName m,"output" .= outputName out])
  CellValue d j s -> tagged "cell-value" (cellFields d j s)
  CellEdge d j s -> tagged "cell-edge" (cellFields d j s)
  Constant v -> tagged "constant" ["value" .= v]
  Planar a b -> tagged "planar" ["from" .= a,"to" .= b]
  Distance c r -> tagged "distance" ["centre" .= c,"radius" .= r]
  Angular c a -> tagged "angular" ["centre" .= c,"axis" .= a]
  SdfSphere c r -> tagged "sphere" ["centre" .= c,"radius" .= r]
  SdfBox c h -> tagged "box" ["centre" .= c,"half" .= h]
  SdfCylinder c r h -> tagged "cylinder" ["centre" .= c,"radius" .= r,"height" .= h]
  SdfTorus c r t -> tagged "torus" ["centre" .= c,"major" .= r,"minor" .= t]
  SdfPlane n o -> tagged "plane" ["normal" .= n,"offset" .= o]
  SdfCombine op k a b -> tagged (case op of SdfUnion -> "sdf-union"; SdfIntersection -> "sdf-intersection"; SdfDifference -> "sdf-difference") ["amount" .= k,"a" .= scalarToValue a,"b" .= scalarToValue b]
  Noise -> tagged "noise" []
  Fractal o p l style source -> tagged "fractal" (fractalFields o p l source <> ["style" .= noiseStyleName style])
  AbsoluteFractal o p l source -> tagged "absolute-fractal" (fractalFields o p l source)
  ScalarDomain d source -> tagged "scalar-domain" ["domain" .= domainToValue d,"source" .= scalarToValue source]
  Arithmetic op a b -> tagged (case op of Add -> "add"; Multiply -> "multiply"; Minimum -> "min"; Maximum -> "max") ["a" .= scalarToValue a,"b" .= scalarToValue b]
  Remap lo hi a b source -> tagged "remap" ["low" .= lo,"high" .= hi,"outLow" .= a,"outHigh" .= b,"source" .= scalarToValue source]
  ScalarSin source -> tagged "sin" ["source" .= scalarToValue source]
  ScalarCos source -> tagged "cos" ["source" .= scalarToValue source]
  ScalarAbs source -> tagged "abs" ["source" .= scalarToValue source]
  ScalarFloor source -> tagged "floor" ["source" .= scalarToValue source]
  ScalarFract source -> tagged "fract" ["source" .= scalarToValue source]
  SafeDivide a b -> tagged "divide" ["a" .= scalarToValue a,"b" .= scalarToValue b]
  ScalarPower a b -> tagged "power" ["a" .= scalarToValue a,"b" .= scalarToValue b]
  ScalarLerp a b t -> tagged "lerp" ["a" .= scalarToValue a,"b" .= scalarToValue b,"amount" .= scalarToValue t]
  ScalarClamp lo hi source -> tagged "clamp" ["low" .= lo,"high" .= hi,"source" .= scalarToValue source]
  Azimuth c -> tagged "azimuth" ["centre" .= c]
  VectorComponent i source -> tagged "component" ["axis" .= ((["x","y","z"] :: [Text]) !! max 0 (min 2 i)),"source" .= vectorToValue source]
  Threshold lo hi source -> tagged "threshold" ["low" .= lo,"high" .= hi,"source" .= scalarToValue source]
  where fractalFields o p l source = ["octaves" .= o,"persistence" .= p,"lacunarity" .= l,"source" .= scalarToValue source]

vectorToValue :: Vector -> Value
vectorToValue field = case field of
  CellIdentity d j s -> tagged "cell-id" (cellFields d j s)
  CellColour d j s -> tagged "cell-colour" (cellFields d j s)
  VectorConstant v -> tagged "vector-constant" ["value" .= v]
  Position -> tagged "position" []
  Components x y z -> tagged "components" ["x" .= scalarToValue x,"y" .= scalarToValue y,"z" .= scalarToValue z]
  VectorAdd a b -> tagged "vector-add" ["a" .= vectorToValue a,"b" .= vectorToValue b]
  VectorScale amount source -> tagged "vector-scale" ["amount" .= scalarToValue amount,"source" .= vectorToValue source]
  VectorDomain d source -> tagged "vector-domain" ["domain" .= domainToValue d,"source" .= vectorToValue source]

domainToValue :: Domain -> Value
domainToValue domain = case domain of
  Translate v -> tagged "translate" ["offset" .= v]
  Scale v -> tagged "scale" ["scale" .= v]
  Rotate v -> tagged "rotate" ["rotation" .= v]
  Repeat v -> tagged "repeat" ["period" .= v]
  MirrorDomain centre axes -> tagged "mirror" ["centre" .= centre,"axes" .= axes]
  PolarRepeat centre count -> tagged "polar-repeat" ["centre" .= centre,"count" .= count]
  RadialRepeat centre period -> tagged "radial-repeat" ["centre" .= centre,"period" .= period]
  Twist centre amount -> tagged "twist" ["centre" .= centre,"amount" .= amount]
  Bend centre amount -> tagged "bend" ["centre" .= centre,"amount" .= amount]
  Compose first second -> tagged "compose" ["first" .= domainToValue first,"second" .= domainToValue second]
  Warp amount field -> tagged "warp" ["amount" .= amount,"field" .= vectorToValue field]

parseScalar :: Value -> Parser Scalar
parseScalar = withObject "Scalar field" $ \o -> do
  kind <- o .: "type"
  let child = explicitParseField parseScalar o
      domain = explicitParseField parseDomain o
      vec = explicitParseField parseVec3 o
      n = finiteField o
  case kind :: Text of
    "reaction-diffusion" -> ReactionField <$> parseReaction o <*> explicitParseField parseChemical o "output"
    "worley" -> Worley <$> cellDims o <*> n "jitter" <*> cellSeed o <*> explicitParseField parseMetric o "metric" <*> explicitParseField parseOutput o "output"
    "cell-value" -> CellValue <$> cellDims o <*> n "jitter" <*> cellSeed o
    "cell-edge" -> CellEdge <$> cellDims o <*> n "jitter" <*> cellSeed o
    "constant" -> Constant <$> n "value"
    "planar" -> Planar <$> vec "from" <*> vec "to"
    "distance" -> Distance <$> vec "centre" <*> n "radius"
    "angular" -> Angular <$> vec "centre" <*> vec "axis"
    "sphere" -> SdfSphere <$> vec "centre" <*> n "radius"
    "box" -> SdfBox <$> vec "centre" <*> vec "half"
    "cylinder" -> SdfCylinder <$> vec "centre" <*> n "radius" <*> n "height"
    "torus" -> SdfTorus <$> vec "centre" <*> n "major" <*> n "minor"
    "plane" -> SdfPlane <$> vec "normal" <*> n "offset"
    "sdf-union" -> SdfCombine SdfUnion <$> n "amount" <*> child "a" <*> child "b"
    "sdf-intersection" -> SdfCombine SdfIntersection <$> n "amount" <*> child "a" <*> child "b"
    "sdf-difference" -> SdfCombine SdfDifference <$> n "amount" <*> child "a" <*> child "b"
    "noise" -> pure Noise
    "fractal" -> Fractal <$> o .: "octaves" <*> n "persistence" <*> n "lacunarity" <*> explicitParseField parseNoiseStyle o "style" <*> child "source"
    "absolute-fractal" -> AbsoluteFractal <$> o .: "octaves" <*> n "persistence" <*> n "lacunarity" <*> child "source"
    "scalar-domain" -> ScalarDomain <$> domain "domain" <*> child "source"
    "add" -> Arithmetic Add <$> child "a" <*> child "b"
    "multiply" -> Arithmetic Multiply <$> child "a" <*> child "b"
    "min" -> Arithmetic Minimum <$> child "a" <*> child "b"
    "max" -> Arithmetic Maximum <$> child "a" <*> child "b"
    "remap" -> Remap <$> n "low" <*> n "high" <*> n "outLow" <*> n "outHigh" <*> child "source"
    "sin" -> ScalarSin <$> child "source"
    "cos" -> ScalarCos <$> child "source"
    "abs" -> ScalarAbs <$> child "source"
    "floor" -> ScalarFloor <$> child "source"
    "fract" -> ScalarFract <$> child "source"
    "divide" -> SafeDivide <$> child "a" <*> child "b"
    "power" -> ScalarPower <$> child "a" <*> child "b"
    "lerp" -> ScalarLerp <$> child "a" <*> child "b" <*> child "amount"
    "clamp" -> ScalarClamp <$> n "low" <*> n "high" <*> child "source"
    "azimuth" -> Azimuth <$> vec "centre"
    "component" -> do
      axis <- o .: "axis"
      i <- case (axis :: String) of "x" -> pure 0; "y" -> pure 1; "z" -> pure 2; _ -> fail "Component axis must be x, y or z"
      VectorComponent i <$> explicitParseField parseVector o "source"
    "threshold" -> Threshold <$> n "low" <*> n "high" <*> child "source"
    _ -> fail ("Unknown scalar type " <> show kind)

parseVector :: Value -> Parser Vector
parseVector = withObject "Vector field" $ \o -> do
  kind <- o .: "type"
  let component = explicitParseField parseScalar o
      child = explicitParseField parseVector o
  case kind :: Text of
    "cell-id" -> CellIdentity <$> cellDims o <*> finiteField o "jitter" <*> cellSeed o
    "cell-colour" -> CellColour <$> cellDims o <*> finiteField o "jitter" <*> cellSeed o
    "vector-constant" -> VectorConstant <$> explicitParseField parseVec3 o "value"
    "position" -> pure Position
    "components" -> Components <$> component "x" <*> component "y" <*> component "z"
    "vector-add" -> VectorAdd <$> child "a" <*> child "b"
    "vector-scale" -> VectorScale <$> component "amount" <*> child "source"
    "vector-domain" -> VectorDomain <$> explicitParseField parseDomain o "domain" <*> child "source"
    _ -> fail ("Unknown vector type " <> show kind)

parseDomain :: Value -> Parser Domain
parseDomain = withObject "Domain" $ \o -> do
  kind <- o .: "type"
  case kind :: Text of
    "translate" -> Translate <$> explicitParseField parseVec3 o "offset"
    "scale" -> Scale <$> explicitParseField parseVec3 o "scale"
    "rotate" -> Rotate <$> explicitParseField parseVec3 o "rotation"
    "repeat" -> Repeat <$> explicitParseField parseVec3 o "period"
    "mirror" -> MirrorDomain <$> explicitParseField parseVec3 o "centre" <*> explicitParseField parseVec3 o "axes"
    "polar-repeat" -> PolarRepeat <$> explicitParseField parseVec3 o "centre" <*> o .: "count"
    "radial-repeat" -> RadialRepeat <$> explicitParseField parseVec3 o "centre" <*> finiteField o "period"
    "twist" -> Twist <$> explicitParseField parseVec3 o "centre" <*> finiteField o "amount"
    "bend" -> Bend <$> explicitParseField parseVec3 o "centre" <*> finiteField o "amount"
    "compose" -> Compose <$> explicitParseField parseDomain o "first" <*> explicitParseField parseDomain o "second"
    "warp" -> Warp <$> finiteField o "amount" <*> explicitParseField parseVector o "field"
    _ -> fail ("Unknown domain type " <> show kind)

finiteField :: Object -> Key -> Parser Double
finiteField o key = explicitParseField finite o key
  where finite v = do
          x <- parseJSON v
          if isNaN x || isInfinite x then fail "Expected a finite number" else pure x

cellFields :: Int -> Double -> Int -> [Pair]
cellFields d j s = ["dimensions" .= d,"jitter" .= j,"seed" .= s]
cellDims :: Object -> Parser Int
cellDims o = do
  d <- o .: "dimensions"
  if d==2 || d==3 then pure d else fail "Cellular dimensions must be 2 or 3"
cellSeed :: Object -> Parser Int
cellSeed o = do
  s <- o .: "seed"
  if s>=0 && toInteger s<=4294967295 then pure s else fail "Cellular seed must be an unsigned 32-bit integer"
metricName :: C.Metric -> Text
metricName m = case m of C.Euclidean -> "euclidean"; C.Manhattan -> "manhattan"; C.Chebyshev -> "chebyshev"
outputName :: C.Output -> Text
outputName out = case out of C.F1 -> "f1"; C.F2 -> "f2"; C.Gap -> "gap"
parseMetric :: Value -> Parser C.Metric
parseMetric = withText "Distance metric" $ \m -> case m of
  "euclidean" -> pure C.Euclidean
  "manhattan" -> pure C.Manhattan
  "chebyshev" -> pure C.Chebyshev
  _ -> fail "Unknown distance metric"
parseOutput :: Value -> Parser C.Output
parseOutput = withText "Worley output" $ \out -> case out of
  "f1" -> pure C.F1
  "f2" -> pure C.F2
  "gap" -> pure C.Gap
  _ -> fail "Unknown Worley output"

blendModeName :: BlendMode -> Text
blendModeName mode = case mode of
  NormalBlend -> "normal"; MultiplyBlend -> "multiply"; ScreenBlend -> "screen"
  OverlayBlend -> "overlay"; SoftLightBlend -> "soft-light"; DarkenBlend -> "darken"
  LightenBlend -> "lighten"; DifferenceBlend -> "difference"; ExclusionBlend -> "exclusion"
parseBlendMode :: Value -> Parser BlendMode
parseBlendMode = withText "Blend mode" $ \name ->
  maybe (fail "Unknown blend mode") pure (lookup name [(blendModeName m,m) | m<-[minBound..maxBound]])

reactionFields :: R.Config -> [Pair]
reactionFields c = ["resolution" .= R.resolution c,"iterations" .= R.iterations c,"feed" .= R.feed c,"kill" .= R.kill c,"diffusionU" .= R.diffusionU c,"diffusionV" .= R.diffusionV c,"timeStep" .= R.timeStep c,"seed" .= R.seed c,"initial" .= (case R.initial c of R.NoisePatches -> "noise"; R.SeedSpots -> "spots"; R.SeedSlab -> "slab" :: Text)]
parseReaction :: Object -> Parser R.Config
parseReaction o = do
  c <- R.Config <$> o .: "resolution" <*> o .: "iterations" <*> finiteField o "feed" <*> finiteField o "kill" <*> finiteField o "diffusionU" <*> finiteField o "diffusionV" <*> finiteField o "timeStep" <*> cellSeed o <*> explicitParseField parseInitial o "initial"
  either fail pure (R.validate c)
parseInitial :: Value -> Parser R.Initial
parseInitial = withText "Reaction initial condition" $ \value -> case value of
  "noise" -> pure R.NoisePatches; "spots" -> pure R.SeedSpots; "slab" -> pure R.SeedSlab; _ -> fail "Unknown reaction initial condition"
parseChemical :: Value -> Parser R.Chemical
parseChemical = withText "Reaction chemical" $ \value -> case value of
  "u" -> pure R.U; "v" -> pure R.V; _ -> fail "Unknown reaction chemical"
