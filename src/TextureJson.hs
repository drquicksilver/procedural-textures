{-# LANGUAGE OverloadedStrings #-}

-- | JSON encoding of textures and texture documents.
--
-- A document is the unit that is saved, loaded and sent to the server:
--
-- > {"version": 1, "name": "Marble", "description": "...", "texture": {...}}
--
-- Textures and ramps are tagged objects (@{"type": "linear", ...}@). Colours
-- are written as @"#rrggbbaa"@ hex strings when they are exactly representable
-- with 8-bit channels and as @[r, g, b, a]@ arrays of numbers in [0, 1]
-- otherwise, so encoding never loses precision. Both forms (and @"#rrggbb"@)
-- are accepted when parsing.
module TextureJson
  ( Document (..)
  , simpleDocument
  , currentVersion
  , documentToValue
  , parseDocument
  , decodeDocument
  , encodeDocumentPretty
  , encodeValuePretty
  , migrateDocument
  , textureToValue
  , parseTexture
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
import Data.Aeson.Types (JSONPathElement (Index, Key), Key, Parser, explicitParseField, explicitParseFieldMaybe, parseEither, typeMismatch, (<?>))
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
import Texture (NoiseStyle (..), Texture (..))

data Document = Document
  { documentName :: Text
  , documentDescription :: Text
  , documentCategory :: Text
  -- ^ Groups examples ("natural", "pattern", ...); empty if none.
  , documentRamps :: Map Text ColourRamp
  -- ^ Named ramps, referred to from the texture as 'NamedRamp'. Definitions
  -- are always concrete: never themselves references.
  , documentTexture :: Texture
  }
  deriving (Eq, Show)

-- | A document with just a name and a texture.
simpleDocument :: Text -> Texture -> Document
simpleDocument name =
  Document name "" "" Map.empty

-- | Version history:
--
-- 1. The first format.
-- 2. Adds named ramps (a @ramps@ map and @named@ references to it),
--    @builtin@ references to the ramp library, and @category@.
currentVersion :: Int
currentVersion = 2

documentToValue :: Document -> Value
documentToValue document =
  object
    ( [ "version" .= currentVersion
      , "name" .= documentName document
      , "description" .= documentDescription document
      ]
        <> ["category" .= documentCategory document | not (T.null (documentCategory document))]
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
          | n == 1 -> migrateDocument (Object (KeyMap.insert "version" (Number 2) o))
          | otherwise -> Left ("Unknown document version " <> show n)
        Just _ -> Left "Document \"version\" must be a number"
    _ -> Left "Document must be a JSON object"

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
  [ "version", "name", "description", "category", "ramps", "texture", "type"
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
    Linear from to ramp ->
      tagged "linear" ["from" .= from, "to" .= to, "ramp" .= rampToValue ramp]
    Radial centre ramp ->
      tagged "radial" ["centre" .= centre, "ramp" .= rampToValue ramp]
    Circular centre radius ramp ->
      tagged "circular" ["centre" .= centre, "radius" .= radius, "ramp" .= rampToValue ramp]
    Perlin scale ramp ->
      tagged "perlin" ["scale" .= scale, "ramp" .= rampToValue ramp]
    Fbm scale octaves persistence lacunarity style ramp ->
      tagged
        "fbm"
        [ "scale" .= scale
        , "octaves" .= octaves
        , "persistence" .= persistence
        , "lacunarity" .= lacunarity
        , "style" .= noiseStyleName style
        , "ramp" .= rampToValue ramp
        ]
    Turbulence amount octaves persistence lacunarity base ->
      tagged
        "turbulence"
        [ "amount" .= amount
        , "octaves" .= octaves
        , "persistence" .= persistence
        , "lacunarity" .= lacunarity
        , "base" .= textureToValue base
        ]
    Tiled columns rows a b ->
      tagged "tiled" ["columns" .= columns, "rows" .= rows, "a" .= textureToValue a, "b" .= textureToValue b]
    Layer top bottom ->
      tagged "layer" ["top" .= textureToValue top, "bottom" .= textureToValue bottom]

parseTexture :: Value -> Parser Texture
parseTexture =
  withObject "Texture" $ \o -> do
    kind <- o .: "type"
    case kind :: Text of
      "flat" -> Flat <$> explicitParseField parseColour o "colour"
      "linear" -> Linear <$> o .: "from" <*> o .: "to" <*> ramp o
      "radial" -> Radial <$> o .: "centre" <*> ramp o
      "circular" -> Circular <$> o .: "centre" <*> o .: "radius" <*> ramp o
      "perlin" -> Perlin <$> o .: "scale" <*> ramp o
      "fbm" ->
        Fbm
          <$> o .: "scale"
          <*> o .: "octaves"
          <*> o .: "persistence"
          <*> o .: "lacunarity"
          <*> explicitParseField parseNoiseStyle o "style"
          <*> ramp o
      "turbulence" ->
        Turbulence
          <$> o .: "amount"
          <*> o .: "octaves"
          <*> o .: "persistence"
          <*> o .: "lacunarity"
          <*> child o "base"
      "tiled" -> Tiled <$> o .: "columns" <*> o .: "rows" <*> child o "a" <*> child o "b"
      "layer" -> Layer <$> child o "top" <*> child o "bottom"
      _ -> fail ("Unknown texture type " <> show kind)
  where
    ramp o = explicitParseField parseRamp o "ramp"
    child o key = explicitParseField parseTexture o key

rampToValue :: ColourRamp -> Value
rampToValue ramp =
  case ramp of
    Ramp mode stops ->
      tagged
        "stops"
        [ "mode" .= modeName mode
        , "stops" .= [object ["position" .= position, "colour" .= colourToValue colour] | (position, colour) <- stops]
        ]
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
        Ramp
          <$> explicitParseField parseMode o "mode"
          <*> explicitParseField (withArray "stops" (zipWithM indexed [0 ..] . toList)) o "stops"
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
