{-# LANGUAGE OverloadedStrings #-}

module SchemaSpec (schemaTests) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.Foldable (toList)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Examples (Example (..))
import Schema (Field (..), FieldKind (..), Schema (..), Variant (..), schema)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TextureJson (Document (..), parseRamp, parseTexture, parseScalar, parseVector, parseDomain, textureToValue)

schemaTests :: [Example] -> TestTree
schemaTests examples =
  testGroup
    "Schema"
    [ testGroup "Texture defaults parse and match their fields" (map (defaultMatches parseTexture) (textureVariants schema))
    , testGroup "Scalar defaults" (map (defaultMatches parseScalar) (scalarVariants schema))
    , testGroup "Vector defaults" (map (defaultMatches parseVector) (vectorVariants schema))
    , testGroup "Domain defaults" (map (defaultMatches parseDomain) (domainVariants schema))
    , testGroup "Ramp defaults parse and match their fields" (map (defaultMatches parseRamp) (rampVariants schema))
    , testCase "Variant types are unique" $ do
        let types = map variantType (textureVariants schema)
        assertEqual "texture types" (sort types) (unique (sort types))
    , testGroup "Examples only use fields the schema describes" (map exampleConforms examples)
    ]

unique :: Eq a => [a] -> [a]
unique (x : y : rest)
  | x == y = unique (y : rest)
  | otherwise = x : unique (y : rest)
unique xs = xs

defaultMatches :: (Value -> Parser a) -> Variant -> TestTree
defaultMatches parser variant =
  testCase (T.unpack (variantType variant)) $ do
    let value = variantDefault variant
    either assertFailure (const (pure ())) (parseEither parser value)
    case value of
      Object o -> do
        assertEqual "type tag" (Just (String (variantType variant))) (KeyMap.lookup "type" o)
        assertEqual
          "fields"
          (sort (map fieldKey (variantFields variant)))
          (sort [Key.toText k | k <- KeyMap.keys o, k /= "type"])
      _ -> assertFailure "default is not an object"

exampleConforms :: Example -> TestTree
exampleConforms example =
  testCase (exampleId example) $
    checkNode TextureField (textureToValue (documentTexture (exampleDocument example)))

-- | Walk an encoded value, checking every texture and ramp node against the
-- schema variant named by its type tag.
checkNode :: FieldKind -> Value -> Assertion
checkNode kind value =
  case kind of
    TextureField -> checkVariant (textureVariants schema) value
    ScalarNodeField -> checkVariant (scalarVariants schema) value
    VectorNodeField -> checkVariant (vectorVariants schema) value
    DomainField -> checkVariant (domainVariants schema) value
    RampField -> checkVariant (rampVariants schema) value
    _ -> pure ()

checkVariant :: [Variant] -> Value -> Assertion
checkVariant variants value =
  case value of
    Object o ->
      case KeyMap.lookup "type" o of
        Just (String tag) ->
          case filter ((== tag) . variantType) variants of
            [variant] -> do
              let keys = sort [Key.toText k | k <- KeyMap.keys o, k /= "type"]
              assertEqual ("fields of " <> T.unpack tag) (sort (map fieldKey (variantFields variant))) keys
              mapM_ (checkField o) (variantFields variant)
            _ -> assertFailure ("type " <> show tag <> " is not in the schema")
        _ -> assertFailure "node has no type tag"
    _ -> assertFailure "node is not an object"
  where
    checkField o field =
      case KeyMap.lookup (Key.fromText (fieldKey field)) o of
        Just child -> checkNode (fieldKind field) child >> checkShape (fieldKey field) (fieldKind field) child
        Nothing -> assertFailure ("missing field " <> T.unpack (fieldKey field))

checkShape :: Text -> FieldKind -> Value -> Assertion
checkShape key kind value =
  case kind of
    PointField _ -> pair
    VectorField _ -> pair
    ScalarField _ -> number
    IntField _ _ -> number
    EnumField options ->
      case value of
        String v -> assertBool (T.unpack key <> " option") (v `elem` map fst options)
        _ -> assertFailure (T.unpack key <> " should be a string")
    _ -> pure ()
  where
    pair =
      case value of
        Array items -> assertEqual (T.unpack key <> " length") 3 (length (toList items))
        _ -> assertFailure (T.unpack key <> " should be an array")
    number =
      case value of
        Number _ -> pure ()
        _ -> assertFailure (T.unpack key <> " should be a number")
