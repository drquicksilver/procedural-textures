{-# LANGUAGE OverloadedStrings #-}

module TextureJsonSpec (textureJsonTests) where

import ColourRamps (ColourRamp (..), RampMode (..))
import Data.Aeson (Value (..), encode)
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Examples (Example (..), defaultExamplesDirectory)
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Texture (NoiseStyle (..), Texture (..))
import TextureJson
  ( Document (..)
  , simpleDocument
  , colourToValue
  , decodeDocument
  , documentToValue
  , encodeDocumentPretty
  , parseColour
  , parseDocument
  )

textureJsonTests :: [Example] -> TestTree
textureJsonTests examples =
  testGroup
    "TextureJson"
    [ testGroup "Examples round-trip" (map roundTrip examples)
    , testGroup "Example files are canonically formatted" (map canonical examples)
    , testCase "Byte-exact colours encode as hex" $
        assertEqual "hex" (String "#ff800000") (colourToValue (1.0, 128 / 255, 0.0, 0.0))
    , testCase "Other colours encode as arrays and round-trip" $ do
        let colour = (0.15, 0.2, 0.6, 1.0)
        assertEqual "round trip" (Right colour) (parseEither parseColour (colourToValue colour))
    , testCase "Six-digit hex colours are opaque" $
        assertEqual "opaque" (Right (1.0, 0.0, 0.0, 1.0)) (parseEither parseColour (String "#ff0000"))
    , testCase "Malformed hex colours are rejected" $ do
        assertLeft (parseEither parseColour (String "#ff00"))
        assertLeft (parseEither parseColour (String "ff0000"))
        assertLeft (parseEither parseColour (String "#gg0000"))
    , testCase "Description is optional" $
        assertEqual
          "no description"
          (Right (simpleDocument "Plain" (Flat (0.0, 0.0, 0.0, 1.0))))
          (decodeDocument "{\"version\": 1, \"name\": \"Plain\", \"texture\": {\"type\": \"flat\", \"colour\": \"#000000\"}}")
    , testCase "Errors name the path to the bad value" $
        assertErrorContains
          "$.texture.top"
          (decodeDocument "{\"version\": 1, \"name\": \"x\", \"texture\": {\"type\": \"layer\", \"top\": {\"type\": \"nope\"}, \"bottom\": {\"type\": \"flat\", \"colour\": \"#000000\"}}}")
    , testCase "Errors name the index of a bad stop" $
        assertErrorContains
          "$.texture.ramp.stops[1].colour"
          (decodeDocument "{\"version\": 1, \"name\": \"x\", \"texture\": {\"type\": \"perlin\", \"scale\": [1, 1], \"ramp\": {\"type\": \"stops\", \"mode\": \"clamp\", \"stops\": [{\"position\": 0, \"colour\": \"#000000\"}, {\"position\": 1, \"colour\": \"#12\"}]}}}")
    , testCase "Errors name bad ramp modes" $
        assertErrorContains
          "Unknown ramp mode"
          (decodeDocument (encode (documentToValue sample) `replaceMode` "sideways"))
    , testCase "Missing version is rejected" $
        assertErrorContains "version" (parseDocument (Object mempty))
    , testCase "Version 1 documents migrate to the current version" $ do
        assertEqual
          "migrated"
          (Right (simpleDocument "Old" (Flat (0.0, 0.0, 0.0, 1.0))))
          (decodeDocument "{\"version\": 1, \"name\": \"Old\", \"texture\": {\"type\": \"flat\", \"colour\": \"#000000\"}}")
    , testCase "Fractal noise round-trips, and bad styles are named" $ do
        let document = simpleDocument "Fbm" (Fbm (3, 5) 6 0.45 2.2 Ridged (BuiltinRamp "terrain"))
        assertEqual "round trip" (Right document) (decodeDocument (encode (documentToValue document)))
        assertErrorContains
          "Unknown noise style"
          (decodeDocument "{\"version\": 2, \"name\": \"x\", \"texture\": {\"type\": \"fbm\", \"scale\": [1, 1], \"octaves\": 3, \"persistence\": 0.5, \"lacunarity\": 2, \"style\": \"lumpy\", \"ramp\": {\"type\": \"builtin\", \"name\": \"greyscale\"}}}")
    , testCase "Named ramps, references and categories round-trip" $ do
        let document =
              (simpleDocument "Refs" (Layer (Perlin (1, 2) (NamedRamp "eye")) (Perlin (3, 4) (BuiltinRamp "viridis"))))
                { documentCategory = "natural"
                , documentRamps = Map.fromList [("eye", Ramp Wrap [(0.0, (1.0, 0.0, 0.0, 1.0))])]
                }
        assertEqual "round trip" (Right document) (decodeDocument (encode (documentToValue document)))
    , testCase "Named ramp definitions cannot themselves be references" $
        assertErrorContains
          "$.ramps.a"
          (decodeDocument "{\"version\": 2, \"name\": \"x\", \"ramps\": {\"a\": {\"type\": \"named\", \"name\": \"b\"}}, \"texture\": {\"type\": \"flat\", \"colour\": \"#000000\"}}")
    , testCase "Newer versions are rejected" $
        assertErrorContains "newer" (decodeDocument "{\"version\": 99, \"name\": \"x\", \"texture\": {\"type\": \"flat\", \"colour\": \"#000000\"}}")
    ]

sample :: Document
sample =
  simpleDocument "Sample" (Linear (0.0, 0.5) (1.0, 0.5) (Ramp Clamp [(0.0, (1.0, 0.0, 0.0, 1.0)), (1.0, (0.0, 0.0, 1.0, 1.0))]))

-- | Swap the "clamp" mode in an encoded document for another word.
replaceMode :: BL.ByteString -> String -> BL.ByteString
replaceMode bytes mode =
  BLC.pack (go (BLC.unpack bytes))
  where
    go text
      | "\"clamp\"" `isPrefix` text = "\"" <> mode <> "\"" <> go (drop 7 text)
    go (c : rest) = c : go rest
    go [] = []
    isPrefix prefix text = take (length prefix) text == prefix

roundTrip :: Example -> TestTree
roundTrip example =
  testCase (exampleId example) $
    assertEqual
      "decode . encode"
      (Right (exampleDocument example))
      (decodeDocument (encode (documentToValue (exampleDocument example))))

canonical :: Example -> TestTree
canonical example =
  testCase (exampleId example) $ do
    bytes <- BL.readFile (defaultExamplesDirectory </> exampleId example <.> "json")
    assertBool
      "file is not in canonical form; run `stack run procedural-textures -- format examples/*.json`"
      (bytes == encodeDocumentPretty (exampleDocument example))

assertLeft :: Show a => Either String a -> Assertion
assertLeft result =
  case result of
    Left _ -> pure ()
    Right value -> assertFailure ("Expected an error, got " <> show value)

assertErrorContains :: Show a => String -> Either String a -> Assertion
assertErrorContains needle result =
  case result of
    Left err -> assertBool ("error " <> show err <> " should mention " <> show needle) (needle `isInfixOf` err)
    Right value -> assertFailure ("Expected an error, got " <> show value)
