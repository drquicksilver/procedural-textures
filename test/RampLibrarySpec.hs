{-# LANGUAGE OverloadedStrings #-}

module RampLibrarySpec (rampLibraryTests) where

import ColourRamps (ColourRamp (..), RampMode (..))
import qualified Data.ByteString.Lazy as BL
import Data.List (isInfixOf, nub)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import RampLibrary (LibraryRamp (..), RampLibrary, defaultRampsDirectory, libraryRampToValue)
import Resolve (resolveDocument)
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Texture (Texture (..))
import TextureJson (Document (..), encodeValuePretty, simpleDocument)

rampLibraryTests :: RampLibrary -> TestTree
rampLibraryTests library =
  testGroup
    "Ramp library"
    [ testCase "Library ramps have unique ids, names and known categories" $ do
        assertBool "not empty" (not (null library))
        assertEqual "ids" (length library) (length (nub (map libraryRampId library)))
        assertEqual "names" (length library) (length (nub (map libraryRampName library)))
        mapM_ (\r -> assertBool (T.unpack (libraryRampId r) <> " category") (libraryRampCategory r `elem` categories)) library
    , testGroup "Ramp files are canonically formatted" (map canonical library)
    , testCase "Named and library references resolve" $ do
        let document =
              (simpleDocument "x" (Layer (Perlin (1, 1) (NamedRamp "mine")) (Perlin (1, 1) (BuiltinRamp "greyscale"))))
                { documentRamps = Map.fromList [("mine", red)]
                }
        case resolveDocument library document of
          Right (Layer (Perlin _ top) (Perlin _ bottom)) -> do
            assertEqual "named" red top
            assertBool "builtin is concrete" (case bottom of Ramp _ _ -> True; _ -> False)
          other -> assertFailure (show other)
    , testCase "Missing references are reported with their path" $ do
        let document = simpleDocument "x" (Layer (Flat (0, 0, 0, 1)) (Perlin (1, 1) (NamedRamp "nope")))
        case resolveDocument library document of
          Left err -> assertBool err ("$.texture.bottom.ramp" `isInfixOf` err && "nope" `isInfixOf` err)
          Right _ -> assertFailure "expected an error"
        case resolveDocument library (simpleDocument "x" (Perlin (1, 1) (BuiltinRamp "nope"))) of
          Left err -> assertBool err ("library ramp" `isInfixOf` err)
          Right _ -> assertFailure "expected an error"
    ]
  where
    red = Ramp Clamp [(0, (1, 0, 0, 1))]
    categories = ["scientific", "natural", "sky", "fire", "water", "utility", "decorative"]

canonical :: LibraryRamp -> TestTree
canonical ramp =
  testCase (T.unpack (libraryRampId ramp)) $ do
    bytes <- BL.readFile (defaultRampsDirectory </> T.unpack (libraryRampId ramp) <.> "json")
    assertBool
      "not in canonical form; run `stack run procedural-textures -- format ramps/*.json`"
      (bytes == encodeValuePretty (libraryRampToValue ramp))
