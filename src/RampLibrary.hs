{-# LANGUAGE OverloadedStrings #-}

-- | The built-in ramp library: read-only ramps shipped in @ramps/*.json@,
-- referred to from documents as @{"type": "builtin", "name": <file name>}@.
-- Like the examples, the files are the source of truth.
--
-- > {"version": 1, "name": "Viridis", "description": "...", "category": "scientific", "ramp": {...}}
module RampLibrary
  ( LibraryRamp (..)
  , RampLibrary
  , defaultRampsDirectory
  , loadRampLibrary
  , lookupLibraryRamp
  , libraryRampToValue
  , parseLibraryRamp
  ) where

import ColourRamps (ColourRamp (..))
import Data.Aeson (Value, eitherDecode, object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, explicitParseField, parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.List (find, sort)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import TextureJson (parseRamp, rampToValue)

data LibraryRamp = LibraryRamp
  { libraryRampId :: Text
  -- ^ The file's base name, which is how documents refer to it.
  , libraryRampName :: Text
  , libraryRampDescription :: Text
  , libraryRampCategory :: Text
  , libraryRamp :: ColourRamp
  }
  deriving (Eq, Show)

type RampLibrary = [LibraryRamp]

defaultRampsDirectory :: FilePath
defaultRampsDirectory = "ramps"

-- | Load every @*.json@ ramp in a directory, ordered by file name. Fails with
-- the file name and the parse error if any is invalid.
loadRampLibrary :: FilePath -> IO RampLibrary
loadRampLibrary directory = do
  names <- sort . filter ((== ".json") . takeExtension) <$> listDirectory directory
  mapM load names
  where
    load name = do
      let path = directory </> name
      bytes <- BL.readFile path
      case eitherDecode bytes >>= parseEither (parseLibraryRamp (T.pack (takeBaseName name))) of
        Left err -> fail (path <> ": " <> err)
        Right ramp -> pure ramp

lookupLibraryRamp :: RampLibrary -> Text -> Maybe ColourRamp
lookupLibraryRamp library name =
  libraryRamp <$> find ((== name) . libraryRampId) library

parseLibraryRamp :: Text -> Value -> Parser LibraryRamp
parseLibraryRamp rampId =
  withObject "LibraryRamp" $ \o -> do
    version <- o .: "version"
    if version /= (1 :: Int)
      then fail ("Unsupported ramp file version " <> show version)
      else do
        ramp <- explicitParseField parseRamp o "ramp"
        case ramp of
          NamedRamp _ -> fail "Library ramps must be concrete ramps, not references"
          BuiltinRamp _ -> fail "Library ramps must be concrete ramps, not references"
          _ -> LibraryRamp rampId <$> o .: "name" <*> o .: "description" <*> o .: "category" <*> pure ramp

-- | The file format (without the id, which is the file name).
libraryRampToValue :: LibraryRamp -> Value
libraryRampToValue ramp =
  object
    [ "version" .= (1 :: Int)
    , "name" .= libraryRampName ramp
    , "description" .= libraryRampDescription ramp
    , "category" .= libraryRampCategory ramp
    , "ramp" .= rampToValue (libraryRamp ramp)
    ]
