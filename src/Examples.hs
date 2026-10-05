-- | The shipped example textures, stored as JSON documents in @examples/@.
-- Those files are the source of truth for the examples.
module Examples
  ( Example (..)
  , defaultExamplesDirectory
  , loadExamples
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.List (sort)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import TextureJson (Document, decodeDocument)

data Example = Example
  { exampleId :: String
  -- ^ The file's base name, e.g. @"marble"@ for @examples/marble.json@.
  , exampleDocument :: Document
  }
  deriving (Eq, Show)

defaultExamplesDirectory :: FilePath
defaultExamplesDirectory = "examples"

-- | Load every @*.json@ document in a directory, ordered by file name. Fails
-- with the file name and the parse error if any document is invalid.
loadExamples :: FilePath -> IO [Example]
loadExamples directory = do
  names <- sort . filter ((== ".json") . takeExtension) <$> listDirectory directory
  mapM load names
  where
    load name = do
      let path = directory </> name
      bytes <- BL.readFile path
      case decodeDocument bytes of
        Left err -> fail (path <> ": " <> err)
        Right document -> pure (Example (takeBaseName name) document)
