{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The editor's backend. Everything under @/api@ is JSON or PNG; every other
-- path is served from the built frontend.
--
-- * @GET /api/schema@: the texture language description (see "Schema").
-- * @GET /api/examples@: @[{"id": ..., "document": ...}]@, read from disk on
--   each request so edits to the example files show up immediately.
-- * @POST /api/render?size=N@: a document in the body, a PNG back.
-- * @POST /api/migrate@: a document of any supported version in the body, the
--   same document in canonical current form back.
--
-- Errors are @{"error": "..."}@ with a 4xx or 5xx status.
module Server
  ( ServerConfig (..)
  , defaultServerConfig
  , serverApp
  , renderPng
  ) where

import Codec.Picture (encodePng)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, eitherDecode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Examples (Example (..), loadExamples)
import Network.HTTP.Types.Status (Status, badRequest400, requestEntityTooLarge413, serviceUnavailable503, internalServerError500, notFound404)
import qualified Data.ByteString as B
import Network.Wai (Application, RequestBodyLength (KnownLength), pathInfo, requestBodyLength, responseLBS)
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import Render (renderImage)
import Schema (schema, schemaToValue)
import System.Directory (doesDirectoryExist)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Texture (Texture, textureToImageFn)
import TextureJson (Document (..), documentToValue, parseDocument)
import Web.Scotty (ActionM, ScottyM, bodyReader, finish, get, json, notFound, post, queryParams, raw, request, scottyApp, setHeader, status)

data ServerConfig = ServerConfig
  { configExamplesDir :: FilePath
  , configStaticDir :: FilePath
  , configMaxSize :: Int
  -- ^ Largest width/height a render request may ask for.
  , configDefaultSize :: Int
  , configTimeoutMicros :: Int
  , configMaxBodyBytes :: Word64
  -- ^ Larger request bodies are refused while being read, not after.
  }

defaultServerConfig :: ServerConfig
defaultServerConfig =
  ServerConfig
    { configExamplesDir = "examples"
    , configStaticDir = "frontend/dist"
    , configMaxSize = 1024
    , configDefaultSize = 256
    , configTimeoutMicros = 10 * 1000 * 1000
    , configMaxBodyBytes = 1024 * 1024
    }

serverApp :: ServerConfig -> IO Application
serverApp config = do
  api <- scottyApp (routes config)
  hasStatic <- doesDirectoryExist (configStaticDir config)
  let static =
        if hasStatic
          then staticApp (defaultFileServerSettings (configStaticDir config))
          else \_ respond -> respond (responseLBS notFound404 [("Content-Type", "text/plain")] (missingFrontend config))
  pure $ \req respond ->
    case pathInfo req of
      "api" : _ -> api req respond
      _ -> static req respond

missingFrontend :: ServerConfig -> BL.ByteString
missingFrontend config =
  BL.fromStrict . TE.encodeUtf8 $
    "The frontend has not been built (no " <> T.pack (configStaticDir config) <> " directory).\n"
      <> "Run `make app`, or `make dev` while working on the frontend.\n"

routes :: ServerConfig -> ScottyM ()
routes config = do
  get "/api/schema" $
    json (schemaToValue schema)

  get "/api/examples" $ do
    loaded <- liftIO (try (loadExamples (configExamplesDir config)))
    case loaded of
      Left err -> failWith internalServerError500 (T.pack (show (err :: SomeException)))
      Right examples ->
        json
          [ object ["id" .= exampleId example, "document" .= documentToValue (exampleDocument example)]
          | example <- examples
          ]

  post "/api/render" $ do
    size <- sizeParam config
    document <- documentBody config
    rendered <- liftIO (timeout (configTimeoutMicros config) (forcePng (renderPng size (documentTexture document))))
    case rendered of
      Nothing -> failWith serviceUnavailable503 "Rendering took too long"
      Just png -> do
        setHeader "Content-Type" "image/png"
        setHeader "Cache-Control" "no-store"
        raw png

  post "/api/migrate" $ do
    document <- documentBody config
    json (documentToValue document)

  notFound $
    failWith notFound404 "No such endpoint"

renderPng :: Int -> Texture -> BL.ByteString
renderPng size texture =
  encodePng (renderImage size size (textureToImageFn texture))

forcePng :: BL.ByteString -> IO BL.ByteString
forcePng png = do
  _ <- evaluate (BL.length png)
  pure png

sizeParam :: ServerConfig -> ActionM Int
sizeParam config = do
  params <- queryParams
  case lookup "size" params of
    Nothing -> pure (configDefaultSize config)
    Just text ->
      case readMaybe (T.unpack text) of
        Just size
          | size >= 1 && size <= configMaxSize config -> pure size
        _ ->
          failWith badRequest400 ("size must be a whole number from 1 to " <> T.pack (show (configMaxSize config)))

documentBody :: ServerConfig -> ActionM Document
documentBody config = do
  bytes <- limitedBody (configMaxBodyBytes config)
  case eitherDecode bytes >>= parseDocument of
    Left err -> failWith badRequest400 (T.pack err)
    Right document -> pure document

-- | The request body, refused with a 413 once it is known to exceed the
-- limit: at once if its declared length is too big, otherwise as soon as
-- enough has been read, so an oversized body is never read in full.
limitedBody :: Word64 -> ActionM BL.ByteString
limitedBody limit = do
  declared <- requestBodyLength <$> request
  case declared of
    KnownLength len | len > limit -> tooLarge
    _ -> do
      readChunk <- bodyReader
      chunks <- liftIO (readUpTo readChunk)
      maybe tooLarge (pure . BL.fromChunks) chunks
  where
    tooLarge = failWith requestEntityTooLarge413 "Document is too large"
    readUpTo readChunk = go 0 []
      where
        go total acc = do
          chunk <- readChunk
          let total' = total + fromIntegral (B.length chunk)
          if
            | B.null chunk -> pure (Just (reverse acc))
            | total' > limit -> pure Nothing
            | otherwise -> go total' (chunk : acc)

failWith :: Status -> Text -> ActionM a
failWith code message = do
  status code
  json (object ["error" .= message] :: Value)
  finish
