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
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Examples (Example (..), loadExamples)
import Network.HTTP.Types.Status (Status, badRequest400, requestEntityTooLarge413, serviceUnavailable503, internalServerError500, notFound404)
import Network.Wai (Application, pathInfo, responseLBS)
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import Render (renderImage)
import Schema (schema, schemaToValue)
import System.Directory (doesDirectoryExist)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Texture (Texture, textureToImageFn)
import TextureJson (Document (..), documentToValue, parseDocument)
import Web.Scotty (ActionM, ScottyM, body, finish, get, json, notFound, post, queryParams, raw, scottyApp, setHeader, status)

data ServerConfig = ServerConfig
  { configExamplesDir :: FilePath
  , configStaticDir :: FilePath
  , configMaxSize :: Int
  -- ^ Largest width/height a render request may ask for.
  , configDefaultSize :: Int
  , configTimeoutMicros :: Int
  , configMaxBodyBytes :: Int64
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
  pure $ \request respond ->
    case pathInfo request of
      "api" : _ -> api request respond
      _ -> static request respond

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
  bytes <- body
  if BL.length bytes > configMaxBodyBytes config
    then failWith requestEntityTooLarge413 "Document is too large"
    else case eitherDecode bytes >>= parseDocument of
      Left err -> failWith badRequest400 (T.pack err)
      Right document -> pure document

failWith :: Status -> Text -> ActionM a
failWith code message = do
  status code
  json (object ["error" .= message] :: Value)
  finish
