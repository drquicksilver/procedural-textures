{-# LANGUAGE OverloadedStrings #-}

module ServerSpec (serverTests) where

import Control.Monad.IO.Class (liftIO)
import Codec.Picture (DynamicImage, decodePng, dynamicMap, imageHeight, imageWidth)
import Data.Aeson (Value (..), decode, encode)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Examples (Example (..))
import Network.HTTP.Types (methodGet, methodPost)
import Network.Wai (Application, defaultRequest, requestMethod)
import Network.Wai.Test
  ( SRequest (..)
  , SResponse (..)
  , Session
  , assertHeader
  , assertStatus
  , runSession
  , setPath
  , srequest
  )
import Server (ServerConfig (..), defaultServerConfig, serverApp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import TextureJson (Document, documentToValue)

serverTests :: [Example] -> TestTree
serverTests examples =
  testGroup
    "Server"
    [ testCase "GET /api/schema returns the schema" $
        withApp config $ do
          response <- get "/api/schema"
          assertStatus 200 response
          assertHeader "Content-Type" "application/json; charset=utf-8" response
          assertJsonHas "texture" response
    , testCase "GET /api/examples lists every example" $
        withApp config $ do
          response <- get "/api/examples"
          assertStatus 200 response
          case decode (simpleBody response) of
            Just (Array items) -> liftAssert (assertEqual "count" (length examples) (length items))
            _ -> liftAssert (assertFailure "expected a JSON array")
    , testCase "POST /api/render returns a PNG of the requested size" $
        withApp config $ do
          response <- post "/api/render?size=16" (encodeDocument sample)
          assertStatus 200 response
          assertHeader "Content-Type" "image/png" response
          liftAssert $
            case decodePng (BL.toStrict (simpleBody response)) of
              Left err -> assertFailure err
              Right image -> assertEqual "size" (16, 16) (dimensions image)
    , testCase "POST /api/render rejects bad sizes" $
        withApp config $ do
          post "/api/render?size=0" (encodeDocument sample) >>= assertStatus 400
          post "/api/render?size=abc" (encodeDocument sample) >>= assertStatus 400
          post "/api/render?size=5000" (encodeDocument sample) >>= assertStatus 400
    , testCase "POST /api/render explains malformed documents" $
        withApp config $ do
          response <- post "/api/render" "{\"version\": 1, \"name\": \"x\", \"texture\": {\"type\": \"nope\"}}"
          assertStatus 400 response
          assertJsonHas "error" response
    , testCase "POST /api/render gives up after the timeout" $
        withApp config {configTimeoutMicros = 1} $
          post "/api/render?size=1024" (encodeDocument sample) >>= assertStatus 503
    , testCase "POST /api/render rejects oversized bodies" $
        withApp config {configMaxBodyBytes = 10} $
          post "/api/render" (encodeDocument sample) >>= assertStatus 413
    , testCase "POST /api/migrate returns the canonical document" $
        withApp config $ do
          response <- post "/api/migrate" (encodeDocument sample)
          assertStatus 200 response
          liftAssert (assertEqual "document" (Just (documentToValue sample)) (decode (simpleBody response)))
    , testCase "Unknown API paths are JSON 404s" $
        withApp config $ do
          response <- get "/api/nope"
          assertStatus 404 response
          assertJsonHas "error" response
    , testCase "Without a built frontend, other paths explain what to do" $
        withApp config $ do
          response <- get "/"
          assertStatus 404 response
          liftAssert (assertBool "mentions make" ("make" `B.isInfixOf` BL.toStrict (simpleBody response)))
    ]
  where
    config = defaultServerConfig {configStaticDir = "does-not-exist"}
    sample =
      case filter ((== "marble") . exampleId) examples of
        example : _ -> exampleDocument example
        [] -> error "the marble example is missing"

dimensions :: DynamicImage -> (Int, Int)
dimensions image =
  (dynamicMap imageWidth image, dynamicMap imageHeight image)

encodeDocument :: Document -> BL.ByteString
encodeDocument =
  encode . documentToValue

withApp :: ServerConfig -> Session a -> IO a
withApp config session = do
  app <- serverApp config
  runSession session (app :: Application)

get :: B.ByteString -> Session SResponse
get path =
  srequest (SRequest (setPath defaultRequest {requestMethod = methodGet} path) "")

post :: B.ByteString -> BL.ByteString -> Session SResponse
post path body =
  srequest (SRequest (setPath defaultRequest {requestMethod = methodPost} path) body)

assertJsonHas :: KeyMap.Key -> SResponse -> Session ()
assertJsonHas key response =
  liftAssert $
    case decode (simpleBody response) of
      Just (Object o) -> assertBool ("has " <> show key) (KeyMap.member key o)
      _ -> assertFailure ("expected a JSON object, got " <> show (simpleBody response))

liftAssert :: IO () -> Session ()
liftAssert = liftIO
