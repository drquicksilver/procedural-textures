{-# LANGUAGE OverloadedStrings #-}

-- | Versioned, deterministic build-time data for the static editor.
module EditorAssets (editorAssets, documentVectors, shapeLabel) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Examples (Example(..))
import Gallery (exampleCategories)
import Geometry (Shape(..), shapes, shapeName, shapeSolid)
import GeometryJson (sdfValue)
import RampLibrary (RampLibrary, LibraryRamp(..))
import Resolve (resolveDocument)
import Schema (schema, schemaToValue)
import TextureJson (currentVersion, documentToValue, parseDocument, rampToValue, version2WrappingLibraryRamps)

editorAssets :: RampLibrary -> [Example] -> Value
editorAssets library examples = object
  [ "assetVersion" .= (1 :: Int)
  , "documentVersion" .= currentVersion
  , "exampleCategories" .= [object ["id" .= category, "label" .= label] | (category, label) <- exampleCategories]
  , "schema" .= schemaToValue schema
  , "examples" .= [object ["id" .= exampleId e, "document" .= documentToValue (exampleDocument e)] | e <- examples]
  , "ramps" .= [object ["id" .= libraryRampId r, "name" .= libraryRampName r, "description" .= libraryRampDescription r, "category" .= libraryRampCategory r, "ramp" .= rampToValue (libraryRamp r)] | r <- library]
  , "shapes" .= [object ["id" .= shapeName s, "label" .= shapeLabel s, "solid" .= sdfValue (shapeSolid s)] | s <- shapes]
  , "version2WrappingLibraryRamps" .= version2WrappingLibraryRamps
  ]

-- | Canonical outputs come from the reference parser AND reference resolver.
-- Historical examples exercise versions 1–5; new expressions exercise v5.
-- Targeted cases cover
-- migration defaults, references, malformed data and ignored fields.
documentVectors :: RampLibrary -> [Example] -> Value
documentVectors library examples = object ["cases" .= map fixture inputs]
  where
    fixture (name, input) = object (["name" .= name, "input" .= input] <> case parseDocument input >>= (\d -> resolveDocument library d >> pure d) of
      Right d -> ["output" .= documentToValue d]
      Left e -> ["error" .= e])
    inputs = [(exampleId e <> "-v" <> show version, older version (documentToValue (exampleDocument e))) | e <- examples, version <- (if containsCore (documentToValue (exampleDocument e)) then [5] else [1..5 :: Int])]
      <> [("builtin-wrap", doc 2 (object ["type" .= ("linear" :: String), "from" .= ([0,0] :: [Int]), "to" .= ([1,0] :: [Int]), "ramp" .= object ["type" .= ("builtin" :: String), "name" .= ("rainbow" :: String)]]))
         ,("named-mode", object ["version" .= (2 :: Int), "name" .= ("old autosave" :: String), "ramps" .= object ["shared" .= object ["type" .= ("stops" :: String), "mode" .= ("wrap" :: String), "stops" .= ([] :: [Value])]], "texture" .= object ["type" .= ("perlin" :: String), "scale" .= ([2,8] :: [Int]), "ramp" .= object ["type" .= ("named" :: String), "name" .= ("shared" :: String)]]])
         ,("extra-fields-colour", doc 4 (object ["type" .= ("flat" :: String), "ignored" .= True, "colour" .= ([0.1,0.2,0.3] :: [Double])]))
         ,("bad-texture", doc 4 (object ["type" .= ("unknown" :: String)]))
         ,("bad-colour", doc 4 (object ["type" .= ("flat" :: String), "colour" .= ("#bad" :: String)]))
         ,("bad-coordinate", doc 4 (object ["type" .= ("linear" :: String), "from" .= ([0,0] :: [Int])]))
         ,("missing-ramp", doc 4 (object ["type" .= ("perlin" :: String), "scale" .= ([1,1,1] :: [Int]), "ramp" .= object ["type" .= ("named" :: String), "name" .= ("missing" :: String)]]))
         ,("missing-builtin", doc 4 (object ["type" .= ("perlin" :: String), "scale" .= ([1,1,1] :: [Int]), "ramp" .= object ["type" .= ("builtin" :: String), "name" .= ("missing" :: String)]]))
         ,("bad-scalar-edge", doc 5 (object ["type" .= ("colourise" :: String), "field" .= object ["type" .= ("flat" :: String),"colour" .= ("#ffffff" :: String)],"ramp" .= object ["type" .= ("stops" :: String),"stops" .= ([] :: [Value])]]))
         ,("bad-domain-edge", doc 5 (object ["type" .= ("domain" :: String), "domain" .= object ["type" .= ("noise" :: String)],"base" .= object ["type" .= ("flat" :: String),"colour" .= ("#ffffff" :: String)]]))
         ,("bad-vector-edge", doc 5 (object ["type" .= ("vector-colour" :: String),"field" .= object ["type" .= ("noise" :: String)]]))
         ,("vector-ignored-fields", doc 5 (object ["type" .= ("vector-colour" :: String), "field" .= object ["type" .= ("position" :: String),"ignored" .= True]]))
         ,("future-version", doc 6 Null), ("no-version", object ["name" .= ("bad" :: String)]), ("not-object", Null)]
      <> [(name, object ["version" .= (5 :: Int), "name" .= ("guide" :: String), "texture" .= object ["type" .= ("flat" :: String), "colour" .= ("#ffffff" :: String)], "guide" .= guide])
         | (name, guide) <-
           [("guide-defaults", object ["role" .= ("study" :: String)])
           ,("guide-bad-role", object ["role" .= ("unknown" :: String)])
           ,("guide-bad-tags", object ["role" .= ("study" :: String), "tags" .= ([1 :: Int])])
           ,("guide-bad-order", object ["role" .= ("study" :: String), "order" .= (-1 :: Int)])
           ,("guide-bad-axis", object ["role" .= ("study" :: String), "preview" .= object ["axis" .= ("zz" :: String)]])
           ,("guide-bad-position", object ["role" .= ("study" :: String), "preview" .= object ["position" .= (3 :: Int)]])]]
    containsCore (Object o) = maybe False (\v -> v `elem` map String ["colourise","domain","mix","vector-colour","blend"]) (KM.lookup "type" o) || any containsCore (KM.elems o)
    containsCore (Array a) = any containsCore a
    containsCore _ = False
    doc version texture = object ["version" .= (version :: Int), "name" .= ("fixture" :: String), "texture" .= texture]
    older version (Object o) = Object (KM.insert "version" (Number (fromIntegral version)) (if version >= 4 then o else KM.insert "texture" (lower version (maybe Null id (KM.lookup "texture" o))) o))
    older _ v = v
    lower version (Object o) = Object $ KM.mapWithKey (\key value ->
      if key `elem` ["from","to","centre","scale"] then case value of Array a -> Array (V.take 2 a); _ -> value
      else if key == "ramp" && version <= 2 then case value of Object r -> Object (KM.insert "mode" (maybe (String "clamp") id (KM.lookup "mode" o)) r); _ -> value
      else if key `elem` ["base","top","bottom","a","b"] then lower version value else value) (KM.delete "axis" (KM.delete "depth" o))
    lower _ v = v

shapeLabel :: Shape -> String
shapeLabel shape = case shape of
  Ball -> "Sphere"
  Cube -> "Cube"
  Tube -> "Cylinder"
  Ring -> "Torus"
  BittenCube -> "Cube with spherical bite"
  CutSphere -> "Sphere with octant removed"
  CutCube -> "Cube cut by a plane"
  Pawn -> "Chess pawn"
  Rook -> "Chess rook"
  Knight -> "Chess knight"
  Bishop -> "Chess bishop"
  Queen -> "Chess queen"
  King -> "Chess king"
