{-# LANGUAGE OverloadedStrings #-}

-- | Replacing ramp references with their definitions, so a texture can be
-- rendered. Named ramps come from the document's own @ramps@ map, built-in
-- ones from the ramp library.
module Resolve
  ( resolveDocument
  , resolveTexture
  , resolveRamp
  ) where

import ColourRamps (ColourRamp (..))
import qualified Data.Map.Strict as Map
import RampLibrary (RampLibrary, lookupLibraryRamp)
import Texture (Texture (..))
import TextureJson (Document (..))

-- | The document's texture with every ramp reference replaced, or an error
-- naming the JSON path of the first reference that cannot be found.
resolveDocument :: RampLibrary -> Document -> Either String Texture
resolveDocument library document =
  resolveTexture lookupRamp "$.texture" (documentTexture document)
  where
    lookupRamp ramp =
      case ramp of
        NamedRamp name -> found ("named ramp " <> show name <> " (not defined in this document's \"ramps\")") (Map.lookup name (documentRamps document))
        BuiltinRamp name -> found ("library ramp " <> show name) (lookupLibraryRamp library name)
        _ -> Right ramp
    found what = maybe (Left ("Unknown " <> what)) Right

resolveTexture :: (ColourRamp -> Either String ColourRamp) -> String -> Texture -> Either String Texture
resolveTexture lookupRamp path texture =
  case texture of
    VectorColour v -> pure (VectorColour v)
    Colourise field mode ramp -> Colourise field mode <$> ramp' ramp
    InDomain domain base -> InDomain domain <$> child "base" base
    Mix mask a b -> Mix mask <$> child "a" a <*> child "b" b
    Flat colour -> pure (Flat colour)
    Linear from to mode ramp -> Linear from to mode <$> ramp' ramp
    Radial centre axis mode ramp -> Radial centre axis mode <$> ramp' ramp
    Circular centre radius mode ramp -> Circular centre radius mode <$> ramp' ramp
    Perlin scale mode ramp -> Perlin scale mode <$> ramp' ramp
    Fbm scale octaves persistence lacunarity style mode ramp -> Fbm scale octaves persistence lacunarity style mode <$> ramp' ramp
    Turbulence amount octaves persistence lacunarity base ->
      Turbulence amount octaves persistence lacunarity <$> child "base" base
    Tiled columns rows depth a b -> Tiled columns rows depth <$> child "a" a <*> child "b" b
    BlendTexture mode opacity top bottom -> BlendTexture mode opacity <$> child "top" top <*> child "bottom" bottom
    Layer top bottom -> Layer <$> child "top" top <*> child "bottom" bottom
  where
    ramp' = resolveRamp lookupRamp (path <> ".ramp")
    child key = resolveTexture lookupRamp (path <> "." <> key)

resolveRamp :: (ColourRamp -> Either String ColourRamp) -> String -> ColourRamp -> Either String ColourRamp
resolveRamp lookupRamp path ramp =
  either (\err -> Left ("Error in " <> path <> ": " <> err)) Right (lookupRamp ramp)

