{-# LANGUAGE OverloadedStrings #-}

-- | Build-time geometry export shared by static assets and conformance fixtures.
module GeometryJson (sdfValue) where

import qualified Geometry as G
import Vector3 (Vec3)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)

sdfValue :: G.SDF -> Value
sdfValue solid = case solid of
  G.Sphere c r -> tagged "sphere" ["centre" .= v3 c,"radius" .= r]
  G.Box c h -> tagged "box" ["centre" .= v3 c,"half" .= v3 h]
  G.Cylinder c r h -> tagged "cylinder" ["centre" .= v3 c,"radius" .= r,"height" .= h]
  G.Torus c r t -> tagged "torus" ["centre" .= v3 c,"major" .= r,"minor" .= t]
  G.Plane n o -> tagged "plane" ["normal" .= v3 n,"offset" .= o]
  G.Union a b -> pair "union" a b []
  G.Intersection a b -> pair "intersection" a b []
  G.Difference a b -> pair "difference" a b []
  G.Blend k a b -> pair "blend" a b ["amount" .= k]
  G.Rounded r a -> tagged "rounded" ["amount" .= r,"base" .= sdfValue a]
  G.Revolve c p -> tagged "revolve" ["centre" .= v3 c,"profile" .= profileValue p]
  G.Extrude c h p -> tagged "extrude" ["centre" .= v3 c,"height" .= h,"profile" .= profileValue p]
  G.Turn c a s -> tagged "turn" ["centre" .= v3 c,"angle" .= a,"base" .= sdfValue s]
  G.RadialRepeat c n s -> tagged "radial-repeat" ["centre" .= v3 c,"count" .= n,"base" .= sdfValue s]
  G.Scaled c k s -> tagged "scaled" ["centre" .= v3 c,"amount" .= k,"base" .= sdfValue s]
  G.Bounded b s -> tagged "bounded" ["bound" .= sdfValue b,"base" .= sdfValue s]
  where pair name a b fields = tagged name (["a" .= sdfValue a,"b" .= sdfValue b] <> fields)

profileValue :: G.Profile -> Value
profileValue profile = case profile of
  G.Disc c r -> tagged "disc" ["centre" .= v2 c,"radius" .= r]
  G.Rect c h r -> tagged "rect" ["centre" .= v2 c,"half" .= v2 h,"radius" .= r]
  G.Polygon vs -> tagged "polygon" ["vertices" .= map v2 vs]
  G.ProfileUnion a b -> pair "union" a b []
  G.ProfileBlend k a b -> pair "blend" a b ["amount" .= k]
  G.ProfileDifference a b -> pair "difference" a b []
  where pair name a b fields = tagged name (["a" .= profileValue a,"b" .= profileValue b] <> fields)

tagged :: String -> [Pair] -> Value
tagged name fields = object (["type" .= name] <> fields)
v3 :: Vec3 -> [Double]
v3 (x,y,z) = [x,y,z]
v2 :: G.Vec2 -> [Double]
v2 (x,y) = [x,y]
