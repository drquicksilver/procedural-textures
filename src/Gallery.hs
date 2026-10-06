module Gallery
  ( GalleryEntry (..)
  , groupByCategory
  , shapeMaterials
  , shapeTitle
  , shapeDescription
  ) where

import Data.Char (toUpper)
import Data.List (nub, sort)
import Geometry (Shape (..))

data GalleryEntry image = GalleryEntry
  { entryImage :: image
  , entryTitle :: String
  , entryDescription :: String
  , entryCategory :: String
  -- ^ Cards are grouped by category; empty means "Other".
  , entryCode :: String
  -- ^ The texture document, shown in a collapsible block.
  }

-- | Known categories in display order, then any others alphabetically, then
-- uncategorised entries.
groupByCategory :: [GalleryEntry image] -> [(String, [GalleryEntry image])]
groupByCategory entries =
  [ (heading category, members)
  | category <- order
  , let members = filter ((== category) . entryCategory) entries
  , not (null members)
  ]
  where
    known = ["natural", "pattern", "geometric", "effect"]
    others = sort (nub [c | c <- map entryCategory entries, c `notElem` known, not (null c)])
    order = known <> others <> [""]
    heading "" = "Other"
    heading (c : cs) = toUpper c : cs

-- | Example ids shown on every per-shape page, in display order. Banded
-- minerals and wood grain show the most of a solid's interior.
shapeMaterials :: [String]
shapeMaterials = ["agate", "walnut", "malachite", "marble", "tiger-eye", "lava"]

shapeTitle :: Shape -> String
shapeTitle s = case s of
  Ball -> "Sphere"
  Cube -> "Cube"
  Tube -> "Cylinder"
  Ring -> "Torus"
  BittenCube -> "Bitten cube"
  CutSphere -> "Cut sphere"
  CutCube -> "Cut cube"

shapeDescription :: Shape -> String
shapeDescription s = case s of
  Ball -> "A plain sphere: the material only shows on its curved surface."
  Cube -> "A plain cube: three flat faces cut the material along different axes."
  Tube -> "An upright cylinder: a curved side and a flat circular cap."
  Ring -> "A torus: the material wraps around a ring with a hole through it."
  BittenCube -> "A cube with a spherical bite taken out of one corner, exposing a curved cutaway."
  CutSphere -> "A sphere with a box-shaped notch removed, exposing two flat cut faces."
  CutCube -> "A cube sliced by a tilted plane, exposing one large oblique cut face."
