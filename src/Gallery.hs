module Gallery
  ( GalleryEntry (..)
  , groupByCategory
  , selectShapeMaterials
  , shapeMaterials
  , shapeTitle
  , shapeDescription
  , exampleCategories
  , groupByFamily, guideRoleLabel
  ) where

import Data.Char (toUpper)
import Data.List (nub, sort, sortOn, groupBy)
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import TextureJson (ExampleGuide(..))
import Geometry (Shape (..))

data GalleryEntry image = GalleryEntry
  { entryImage :: image
  , entryTitle :: String
  , entryDescription :: String
  , entryCategory :: String
  -- ^ Cards are grouped by category; empty means "Other".
  , entryCode :: String
  , entryGuide :: Maybe ExampleGuide
  -- ^ The texture document, shown in a collapsible block.
  }

-- | Known categories in display order, then any others alphabetically, then
-- uncategorised entries.
groupByCategory :: [GalleryEntry image] -> [(String, [GalleryEntry image])]
groupByCategory entries =
  [ (heading category, members)
  | category <- order
  , let members = sortOn ordering (filter ((== category) . entryCategory) entries)
  , not (null members)
  ]
  where
    known = map fst exampleCategories
    others = sort (nub [c | c <- map entryCategory entries, c `notElem` known, not (null c)])
    order = known <> others <> [""]
    heading "" = "Other"
    heading category@(c : cs) = case lookup category exampleCategories of
      Just label -> label
      Nothing -> toUpper c : cs
    rank entry = maybe 100 guideOrder (entryGuide entry)
    family entry = maybe "" (T.unpack . guideFamily) (entryGuide entry)
    ordering entry =
      let f = family entry
          firstRank = if null f then rank entry else minimum [rank e | e <- entries, entryCategory e == entryCategory entry, family e == f]
      in (firstRank, f, rank entry)

groupByFamily :: [GalleryEntry image] -> [(String, [GalleryEntry image])]
groupByFamily entries = [(family first, members) | members@(first : _) <- groupBy (\a b -> family a == family b) entries]
  where family entry = concat [T.unpack (guideFamily guide) | guide <- maybeToList (entryGuide entry)]

guideRoleLabel :: T.Text -> String
guideRoleLabel role = case T.unpack role of
  "preset" -> "Material preset"
  "study" -> "Minimal study"
  "comparison" -> "Controlled comparison"
  _ -> "Composition study"

-- | Shared browsing taxonomy. Unknown/legacy categories remain usable.
exampleCategories :: [(String, String)]
exampleCategories =
  [ ("materials", "Materials")
  , ("landscape", "Landscapes & atmosphere")
  , ("pattern", "Patterns & symmetry")
  , ("geometry", "Geometry & distance")
  , ("colour", "Colour & compositing")
  , ("fields", "Fields & warps")
  , ("simulation", "Simulation")
  ]

-- | Example ids shown on every per-shape page, in display order. Banded
-- minerals and wood grain show the most of a solid's interior.
shapeMaterials :: [String]
shapeMaterials = ["agate", "walnut", "malachite", "marble", "tiger-eye", "lava"]

-- | Preserve the repository showcase when available, then fill remaining
-- slots from the supplied library in stable id order. Empty libraries are valid.
selectShapeMaterials :: [String] -> [String]
selectShapeMaterials available = take (length shapeMaterials)
  (filter (`elem` available) shapeMaterials <> sort [name | name <- nub available, name `notElem` shapeMaterials])

shapeTitle :: Shape -> String
shapeTitle s = case s of
  Ball -> "Sphere"
  Cube -> "Cube"
  Tube -> "Cylinder"
  Ring -> "Torus"
  BittenCube -> "Bitten cube"
  CutSphere -> "Cut sphere"
  CutCube -> "Cut cube"
  Pawn -> "Pawn"
  Rook -> "Rook"
  Knight -> "Knight"
  Bishop -> "Bishop"
  Queen -> "Queen"
  King -> "King"

shapeDescription :: Shape -> String
shapeDescription s = case s of
  Ball -> "A plain sphere: the material only shows on its curved surface."
  Cube -> "A plain cube: three flat faces cut the material along different axes."
  Tube -> "An upright cylinder: a curved side and a flat circular cap."
  Ring -> "A torus: the material wraps around a ring with a hole through it."
  BittenCube -> "A cube with a spherical bite taken out of one corner, exposing a curved cutaway."
  CutSphere -> "A sphere with a box-shaped notch removed, exposing two flat cut faces."
  CutCube -> "A cube sliced by a tilted plane, exposing one large oblique cut face."
  Pawn -> "A Staunton pawn: a turned foot, tapering stem and collar under a round head."
  Rook -> "A Staunton rook: a stout tower with a hollow top and six crenellations."
  Knight -> "A Staunton knight: a horse's head and neck, rounded at the edges, on a turned foot."
  Bishop -> "A Staunton bishop: a slender stem rising to a pointed mitre with a slit cut into it."
  Queen -> "A Staunton queen: a tall stem under a coronet of eight points around a domed finial."
  King -> "A Staunton king: the tallest piece, with a flared crown topped by a cross."
