module Gallery
  ( GalleryEntry (..)
  , groupByCategory
  ) where

import Data.Char (toUpper)
import Data.List (nub, sort)

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

