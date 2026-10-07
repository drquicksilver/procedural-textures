module ContactSheetSpec (contactSheetTests) where

import Codec.Picture (encodePng, imageHeight, imageWidth, pixelAt, PixelRGBA8 (..))
import ContactSheet (loadContactSheetFont, renderContactSheet, wrapText)
import Data.List (isInfixOf)
import Gallery (GalleryEntry (..), groupByCategory)
import Graphics.Text.TrueType (PointSize (..), BoundingBox (..), stringBoundingBox)
import Render (ImageFn, renderImage)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

contactSheetTests :: IO TestTree
contactSheetTests = do
  font <- loadContactSheetFont
  let entry :: String -> GalleryEntry ImageFn
      entry category = GalleryEntry (\_ _ -> (1, 0, 0, 1)) "Red" "Solid red." category "{}" Nothing
      render = renderContactSheet font "Gallery"
      eight = render (replicate 8 (entry "natural"))
  pure $ testGroup "Contact sheet"
    [ testCase "Eight columns contain exactly 128-square previews" $ do
        assertEqual "canvas width" 1376 (imageWidth eight)
        mapM_ (\column -> do
          let x = 36 + column * 168
          assertEqual "top left" (PixelRGBA8 255 0 0 255) (pixelAt eight x 124)
          assertEqual "bottom right" (PixelRGBA8 255 0 0 255) (pixelAt eight (x + 127) 251)
          assertEqual "card padding" (PixelRGBA8 255 255 255 255) (pixelAt eight (x + 128) 251)
          ) [0 .. 7]
    , testCase "Preview pixels match the renderer without resampling" $ do
        let imageFn x y = (x, y, 0, 1)
            image = render [(entry "natural") {entryImage = imageFn}]
            original = renderImage 128 128 imageFn
        mapM_ (\(x, y) -> assertEqual "exact pixel" (pixelAt original x y) (pixelAt image (36 + x) (124 + y)))
          [(0, 0), (1, 1), (63, 85), (126, 126), (127, 127)]
    , testCase "A ninth texture adds a row, and another category starts a section" $ do
        let nine = render (replicate 9 (entry "natural"))
            sections = render [entry "natural", entry "pattern"]
        assertEqual "one row" 373 (imageHeight eight)
        assertEqual "two rows" 594 (imageHeight nine)
        assertEqual "ninth preview" (PixelRGBA8 255 0 0 255) (pixelAt nine 36 345)
        assertEqual "two section headers" 285 (imageHeight sections - imageHeight (render [entry "natural"]))
    , testCase "Long titles and descriptions grow the row, with no truncation" $ do
        let long = (entry "natural") {entryTitle = unwords (replicate 15 "Title"), entryDescription = unwords (replicate 80 "description")}
        assertBool "height grows" (imageHeight (render [long]) > imageHeight eight)
        let wordsToWrap = "First line\nUnicode: café — textures " <> replicate 80 'w'
            wrapped = wrapText font (PointSize 8.25) 128 wordsToWrap
        assertBool "explicit newline" ("First line" `elem` wrapped)
        assertBool "unicode retained" ("café" `isInfixOf` unwords wrapped)
        assertEqual "no characters lost" (filter (/= ' ') (filter (/= '\n') wordsToWrap)) (filter (/= ' ') (concat wrapped))
        mapM_ (\line -> assertBool "fits preview width" (_xMax (stringBoundingBox font 96 (PointSize 8.25) line) <= 128)) wrapped
    , testCase "JSON documents have no effect on the PNG" $ do
        let original = entry "natural"
            changed = original {entryCode = error "Contact sheets must not evaluate JSON"}
        assertEqual "identical PNG" (encodePng (render [original])) (encodePng (render [changed]))
    , testCase "Both outputs share known, custom, and uncategorised section order" $
        assertEqual "sections" ["Materials", "Patterns & symmetry", "Geometry & distance", "Colour & compositing", "Custom", "Other"]
          (map fst (groupByCategory (map entry ["", "custom", "colour", "pattern", "materials", "geometry"])))
    , testCase "Transparent previews show the checkerboard" $ do
        let transparent = (entry "natural") {entryImage = \_ _ -> (1, 0, 0, 0)}
            image = render [transparent]
        assertEqual "light square" (PixelRGBA8 255 255 255 255) (pixelAt image 38 126)
        assertEqual "dark square" (PixelRGBA8 224 224 220 255) (pixelAt image 46 126)
    , testCase "An empty gallery still renders its heading" $ do
        let empty = render []
        assertEqual "width" 1376 (imageWidth empty)
        assertEqual "height" 88 (imageHeight empty)
    ]
