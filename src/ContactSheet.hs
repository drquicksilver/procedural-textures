-- | A self-contained PNG gallery, with eight 128-square previews per row.
module ContactSheet
  ( writeContactSheet
  , renderContactSheet
  , loadContactSheetFont
  , wrapText
  ) where

import Codec.Picture (Image, PixelRGBA8 (..), pixelAt, writePng)
import Codec.Picture.Types (thawImage, freezeImage, writePixel)
import Control.Monad.ST (runST)
import Control.Monad (forM_)
import Gallery (GalleryEntry (..), groupByCategory)
import Graphics.Rasterific
  ( PointSize (..), V2 (..), fill, printTextAt
  , rectangle, renderDrawing, withTexture
  )
import Graphics.Rasterific.Texture (uniformTexture)
import Graphics.Text.TrueType (Font, BoundingBox (..), loadFontFile, stringBoundingBox)
import Paths_procedural_textures (getDataFileName)
import Render (ImageFn, renderImage)

loadContactSheetFont :: IO Font
loadContactSheetFont = do
  path <- getDataFileName "fonts/OpenSans-Regular.ttf"
  result <- loadFontFile path
  either (ioError . userError . ((path <> ": ") <>)) pure result

writeContactSheet :: FilePath -> String -> [GalleryEntry ImageFn] -> IO ()
writeContactSheet path title entries = do
  font <- loadContactSheetFont
  writePng path (renderContactSheet font title entries)

-- | Layout measures captions with the same font and sizes used to draw them.
-- Each section starts a new row; row height grows to fit the longest caption.
-- JSON is deliberately never inspected or drawn.
renderContactSheet :: Font -> String -> [GalleryEntry ImageFn] -> Image PixelRGBA8
renderContactSheet font title entries =
  pastePreviews placements $ renderDrawing sheetWidth sheetHeight background $ do
    text ink (PointSize 20) 32 margin 48 titleLines
    drawSections titleHeight sections
  where
    sections = groupByCategory entries
    placements =
      [ (margin + column * (cardWidth + gutter) + 12, rowY + 12, entryImage entry)
      | (sectionY, (heading, members)) <- zip (scanl (+) titleHeight (map sectionHeight sections)) sections
      , let rows = chunksOf 8 members
      , (rowY, row) <- zip (scanl (+) (sectionY + headingHeight heading) (map rowHeight rows)) rows
      , (column, entry) <- zip [0 ..] row
      ]
    titleLines = wrapText font (PointSize 20) (fromIntegral (sheetWidth - 2 * margin)) title
    titleHeight = 32 + 32 * length titleLines
    sheetHeight = titleHeight + sum (map sectionHeight sections) + margin
    sectionLines heading = wrapText font (PointSize 16) (fromIntegral (sheetWidth - 2 * margin)) heading
    headingHeight heading = 20 + 28 * length (sectionLines heading)
    sectionHeight (heading, members) = headingHeight heading + sum (map rowHeight (chunksOf 8 members)) + 16
    titleCaption entry = wrapText font (PointSize 10.5) 128 (entryTitle entry)
    description entry = wrapText font (PointSize 8.25) 128 (entryDescription entry)
    cardHeight entry = 12 + 128 + 12 + 19 * length (titleCaption entry) + 6 + 16 * length (description entry) + 12
    rowHeight row = maximum (map cardHeight row) + gutter
    text colour size lineHeight x baseline ls =
      withTexture (uniformTexture colour) $
        forM_ (zip [0 :: Int ..] ls) $ \(line, contents) ->
          printTextAt font size (V2 (fromIntegral x) (fromIntegral (baseline + line * lineHeight))) contents
    drawSections _ [] = pure ()
    drawSections y ((heading, members) : rest) = do
      text muted (PointSize 16) 28 margin (y + 28) (sectionLines heading)
      drawRows (y + headingHeight heading) (chunksOf 8 members)
      drawSections (y + sectionHeight (heading, members)) rest
    drawRows _ [] = pure ()
    drawRows y (row : rest) = do
      forM_ (zip [0 :: Int ..] row) $ \(column, entry) -> do
        let x = margin + column * (cardWidth + gutter)
            height = rowHeight row - gutter
            titleY = y + 12 + 128 + 26
            descriptionY = titleY + 19 * length (titleCaption entry) + 3
        withTexture (uniformTexture white) $
          fill (rectangle (V2 (fromIntegral x) (fromIntegral y)) (fromIntegral cardWidth) (fromIntegral height))
        text ink (PointSize 10.5) 19 (x + 12) titleY (titleCaption entry)
        text muted (PointSize 8.25) 16 (x + 12) descriptionY (description entry)
      drawRows (y + rowHeight row) rest

-- | Copy previews pixel for pixel so the image drawing backend cannot resample
-- them or antialias their edges. Composite transparency over a checkerboard.
pastePreviews :: [(Int, Int, ImageFn)] -> Image PixelRGBA8 -> Image PixelRGBA8
pastePreviews placements canvas = runST $ do
  target <- thawImage canvas
  forM_ placements $ \(left, top, imageFn) -> do
    let image = renderImage 128 128 imageFn
    forM_ [0 .. 127] $ \y ->
      forM_ [0 .. 127] $ \x -> do
        let PixelRGBA8 r g b a = pixelAt image x y
            checker = if even (x `div` 8 + y `div` 8) then 255 else 224
            checkerBlue = if checker == 255 then 255 else 220
            blend channel backing = fromIntegral ((fromIntegral channel * alpha + backing * (255 - alpha) + 127) `div` 255 :: Int)
            alpha = fromIntegral a
        writePixel target (left + x) (top + y) (PixelRGBA8 (blend r checker) (blend g checker) (blend b checkerBlue) 255)
  freezeImage target

-- | Wrap on words, preserving explicit newlines and splitting overlong words.
-- Width is in pixels, at Rasterific's default 96 DPI.
wrapText :: Font -> PointSize -> Float -> String -> [String]
wrapText font size width = concatMap (wrapWords . words) . lines
  where
    fits s = _xMax (stringBoundingBox font 96 size s) <= width
    wrapWords [] = [""]
    wrapWords ws = pack "" (concatMap splitWord ws)
    splitWord "" = []
    splitWord word =
      let (prefix, rest) = takeFitting "" word
      in prefix : splitWord rest
    takeFitting prefix [] = (prefix, "")
    takeFitting prefix remaining@(c : cs)
      | null prefix || fits (prefix <> [c]) = takeFitting (prefix <> [c]) cs
      | otherwise = (prefix, remaining)
    pack line [] = [line | not (null line)]
    pack "" (w : ws) = pack w ws
    pack line (w : ws)
      | fits (line <> " " <> w) = pack (line <> " " <> w) ws
      | otherwise = line : pack w ws

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (front, rest) = splitAt n xs in front : chunksOf n rest

margin, gutter, cardWidth, sheetWidth :: Int
margin = 24
gutter = 16
cardWidth = 152
sheetWidth = 2 * margin + 8 * cardWidth + 7 * gutter

background, white, ink, muted :: PixelRGBA8
background = PixelRGBA8 245 245 242 255
white = PixelRGBA8 255 255 255 255
ink = PixelRGBA8 31 31 31 255
muted = PixelRGBA8 92 92 92 255
