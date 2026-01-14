module PNGCompareCore
  ( PngImage (..)
  , comparePngImages
  , pngImageFromDynamic
  ) where

import Codec.Picture
  ( DynamicImage (ImageRGB8, ImageRGBA8)
  , Image
  , Pixel8
  , PixelRGB8 (PixelRGB8)
  , PixelRGBA8 (PixelRGBA8)
  , imageHeight
  , imageWidth
  , pixelAt
  )

data PngImage
  = PngImageRGB8 (Image PixelRGB8)
  | PngImageRGBA8 (Image PixelRGBA8)

pngImageFromDynamic :: DynamicImage -> Either String PngImage
pngImageFromDynamic image =
  case image of
    ImageRGB8 img -> Right (PngImageRGB8 img)
    ImageRGBA8 img -> Right (PngImageRGBA8 img)
    _ -> Left "Unsupported PNG pixel format. Only RGB8 and RGBA8 are supported."

comparePngImages :: PngImage -> PngImage -> Either String Double
comparePngImages left right =
  case (left, right) of
    (PngImageRGB8 imgLeft, PngImageRGB8 imgRight) ->
      compareRgbImages imgLeft imgRight
    (PngImageRGBA8 imgLeft, PngImageRGBA8 imgRight) ->
      compareRgbaImages imgLeft imgRight
    _ -> Left "PNG pixel formats differ; both images must have the same format."

compareRgbImages :: Image PixelRGB8 -> Image PixelRGB8 -> Either String Double
compareRgbImages imgLeft imgRight
  | sizesDiffer imgLeft imgRight =
      Left (sizeMismatchMessage imgLeft imgRight)
  | otherwise =
      Right (averageDistance (imageWidth imgLeft) (imageHeight imgLeft) (rgbDistanceSum imgLeft imgRight))

compareRgbaImages :: Image PixelRGBA8 -> Image PixelRGBA8 -> Either String Double
compareRgbaImages imgLeft imgRight
  | sizesDiffer imgLeft imgRight =
      Left (sizeMismatchMessage imgLeft imgRight)
  | otherwise =
      Right (averageDistance (imageWidth imgLeft) (imageHeight imgLeft) (rgbaDistanceSum imgLeft imgRight))

sizesDiffer :: Image a -> Image b -> Bool
sizesDiffer imgLeft imgRight =
  imageWidth imgLeft /= imageWidth imgRight
    || imageHeight imgLeft /= imageHeight imgRight

sizeMismatchMessage :: Image a -> Image b -> String
sizeMismatchMessage imgLeft imgRight =
  "Image sizes differ: "
    <> show (imageWidth imgLeft, imageHeight imgLeft)
    <> " vs "
    <> show (imageWidth imgRight, imageHeight imgRight)

averageDistance :: Int -> Int -> Double -> Double
averageDistance width height total =
  total / fromIntegral (width * height)

rgbDistanceSum :: Image PixelRGB8 -> Image PixelRGB8 -> Double
rgbDistanceSum imgLeft imgRight =
  foldl' (\acc y -> acc + rowSum y) 0 [0 .. imageHeight imgLeft - 1]
  where
    rowSum y =
      foldl' (\acc x -> acc + rgbDistance (pixelAt imgLeft x y) (pixelAt imgRight x y)) 0 [0 .. imageWidth imgLeft - 1]

rgbaDistanceSum :: Image PixelRGBA8 -> Image PixelRGBA8 -> Double
rgbaDistanceSum imgLeft imgRight =
  foldl' (\acc y -> acc + rowSum y) 0 [0 .. imageHeight imgLeft - 1]
  where
    rowSum y =
      foldl' (\acc x -> acc + rgbaDistance (pixelAt imgLeft x y) (pixelAt imgRight x y)) 0 [0 .. imageWidth imgLeft - 1]

rgbDistance :: PixelRGB8 -> PixelRGB8 -> Double
rgbDistance (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) =
  rgbaDistance
    (PixelRGBA8 r1 g1 b1 255)
    (PixelRGBA8 r2 g2 b2 255)

rgbaDistance :: PixelRGBA8 -> PixelRGBA8 -> Double
rgbaDistance (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
  sqrt (dr * dr + dg * dg + db * db + da * da)
  where
    dr = normalizeChannel r1 - normalizeChannel r2
    dg = normalizeChannel g1 - normalizeChannel g2
    db = normalizeChannel b1 - normalizeChannel b2
    da = normalizeChannel a1 - normalizeChannel a2

normalizeChannel :: Pixel8 -> Double
normalizeChannel channel =
  fromIntegral channel / 255.0
