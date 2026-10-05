module PNGCompareCore
  ( PngImage (..)
  , CompareResult (..)
  , Tolerance (..)
  , defaultTolerance
  , withinTolerance
  , comparePngImages
  , compareRgbaImages
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
  , pixelMap
  )

data PngImage
  = PngImageRGB8 (Image PixelRGB8)
  | PngImageRGBA8 (Image PixelRGBA8)

-- | Per-pixel distances are Euclidean distances between RGBA values with
-- channels normalised to [0, 1], so a single pixel's distance is at most 2.
data CompareResult = CompareResult
  { meanError :: Double
  , maxError :: Double
  }
  deriving (Eq, Show)

data Tolerance = Tolerance
  { toleranceMean :: Double
  , toleranceMax :: Double
  }
  deriving (Eq, Show)

-- | The tolerance used by the golden suite. One step of one 8-bit channel is
-- a distance of 1/255 (about 0.0039), so the maximum allows a single rounding
-- step in each channel of a pixel, and the mean allows such flips on only a
-- small fraction of pixels. That admits floating-point reordering and little
-- else. An optimisation that needs more must say so and use a looser,
-- documented tolerance.
defaultTolerance :: Tolerance
defaultTolerance =
  Tolerance {toleranceMean = 0.0001, toleranceMax = 0.008}

withinTolerance :: Tolerance -> CompareResult -> Bool
withinTolerance tolerance result =
  meanError result <= toleranceMean tolerance
    && maxError result <= toleranceMax tolerance

pngImageFromDynamic :: DynamicImage -> Either String PngImage
pngImageFromDynamic image =
  case image of
    ImageRGB8 img -> Right (PngImageRGB8 img)
    ImageRGBA8 img -> Right (PngImageRGBA8 img)
    _ -> Left "Unsupported PNG pixel format. Only RGB8 and RGBA8 are supported."

comparePngImages :: PngImage -> PngImage -> Either String CompareResult
comparePngImages left right =
  case (left, right) of
    (PngImageRGB8 imgLeft, PngImageRGB8 imgRight) ->
      compareRgbaImages (pixelMap opaque imgLeft) (pixelMap opaque imgRight)
    (PngImageRGBA8 imgLeft, PngImageRGBA8 imgRight) ->
      compareRgbaImages imgLeft imgRight
    _ -> Left "PNG pixel formats differ; both images must have the same format."

opaque :: PixelRGB8 -> PixelRGBA8
opaque (PixelRGB8 r g b) =
  PixelRGBA8 r g b 255

compareRgbaImages :: Image PixelRGBA8 -> Image PixelRGBA8 -> Either String CompareResult
compareRgbaImages imgLeft imgRight
  | sizesDiffer imgLeft imgRight =
      Left (sizeMismatchMessage imgLeft imgRight)
  | otherwise =
      Right (summarise (imageWidth imgLeft * imageHeight imgLeft) (distances imgLeft imgRight))

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

distances :: Image PixelRGBA8 -> Image PixelRGBA8 -> [Double]
distances imgLeft imgRight =
  [ rgbaDistance (pixelAt imgLeft x y) (pixelAt imgRight x y)
  | y <- [0 .. imageHeight imgLeft - 1]
  , x <- [0 .. imageWidth imgLeft - 1]
  ]

summarise :: Int -> [Double] -> CompareResult
summarise count values =
  let (total, largest) = foldl' step (0.0, 0.0) values
      step (accTotal, accMax) value =
        let newTotal = accTotal + value
            newMax = max accMax value
        in newTotal `seq` newMax `seq` (newTotal, newMax)
      mean = if count <= 0 then 0.0 else total / fromIntegral count
  in CompareResult {meanError = mean, maxError = largest}

-- | Two fully transparent pixels are equal whatever their colour channels say.
rgbaDistance :: PixelRGBA8 -> PixelRGBA8 -> Double
rgbaDistance (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2)
  | a1 == 0 && a2 == 0 = 0.0
  | otherwise = sqrt (dr * dr + dg * dg + db * db + da * da)
  where
    dr = normalizeChannel r1 - normalizeChannel r2
    dg = normalizeChannel g1 - normalizeChannel g2
    db = normalizeChannel b1 - normalizeChannel b2
    da = normalizeChannel a1 - normalizeChannel a2

normalizeChannel :: Pixel8 -> Double
normalizeChannel channel =
  fromIntegral channel / 255.0
