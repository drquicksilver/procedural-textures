module PNGCompareSpec (pngCompareTests) where

import Codec.Picture
  ( Image
  , PixelRGB8 (PixelRGB8)
  , PixelRGBA8 (PixelRGBA8)
  , generateImage
  )
import Data.Either (isLeft)
import PNGCompareCore (PngImage (PngImageRGB8, PngImageRGBA8), comparePngImages)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

pngCompareTests :: TestTree
pngCompareTests =
  testGroup
    "PNGCompare"
    [ testCase "Identical RGB8 images yield zero" $ do
        let img = solidRgb8 2 2 (PixelRGB8 12 34 56)
        assertEqual "zero" (Right 0.0) (comparePngImages (PngImageRGB8 img) (PngImageRGB8 img))
    , testCase "RGB8 difference produces expected average" $ do
        let imgA = solidRgb8 1 1 (PixelRGB8 0 0 0)
            imgB = solidRgb8 1 1 (PixelRGB8 255 255 255)
            expected = sqrt 3
        case comparePngImages (PngImageRGB8 imgA) (PngImageRGB8 imgB) of
          Right avg -> assertBool "avg" (approx expected avg)
          Left err -> assertBool err False
    , testCase "Size mismatch returns error" $ do
        let imgA = solidRgb8 1 1 (PixelRGB8 0 0 0)
            imgB = solidRgb8 2 1 (PixelRGB8 0 0 0)
        assertBool "size mismatch" (isLeft (comparePngImages (PngImageRGB8 imgA) (PngImageRGB8 imgB)))
    , testCase "Format mismatch returns error" $ do
        let imgA = solidRgb8 1 1 (PixelRGB8 0 0 0)
            imgB = solidRgba8 1 1 (PixelRGBA8 0 0 0 0)
        assertBool "format mismatch" (isLeft (comparePngImages (PngImageRGB8 imgA) (PngImageRGBA8 imgB)))
    ]

solidRgb8 :: Int -> Int -> PixelRGB8 -> Image PixelRGB8
solidRgb8 width height pixel =
  generateImage (\_ _ -> pixel) width height

solidRgba8 :: Int -> Int -> PixelRGBA8 -> Image PixelRGBA8
solidRgba8 width height pixel =
  generateImage (\_ _ -> pixel) width height

approx :: Double -> Double -> Bool
approx expected actual =
  abs (expected - actual) <= 1.0e-6
