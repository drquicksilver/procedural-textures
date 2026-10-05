module PNGCompareSpec (pngCompareTests) where

import Codec.Picture
  ( Image
  , PixelRGB8 (PixelRGB8)
  , PixelRGBA8 (PixelRGBA8)
  , generateImage
  )
import Data.Either (isLeft)
import PNGCompareCore
  ( CompareResult (..)
  , PngImage (PngImageRGB8, PngImageRGBA8)
  , Tolerance (..)
  , comparePngImages
  , withinTolerance
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

pngCompareTests :: TestTree
pngCompareTests =
  testGroup
    "PNGCompare"
    [ testCase "Identical RGB8 images yield zero" $ do
        let img = solidRgb8 2 2 (PixelRGB8 12 34 56)
        assertEqual "zero" (Right (CompareResult 0.0 0.0)) (comparePngImages (PngImageRGB8 img) (PngImageRGB8 img))
    , testCase "RGB8 difference produces expected average" $ do
        let imgA = solidRgb8 1 1 (PixelRGB8 0 0 0)
            imgB = solidRgb8 1 1 (PixelRGB8 255 255 255)
        withResult (comparePngImages (PngImageRGB8 imgA) (PngImageRGB8 imgB)) $ \result -> do
          assertBool "mean" (approx (sqrt 3) (meanError result))
          assertBool "max" (approx (sqrt 3) (maxError result))
    , testCase "Mean and max differ when one pixel changes" $ do
        let imgA = solidRgba8 2 2 (PixelRGBA8 0 0 0 255)
            imgB = generateImage (\x y -> if x == 0 && y == 0 then PixelRGBA8 255 0 0 255 else PixelRGBA8 0 0 0 255) 2 2
        withResult (comparePngImages (PngImageRGBA8 imgA) (PngImageRGBA8 imgB)) $ \result -> do
          assertBool "mean" (approx 0.25 (meanError result))
          assertBool "max" (approx 1.0 (maxError result))
    , testCase "Fully transparent pixels are equal whatever their colour" $ do
        let imgA = solidRgba8 1 1 (PixelRGBA8 255 0 0 0)
            imgB = solidRgba8 1 1 (PixelRGBA8 0 0 255 0)
        assertEqual "transparent" (Right (CompareResult 0.0 0.0)) (comparePngImages (PngImageRGBA8 imgA) (PngImageRGBA8 imgB))
    , testCase "Tolerance checks both mean and max" $ do
        let tolerance = Tolerance {toleranceMean = 0.1, toleranceMax = 0.5}
        assertBool "within" (withinTolerance tolerance (CompareResult 0.05 0.4))
        assertBool "mean exceeded" (not (withinTolerance tolerance (CompareResult 0.2 0.4)))
        assertBool "max exceeded" (not (withinTolerance tolerance (CompareResult 0.05 0.6)))
    , testCase "Size mismatch returns error" $ do
        let imgA = solidRgb8 1 1 (PixelRGB8 0 0 0)
            imgB = solidRgb8 2 1 (PixelRGB8 0 0 0)
        assertBool "size mismatch" (isLeft (comparePngImages (PngImageRGB8 imgA) (PngImageRGB8 imgB)))
    , testCase "Format mismatch returns error" $ do
        let imgA = solidRgb8 1 1 (PixelRGB8 0 0 0)
            imgB = solidRgba8 1 1 (PixelRGBA8 0 0 0 0)
        assertBool "format mismatch" (isLeft (comparePngImages (PngImageRGB8 imgA) (PngImageRGBA8 imgB)))
    ]

withResult :: Either String CompareResult -> (CompareResult -> Assertion) -> Assertion
withResult result check =
  either assertFailure check result

solidRgb8 :: Int -> Int -> PixelRGB8 -> Image PixelRGB8
solidRgb8 width height pixel =
  generateImage (\_ _ -> pixel) width height

solidRgba8 :: Int -> Int -> PixelRGBA8 -> Image PixelRGBA8
solidRgba8 width height pixel =
  generateImage (\_ _ -> pixel) width height

approx :: Double -> Double -> Bool
approx expected actual =
  abs (expected - actual) <= 1.0e-6
