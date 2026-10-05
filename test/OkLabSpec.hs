module OkLabSpec (okLabTests) where

import Colours (Colour)
import OkLab (Lab (..), fromLab, mixLab, toLab)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

okLabTests :: TestTree
okLabTests =
  testGroup
    "OkLab"
    [ testCase "Known values: white, black and pure red" $ do
        assertLab "white" (1.0, 0.0, 0.0) (toLab (1, 1, 1, 1))
        assertLab "black" (0.0, 0.0, 0.0) (toLab (0, 0, 0, 1))
        -- Reference values from Björn Ottosson's OKLab definition.
        assertLab "red" (0.627955, 0.224863, 0.125846) (toLab (1, 0, 0, 1))
    , testCase "Converting to OKLab and back is the identity" $
        sequence_
          [ assertColour (show c) c (fromLab (toLab c))
          | c <- [(0.15, 0.2, 0.6, 1), (1, 0.5, 0, 0.25), (0.02, 0.9, 0.4, 1), (0.5, 0.5, 0.5, 0.5)]
          ]
    , testCase "Blending endpoints gives the endpoints" $ do
        let a = toLab (1, 0, 0, 1)
            b = toLab (0, 0, 1, 0.5)
        assertColour "start" (1, 0, 0, 1) (fromLab (mixLab 0 a b))
        assertColour "end" (0, 0, 1, 0.5) (fromLab (mixLab 1 a b))
    , testCase "Fading to transparent keeps the colour (premultiplied)" $ do
        let (r, g, b, a) = fromLab (mixLab 0.5 (toLab (1, 0, 0, 1)) (toLab (0, 0, 0, 0)))
        assertColour "half-transparent red" (1, 0, 0, 0.5) (r, g, b, a)
    , testCase "Blending red and blue avoids sRGB's dark middle" $ do
        let (r, _, b, _) = fromLab (mixLab 0.5 (toLab (1, 0, 0, 1)) (toLab (0, 0, 1, 1)))
        assertBool "brighter than the sRGB average of 0.5" (r > 0.5 && b > 0.5)
    , testCase "Both transparent: colours blend directly, alpha stays 0" $ do
        let Lab _ _ _ alpha = mixLab 0.3 (toLab (1, 1, 1, 0)) (toLab (0, 0, 0, 0))
        assertBool "alpha" (alpha == 0)
    ]

assertLab :: String -> (Double, Double, Double) -> Lab -> Assertion
assertLab label (l, a, b) (Lab l' a' b' _) =
  assertBool (label <> ": " <> show (l', a', b')) (all (< 1e-4) [abs (l - l'), abs (a - a'), abs (b - b')])

-- | The published OKLab matrices are given to 10 decimal places, so the
-- forward and inverse conversions are inverses only to about 1e-8, which
-- sRGB's linear segment magnifies about 13 times near black. Still some
-- 5000 times smaller than one 8-bit step (about 0.004).
assertColour :: String -> Colour -> Colour -> Assertion
assertColour label (r, g, b, a) (r', g', b', a') =
  assertBool (label <> ": got " <> show (r', g', b', a')) (all (< 1e-6) [abs (r - r'), abs (g - g'), abs (b - b'), abs (a - a')])
