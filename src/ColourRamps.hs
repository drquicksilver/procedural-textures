module ColourRamps
  ( ColourRamp
  , RampMode(..)
  , Stop
  , colourRamp
  , twoStopRamp
  , sawtoothColourRamp
  , sinusoidalColourRamp
  ) where

import Colours (Colour)
import Data.List (sortOn)

type ColourRamp = Double -> Colour

data RampMode
  = Clamp
  | Wrap
  | Mirror
  deriving (Eq, Show)

type Stop = (Double, Colour)

colourRamp :: RampMode -> [Stop] -> ColourRamp
colourRamp mode stops =
  let sortedStops = sortOn fst stops
      (minPos, maxPos) = stopBounds sortedStops
      span = maxPos - minPos
  in \t ->
      let t' = applyMode mode minPos maxPos span t
      in evalStops sortedStops t'

twoStopRamp :: RampMode -> Colour -> Colour -> ColourRamp
twoStopRamp mode from to =
  colourRamp mode [(0.0, from), (1.0, to)]

sawtoothColourRamp :: Colour -> Colour -> ColourRamp
sawtoothColourRamp = twoStopRamp Mirror

sinusoidalColourRamp :: Colour -> Colour -> ColourRamp
sinusoidalColourRamp from to t =
  let mirrored = mirrorParam 0.0 1.0 t
      smooth = 0.5 - 0.5 * cos (pi * mirrored)
  in lerpColour smooth from to

stopBounds :: [Stop] -> (Double, Double)
stopBounds stops =
  case stops of
    [] -> (0.0, 0.0)
    _ ->
      let minPos = fst (head stops)
          maxPos = fst (last stops)
      in (minPos, maxPos)

applyMode :: RampMode -> Double -> Double -> Double -> Double -> Double
applyMode mode minPos maxPos span t =
  case mode of
    Clamp -> clamp minPos maxPos t
    Wrap -> wrap minPos span t
    Mirror ->
      let wrapped = wrap minPos (span * 2.0) t
          offset = wrapped - minPos
          mirrored = if offset <= span then offset else (span * 2.0) - offset
      in minPos + mirrored

mirrorParam :: Double -> Double -> Double -> Double
mirrorParam minPos maxPos t =
  let span = maxPos - minPos
      wrapped = wrap minPos (span * 2.0) t
      offset = wrapped - minPos
      mirrored = if offset <= span then offset else (span * 2.0) - offset
  in if span <= 0.0 then minPos else minPos + mirrored

wrap :: Double -> Double -> Double -> Double
wrap minPos span t =
  if span <= 0.0
    then minPos
    else
      let offset = t - minPos
          wrapped = offset - fromIntegral (floor (offset / span)) * span
      in minPos + wrapped

clamp :: Double -> Double -> Double -> Double
clamp minPos maxPos t
  | t < minPos = minPos
  | t > maxPos = maxPos
  | otherwise = t

evalStops :: [Stop] -> Double -> Colour
evalStops stops t =
  case stops of
    [] -> (0.0, 0.0, 0.0, 1.0)
    _ ->
      let lowerStops = takeWhile (\(pos, _) -> pos <= t) stops
          upperStops = dropWhile (\(pos, _) -> pos < t) stops
          lower = if null lowerStops then head stops else last lowerStops
          upper = if null upperStops then last stops else head upperStops
      in case (lower, upper) of
           ((p1, c1), (p2, c2))
             | p1 == p2 -> c1
             | otherwise ->
                 let denom = p2 - p1
                     weight = if denom <= 0.0 then 0.0 else (t - p1) / denom
                 in lerpColour weight c1 c2

lerpColour :: Double -> Colour -> Colour -> Colour
lerpColour t (r1, g1, b1, a1) (r2, g2, b2, a2) =
  ( lerp t r1 r2
  , lerp t g1 g2
  , lerp t b1 b2
  , lerp t a1 a2
  )

lerp :: Double -> Double -> Double -> Double
lerp t a b =
  a + (b - a) * t
