module ColourRamps
  ( ColourRamp(..)
  , RampMode(..)
  , Stop
  , colourRamp
  , twoStopRamp
  , sawtoothColourRamp
  , sinusoidalColourRamp
  , evalRamp
  ) where

import Colours (Colour)
import Data.List (sortOn)

data ColourRamp
  = Ramp RampMode [Stop]
  | Sinusoidal Colour Colour
  deriving (Eq, Show)

data RampMode
  = Clamp
  | Wrap
  | Mirror
  deriving (Eq, Show)

type Stop = (Double, Colour)

colourRamp :: RampMode -> [Stop] -> ColourRamp
colourRamp mode stops =
  Ramp mode stops

twoStopRamp :: RampMode -> Colour -> Colour -> ColourRamp
twoStopRamp mode from to =
  Ramp mode [(0.0, from), (1.0, to)]

sawtoothColourRamp :: Colour -> Colour -> ColourRamp
sawtoothColourRamp = twoStopRamp Mirror

sinusoidalColourRamp :: Colour -> Colour -> ColourRamp
sinusoidalColourRamp = Sinusoidal

evalRamp :: ColourRamp -> Double -> Colour
evalRamp ramp t =
  case ramp of
    Ramp mode stops ->
      let sortedStops = sortOn fst stops
          (minPos, maxPos) = stopBounds sortedStops
          spanLength = maxPos - minPos
          t' = applyMode mode minPos maxPos spanLength t
      in evalStops sortedStops t'
    Sinusoidal from to ->
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
applyMode mode minPos maxPos spanLength t =
  case mode of
    Clamp -> clamp minPos maxPos t
    Wrap -> wrap minPos spanLength t
    Mirror ->
      let wrapped = wrap minPos (spanLength * 2.0) t
          offset = wrapped - minPos
          mirrored = if offset <= spanLength then offset else (spanLength * 2.0) - offset
      in minPos + mirrored

mirrorParam :: Double -> Double -> Double -> Double
mirrorParam minPos maxPos t =
  let spanLength = maxPos - minPos
      wrapped = wrap minPos (spanLength * 2.0) t
      offset = wrapped - minPos
      mirrored = if offset <= spanLength then offset else (spanLength * 2.0) - offset
  in if spanLength <= 0.0 then minPos else minPos + mirrored

wrap :: Double -> Double -> Double -> Double
wrap minPos spanLength t =
  if spanLength <= 0.0
    then minPos
    else
      let offset = t - minPos
          wrapped = offset - fromIntegral (floor (offset / spanLength)) * spanLength
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
