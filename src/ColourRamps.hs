module ColourRamps
  ( ColourRamp(..)
  , RampMode(..)
  , Stop
  , colourRamp
  , twoStopRamp
  , sawtoothColourRamp
  , sinusoidalColourRamp
  , evalRamp
  , compileRamp
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
evalRamp = compileRamp

-- | Prepare a ramp for evaluation at many positions. The stops are sorted once
-- here, not on every call, so bind the result and reuse it:
-- @let f = compileRamp ramp in map f ts@.
compileRamp :: ColourRamp -> Double -> Colour
compileRamp ramp =
  case ramp of
    Ramp mode stops ->
      -- sortOn is stable, so stops at the same position keep their order.
      let sortedStops = sortOn fst stops
          (minPos, maxPos) = stopBounds sortedStops
          spanLength = maxPos - minPos
      in sortedStops `seq` \t -> evalStops sortedStops (applyMode mode minPos maxPos spanLength t)
    Sinusoidal from to ->
      \t ->
        let mirrored = mirrorParam 0.0 1.0 t
            smooth = 0.5 - 0.5 * cos (pi * mirrored)
        in lerpColour smooth from to

stopBounds :: [Stop] -> (Double, Double)
stopBounds stops =
  case stops of
    [] -> (0.0, 0.0)
    first : rest -> (fst first, fst (lastOr first rest))

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
          wrapped = offset - fromIntegral (floor (offset / spanLength) :: Integer) * spanLength
      in minPos + wrapped

clamp :: Double -> Double -> Double -> Double
clamp minPos maxPos t
  | t < minPos = minPos
  | t > maxPos = maxPos
  | otherwise = t

-- | Colour at position @t@ of sorted stops. With @lower@ the last stop at or
-- before @t@: before every stop the first stop's colour is used; on a stop (or
-- after the last) @lower@'s colour is used, so at a hard edge, where two stops
-- share a position, the later one wins; otherwise the colour is interpolated
-- between @lower@ and the stop after it.
evalStops :: [Stop] -> Double -> Colour
evalStops stops t =
  case stops of
    [] -> (0.0, 0.0, 0.0, 1.0)
    first@(p0, c0) : rest
      | p0 > t -> c0
      | otherwise -> go first rest
  where
    go lower@(p1, c1) remaining =
      case remaining of
        next@(p2, c2) : more
          | p2 <= t -> go next more
          | p1 == t -> c1
          | otherwise -> lerpColour ((t - p1) / (p2 - p1)) c1 c2
        [] -> snd lower

-- | The last element of a list, or the given fallback if it is empty.
lastOr :: a -> [a] -> a
lastOr = foldl (\_ x -> x)

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
