module ColourRamps
  ( ColourRamp(..)
  , RampMode(..)
  , Stop
  , colourRamp
  , twoStopRamp
  , sinusoidalColourRamp
  , evalRamp
  , compileRamp
  ) where

import Colours (Colour)
import Data.List (sortOn)
import Data.Text (Text)
import OkLab (Lab, fromLab, mixLab, toLab)

-- | A ramp is just colours: stops (or an ease between two colours) over a
-- span of positions, usually [0, 1]. What happens beyond the span is decided
-- by whoever uses the ramp, with a 'RampMode'.
data ColourRamp
  = Ramp [Stop]
  -- ^ Interpolates between colour stops. The span runs from the first stop's
  -- position to the last's.
  | Sinusoidal Colour Colour
  -- ^ Eases from one colour to the other across [0, 1], following half a
  -- cosine wave. With 'Mirror' it eases back and forth.
  | NamedRamp Text
  -- ^ A ramp defined in the document's own @ramps@ map.
  | BuiltinRamp Text
  -- ^ A ramp from the built-in library ("RampLibrary").
  deriving (Eq, Show)

-- | How values beyond a ramp's span map back into it. Part of each use of a
-- ramp, not of the ramp, so one ramp can be clamped in one place and
-- repeated in another.
data RampMode
  = Clamp
  -- ^ Beyond either end, keep the end colour.
  | Wrap
  -- ^ Repeat the ramp.
  | Mirror
  -- ^ Repeat the ramp, reversing every other copy.
  deriving (Eq, Show)

type Stop = (Double, Colour)

colourRamp :: [Stop] -> ColourRamp
colourRamp = Ramp

twoStopRamp :: Colour -> Colour -> ColourRamp
twoStopRamp from to =
  Ramp [(0.0, from), (1.0, to)]

sinusoidalColourRamp :: Colour -> Colour -> ColourRamp
sinusoidalColourRamp = Sinusoidal

evalRamp :: RampMode -> ColourRamp -> Double -> Colour
evalRamp = compileRamp

-- | Prepare a ramp for evaluation at many positions. The stops are sorted once
-- here, not on every call, so bind the result and reuse it:
-- @let f = compileRamp mode ramp in map f ts@.
compileRamp :: RampMode -> ColourRamp -> Double -> Colour
compileRamp mode ramp =
  case ramp of
    Ramp stops ->
      -- sortOn is stable, so stops at the same position keep their order.
      -- Each stop's OKLab form is computed once here, for blending.
      let sortedStops = [(p, c, toLab c) | (p, c) <- sortOn fst stops]
          (minPos, maxPos) = stopBounds [(p, c) | (p, c, _) <- sortedStops]
          spanLength = maxPos - minPos
      in length sortedStops `seq` \t -> evalStops sortedStops (applyMode mode minPos maxPos spanLength t)
    Sinusoidal from to ->
      let fromLab' = toLab from
          toLab' = toLab to
      in \t ->
          let eased = 0.5 - 0.5 * cos (pi * applyMode mode 0.0 1.0 1.0 t)
          in fromLab (mixLab eased fromLab' toLab')
    -- References are replaced by their definitions before rendering (see
    -- "Resolve"); one that slips through shows as unmissable magenta.
    NamedRamp _ -> const unresolved
    BuiltinRamp _ -> const unresolved

unresolved :: Colour
unresolved = (1.0, 0.0, 1.0, 1.0)

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
-- share a position, the later one wins; otherwise the colour is blended in
-- OKLab between @lower@ and the stop after it.
evalStops :: [(Double, Colour, Lab)] -> Double -> Colour
evalStops stops t =
  case stops of
    [] -> (0.0, 0.0, 0.0, 1.0)
    first@(p0, c0, _) : rest
      | p0 > t -> c0
      | otherwise -> go first rest
  where
    go lower@(p1, c1, lab1) remaining =
      case remaining of
        next@(p2, _, lab2) : more
          | p2 <= t -> go next more
          | p1 == t -> c1
          | otherwise -> fromLab (mixLab ((t - p1) / (p2 - p1)) lab1 lab2)
        [] -> let (_, c, _) = lower in c

-- | The last element of a list, or the given fallback if it is empty.
lastOr :: a -> [a] -> a
lastOr = foldl (\_ x -> x)
