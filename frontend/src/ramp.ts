// A faithful port of ColourRamps.compileRamp (blending in OKLab, see
// oklab.ts), so ramp previews in the editor update instantly without a
// server round trip. test-vectors/ramps.json,
// produced by the Haskell test suite, keeps the two implementations in step.

import { BLACK, parseColour, type Rgba } from './colour'
import { fromLab, mixLab, toLab, type Lab } from './oklab'
import type { Json, Node } from './types'

interface Stop {
  position: number
  colour: Rgba
}

/** A stop with its colour also in OKLab, computed once per ramp for blending. */
interface LabStop extends Stop {
  lab: Lab
}

export function rampStops(ramp: Node): Stop[] {
  const raw = Array.isArray(ramp.stops) ? ramp.stops : []
  return raw.flatMap((s) =>
    typeof s === 'object' && s !== null && !Array.isArray(s) && typeof s.position === 'number'
      ? [{ position: s.position, colour: parseColour(s.colour) }]
      : [],
  )
}

/** What an unresolved reference evaluates to, matching the Haskell renderer. */
const UNRESOLVED: Rgba = { r: 1, g: 0, b: 1, a: 1 }

/** How values beyond a ramp's span map back into it; chosen where the ramp is used. */
export type RampMode = 'clamp' | 'wrap' | 'mirror'

export function asMode(value: Json | undefined): RampMode {
  return value === 'wrap' || value === 'mirror' ? value : 'clamp'
}

/**
 * Evaluate a concrete ramp, used with the given mode. References must be
 * resolved first (see rampRefs).
 */
export function compileRamp(ramp: Node, mode: RampMode = 'clamp'): (t: number) => Rgba {
  if (ramp.type === 'named' || ramp.type === 'builtin') return () => UNRESOLVED
  if (ramp.type === 'sinusoidal') {
    const from = toLab(parseColour(ramp.from))
    const to = toLab(parseColour(ramp.to))
    return (t) => {
      const eased = 0.5 - 0.5 * Math.cos(Math.PI * applyMode(mode, 0, 1, 1, t))
      return fromLab(mixLab(eased, from, to))
    }
  }
  // Array.prototype.sort is stable, so stops at the same position keep their order.
  const stops = rampStops(ramp)
    .sort((a, b) => a.position - b.position)
    .map((s) => ({ ...s, lab: toLab(s.colour) }))
  const minPos = stops.length > 0 ? stops[0].position : 0
  const maxPos = stops.length > 0 ? stops[stops.length - 1].position : 0
  const span = maxPos - minPos
  return (t) => evalStops(stops, applyMode(mode, minPos, maxPos, span, t))
}

function applyMode(mode: RampMode, minPos: number, maxPos: number, span: number, t: number): number {
  switch (mode) {
    case 'wrap':
      return wrap(minPos, span, t)
    case 'mirror': {
      const wrapped = wrap(minPos, span * 2, t)
      const offset = wrapped - minPos
      const mirrored = offset <= span ? offset : span * 2 - offset
      return minPos + mirrored
    }
    default:
      return t < minPos ? minPos : t > maxPos ? maxPos : t
  }
}

function wrap(minPos: number, span: number, t: number): number {
  if (span <= 0) return minPos
  const offset = t - minPos
  return minPos + (offset - Math.floor(offset / span) * span)
}

function evalStops(stops: LabStop[], t: number): Rgba {
  if (stops.length === 0) return BLACK
  if (stops[0].position > t) return stops[0].colour
  let lower = stops[0]
  for (let i = 1; i < stops.length; i++) {
    const next = stops[i]
    if (next.position <= t) {
      lower = next
      continue
    }
    if (lower.position === t) return lower.colour
    return fromLab(mixLab((t - lower.position) / (next.position - lower.position), lower.lab, next.lab))
  }
  return lower.colour
}
