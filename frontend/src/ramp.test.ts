import { describe, expect, it } from 'vitest'
import { compileRamp, type RampMode } from './ramp'
import { readVector } from './test/fixtures'
import type { Node } from './types'

interface RampVectors {
  ramps: { mode: RampMode; ramp: Node; samples: [number, number, number, number, number][] }[]
}

const vectors = readVector<RampVectors>('ramps.json')

describe('compileRamp matches the Haskell ramp evaluator', () => {
  vectors.ramps.forEach(({ mode, ramp, samples }, index) => {
    it(`ramp ${index} (${ramp.type}, ${mode})`, () => {
      const f = compileRamp(ramp, mode)
      for (const [t, r, g, b, a] of samples) {
        const c = f(t)
        const close = [c.r - r, c.g - g, c.b - b, c.a - a].every((d) => Math.abs(d) <= 1e-12)
        if (!close) expect({ t, got: c }).toEqual({ t, got: { r, g, b, a } })
      }
    })
  })

  it('has vectors to check', () => {
    expect(vectors.ramps.length).toBeGreaterThan(10)
  })
})
