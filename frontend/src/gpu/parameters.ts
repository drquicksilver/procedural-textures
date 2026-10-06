import type { Rgba } from '../colour'
import { fromLab, mixLab, toLab } from '../oklab'
import type { NoiseConfiguration, NoiseStyle } from './material'

export const PARAMETER_WIDTH = 256
export const RAMP_STOP = { position: 0, rgba: 1, lab: 2, stride: 3 } as const
const MAX_OCTAVES = 32, MATRIX_STRIDE = 3
const NOISE_IDENTITY = 1 + MAX_OCTAVES * MATRIX_STRIDE
export const NOISE = { matrices: 1, matrixStride: MATRIX_STRIDE, maxOctaves: MAX_OCTAVES, identity: NOISE_IDENTITY, slots: NOISE_IDENTITY + 1 } as const

/** GLSL decoding and host packing share these declarations. Lane meanings:
 * ramp position = [position, cached interior RGB]; rgba = original stop;
 * lab = [L,a,b,constant-span flag]. Interior alpha clamps original alpha.
 * noise header = [octaves,persistence,total,style or turbulence lacunarity];
 * 32 matrices follow; identity stores an exact-host-configuration key.
 */
export const parameterLayoutGlsl = `
const int PARAMETER_WIDTH = ${PARAMETER_WIDTH};
const int RAMP_STOP_STRIDE = ${RAMP_STOP.stride};
const int RAMP_RGBA = ${RAMP_STOP.rgba};
const int RAMP_LAB = ${RAMP_STOP.lab};
const int NOISE_MATRICES = ${NOISE.matrices};
const int NOISE_MATRIX_STRIDE = ${NOISE.matrixStride};
`

function roundEven(value: number): number {
  const lower = Math.floor(value), fraction = value - lower
  return fraction > 0.5 || (fraction === 0.5 && lower % 2 !== 0) ? lower + 1 : lower
}
/** Preserve the Double reference's byte-rounding side with at most one FP32 ULP. */
function referenceFloat(value: number): number {
  const rounded = Math.fround(value), target = roundEven(value * 255)
  if (roundEven(Math.fround(rounded * 255)) === target) return rounded
  const float = new Float32Array([rounded]), bits = new Uint32Array(float.buffer)
  bits[0] += target < roundEven(Math.fround(rounded * 255)) ? -1 : 1
  return float[0]
}

export class ParameterWriter {
  private readonly values: number[]
  private readonly noiseKeys = new Map<string, number>()
  constructor(lookup: Float32Array) { this.values = Array.from(lookup) }
  get length(): number { return this.values.length / 4 }
  slot(values: readonly number[]): number {
    if (values.length > 4 || values.some((n) => !Number.isFinite(n) || !Number.isFinite(Math.fround(n)))) throw new Error('Non-finite GPU parameter')
    const index = this.length
    this.values.push(...Array.from({ length: 4 }, (_, i) => values[i] ?? 0))
    return index
  }
  colour(colour: Rgba): number { return this.slot([colour.r, colour.g, colour.b, colour.a]) }
  labColour(colour: Rgba): number {
    const lab = toLab(colour)
    return this.slot([lab.l, lab.a, lab.b, lab.alpha])
  }
  rampStop(position: number, colour: Rgba, next?: Rgba): void {
    const lab = toLab(colour)
    const constant = next !== undefined && colour.r === next.r && colour.g === next.g && colour.b === next.b && colour.a === next.a
    const cached = constant ? fromLab(mixLab(0.5, lab, lab)) : null
    this.slot([position, ...(cached ? [cached.r, cached.g, cached.b].map(referenceFloat) : [])])
    this.colour(colour)
    this.slot([lab.l, lab.a, lab.b, constant ? 1 : 0])
  }
  noise(n: NoiseConfiguration, style: NoiseStyle | 'turbulence'): number {
    const { octaves: count, persistence, lacunarity } = n
    if (!Number.isInteger(count) || count < 1 || count > NOISE.maxOctaves) throw new Error('GPU supports 1–32 octaves')
    let total = 0, amplitude = 1
    for (let i = 0; i < count; i++) { total += amplitude; amplitude *= persistence }
    if (style === 'turbulence') total = persistence === 1 ? count : (1 - persistence ** count) / (1 - persistence)
    const start = this.slot([count, persistence, total, style === 'turbulence' ? lacunarity : style === 'billowy' ? 1 : style === 'ridged' ? 2 : 0])
    for (let i = 0; i < NOISE.maxOctaves; i++) {
      const angle = i * 0.83, frequency = i < count ? lacunarity ** i : 0
      for (const [x, y, z] of [[1, 0, 0], [0, 1, 0], [0, 0, 1]]) {
        const u = x * Math.cos(angle) - y * Math.sin(angle), v = x * Math.sin(angle) + y * Math.cos(angle)
        const w = v * Math.cos(angle * 0.71) - z * Math.sin(angle * 0.71), q = v * Math.sin(angle * 0.71) + z * Math.cos(angle * 0.71)
        this.slot([frequency * (u * Math.cos(angle * 0.53) + q * Math.sin(angle * 0.53)), frequency * w, frequency * (q * Math.cos(angle * 0.53) - u * Math.sin(angle * 0.53))])
      }
    }
    const key = JSON.stringify([count, persistence, lacunarity])
    if (!this.noiseKeys.has(key)) this.noiseKeys.set(key, this.noiseKeys.size + 1)
    this.slot([this.noiseKeys.get(key)!])
    return start
  }
  finish(): Float32Array {
    const output = new Float32Array(Math.ceil(Math.max(1, this.length) / PARAMETER_WIDTH) * PARAMETER_WIDTH * 4)
    output.set(this.values)
    return output
  }
}
