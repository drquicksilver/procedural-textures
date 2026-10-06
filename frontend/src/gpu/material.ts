import metadata from '../metadata'
import { processDocument } from '../document'
import { parseColour, type Rgba } from '../colour'
import type { Json, Node, TextureDocument } from '../types'

export type Vector3 = [number, number, number]
export type RampMode = 'clamp' | 'wrap' | 'mirror'
export type NoiseStyle = 'smooth' | 'billowy' | 'ridged'
export type ResolvedRamp =
  | { type: 'stops'; stops: { position: number; colour: Rgba }[] }
  | { type: 'sinusoidal'; from: Rgba; to: Rgba }
export interface NoiseConfiguration { octaves: number; persistence: number; lacunarity: number }
type Mapped = { mode: RampMode; ramp: ResolvedRamp }
export type Material =
  | { type: 'flat'; colour: Rgba }
  | ({ type: 'linear'; from: Vector3; to: Vector3 } & Mapped)
  | ({ type: 'radial'; centre: Vector3; axis: Vector3 } & Mapped)
  | ({ type: 'circular'; centre: Vector3; radius: number } & Mapped)
  | ({ type: 'perlin'; scale: Vector3 } & Mapped)
  | ({ type: 'fbm'; scale: Vector3; style: NoiseStyle } & NoiseConfiguration & Mapped)
  | ({ type: 'turbulence'; amount: number; base: Material } & NoiseConfiguration)
  | { type: 'tiled'; columns: number; rows: number; depth: number; a: Material; b: Material }
  | { type: 'layer'; top: Material; bottom: Material }

function node(value: Json): Node {
  if (!value || typeof value !== 'object' || Array.isArray(value) || typeof value.type !== 'string') throw new Error('Expected node')
  return value as Node
}
function scalar(value: Json): number {
  if (typeof value !== 'number' || !Number.isFinite(value)) throw new Error('Expected finite scalar')
  return value
}
function vector(value: Json): Vector3 {
  if (!Array.isArray(value) || value.length !== 3) throw new Error('Expected 3-vector')
  return [scalar(value[0]), scalar(value[1]), scalar(value[2])]
}
function mode(value: Json): RampMode {
  if (value === 'clamp' || value === 'wrap' || value === 'mirror') return value
  throw new Error('Unknown ramp mode')
}
function style(value: Json): NoiseStyle {
  if (value === 'smooth' || value === 'billowy' || value === 'ridged') return value
  throw new Error('Unknown noise style')
}

/** The editor keeps generic JSON. Only validated, concrete nodes cross into the
 * compiler; this representation describes today's language, not the Phase 4 model.
 */
export function resolveMaterial(input: TextureDocument): Material {
  const document = processDocument(input)
  const builtin = new Map(metadata.ramps.map((r) => [r.id, r.ramp]))
  const ramp = (value: Json): ResolvedRamp => {
    const r = node(value)
    if (r.type === 'named') return ramp(document.ramps![String(r.name)])
    if (r.type === 'builtin') return ramp(builtin.get(String(r.name))!)
    if (r.type === 'sinusoidal') return { type: r.type, from: parseColour(r.from), to: parseColour(r.to) }
    if (r.type === 'stops' && Array.isArray(r.stops)) return {
      type: r.type,
      stops: r.stops.map((value) => {
        if (!value || typeof value !== 'object' || Array.isArray(value)) throw new Error('Expected ramp stop')
        return { position: scalar(value.position), colour: parseColour(value.colour) }
      }).sort((a, b) => a.position - b.position),
    }
    throw new Error(`Unsupported ramp: ${r.type}`)
  }
  const mapped = (n: Node): Mapped => ({ mode: mode(n.mode), ramp: ramp(n.ramp) })
  const noise = (n: Node): NoiseConfiguration => ({ octaves: scalar(n.octaves), persistence: scalar(n.persistence), lacunarity: scalar(n.lacunarity) })
  let count = 0
  const material = (n: Node, path: string, depth: number): Material => {
    if (depth > 64) throw new Error(`${path}: texture nesting exceeds 64`)
    if (++count > 200) throw new Error('GPU supports at most 200 texture nodes')
    const child = (key: string) => material(node(n[key]), `${path}.${key}`, depth + 1)
    switch (n.type) {
      case 'flat': return { type: n.type, colour: parseColour(n.colour) }
      case 'linear': return { type: n.type, from: vector(n.from), to: vector(n.to), ...mapped(n) }
      case 'radial': return { type: n.type, centre: vector(n.centre), axis: vector(n.axis), ...mapped(n) }
      case 'circular': return { type: n.type, centre: vector(n.centre), radius: scalar(n.radius), ...mapped(n) }
      case 'perlin': return { type: n.type, scale: vector(n.scale), ...mapped(n) }
      case 'fbm': return { type: n.type, scale: vector(n.scale), style: style(n.style), ...noise(n), ...mapped(n) }
      case 'turbulence': return { type: n.type, amount: scalar(n.amount), base: child('base'), ...noise(n) }
      case 'tiled': return { type: n.type, columns: scalar(n.columns), rows: scalar(n.rows), depth: scalar(n.depth), a: child('a'), b: child('b') }
      case 'layer': return { type: n.type, top: child('top'), bottom: child('bottom') }
      default: throw new Error(`${path}: Unsupported texture: ${n.type}`)
    }
  }
  return material(document.texture, '$.texture', 0)
}
