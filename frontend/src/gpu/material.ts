import {type ReactionConfig} from '../reaction'
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
export interface CellConfiguration { dimensions: 2 | 3; jitter: number; seed: number }
export type Layout = 'grid' | 'running-bond' | 'hex' | 'herringbone'
export type ScalarField =
  | {type: 'layout-edge'; layout: Layout}
  | {type: 'layout-value'; layout: Layout; seed: number}
  | ({ type: 'reaction-diffusion'; output: 'u' | 'v' } & ReactionConfig)
  | ({ type: 'worley'; metric: 'euclidean' | 'manhattan' | 'chebyshev'; output: 'f1' | 'f2' | 'gap' } & CellConfiguration)
  | ({ type: 'cell-value' | 'cell-edge' } & CellConfiguration)
  | { type: 'sphere'; centre: Vector3; radius: number }
  | { type: 'box'; centre: Vector3; half: Vector3 }
  | { type: 'cylinder'; centre: Vector3; radius: number; height: number }
  | { type: 'torus'; centre: Vector3; major: number; minor: number }
  | { type: 'plane'; normal: Vector3; offset: number }
  | { type: 'sdf-union' | 'sdf-intersection' | 'sdf-difference'; amount: number; a: ScalarField; b: ScalarField }
  | { type: 'constant'; value: number }
  | { type: 'planar'; from: Vector3; to: Vector3 }
  | { type: 'distance'; centre: Vector3; radius: number }
  | { type: 'angular'; centre: Vector3; axis: Vector3 }
  | { type: 'periodic-noise'; periodX: number; periodY: number; periodZ: number }
  | { type: 'periodic-fractal'; periodX: number; periodY: number; periodZ: number; octaves: number; persistence: number; lacunarity: number; style: NoiseStyle }
  | { type: 'noise' }
  | ({ type: 'fractal'; style: NoiseStyle; source: ScalarField } & NoiseConfiguration)
  | ({ type: 'absolute-fractal'; source: ScalarField } & NoiseConfiguration)
  | { type: 'scalar-domain'; domain: Domain; source: ScalarField }
  | { type: 'add' | 'multiply' | 'min' | 'max'; a: ScalarField; b: ScalarField }
  | { type: 'remap'; low: number; high: number; outLow: number; outHigh: number; source: ScalarField }
  | { type: 'sin' | 'cos' | 'abs' | 'floor' | 'fract'; source: ScalarField }
  | { type: 'divide' | 'power'; a: ScalarField; b: ScalarField }
  | { type: 'lerp'; a: ScalarField; b: ScalarField; amount: ScalarField }
  | { type: 'clamp'; low: number; high: number; source: ScalarField }
  | { type: 'azimuth'; centre: Vector3 }
  | { type: 'component'; axis: 'x' | 'y' | 'z'; source: VectorField }
  | { type: 'threshold'; low: number; high: number; source: ScalarField }
export type VectorField =
  | {type: 'layout-id' | 'layout-coordinates'; layout: Layout}
  | ({ type: 'cell-id' | 'cell-colour' } & CellConfiguration)
  | { type: 'vector-constant'; value: Vector3 }
  | { type: 'position' }
  | { type: 'components'; x: ScalarField; y: ScalarField; z: ScalarField }
  | { type: 'vector-add'; a: VectorField; b: VectorField }
  | { type: 'vector-scale'; amount: ScalarField; source: VectorField }
  | { type: 'vector-domain'; domain: Domain; source: VectorField }
export type Domain =
  | {type: 'layout-domain'; layout: Layout}
  | { type: 'translate'; offset: Vector3 }
  | { type: 'rotate'; rotation: Vector3 }
  | { type: 'scale'; scale: Vector3 }
  | { type: 'repeat'; period: Vector3 }
  | { type: 'mirror'; centre: Vector3; axes: Vector3 }
  | { type: 'polar-repeat'; centre: Vector3; count: number }
  | { type: 'radial-repeat'; centre: Vector3; period: number }
  | { type: 'twist' | 'bend'; centre: Vector3; amount: number }
  | { type: 'compose'; first: Domain; second: Domain }
  | { type: 'warp'; amount: number; field: VectorField }
export type BlendMode = 'normal' | 'multiply' | 'screen' | 'overlay' | 'soft-light' | 'darken' | 'lighten' | 'difference' | 'exclusion'
export const blendModes: BlendMode[] = ['normal','multiply','screen','overlay','soft-light','darken','lighten','difference','exclusion']
export type Material =
  | { type: 'scatter'; dimensions: number; seed: number; minScale: number; maxScale: number; rotation: number; density: ScalarField; source: Material }
  | { type: 'blend'; mode: BlendMode; opacity: number; top: Material; bottom: Material }
  | { type: 'flat'; colour: Rgba }
  | ({ type: 'colourise'; field: ScalarField } & Mapped)
  | { type: 'vector-colour'; field: VectorField }
  | { type: 'domain'; domain: Domain; base: Material }
  | { type: 'mix'; mask: ScalarField; a: Material; b: Material }
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
 * compiler. Scalar, vector and domain expressions are separate typed categories;
 * readable legacy colour nodes retain their specialised lowering kernels.
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
  const expression = (value: Json, path: string, depth: number): ScalarField | VectorField | Domain => {
    const n = node(value)
    if (depth > 64 || ++count > 200) throw new Error(`${path}: GPU expression limits exceeded`)
    const out: Record<string, unknown> = { type: n.type }
    for (const [key, value] of Object.entries(n)) {
      if (key === 'type') continue
      out[key] = value && typeof value === 'object' && !Array.isArray(value) && typeof value.type === 'string'
        ? expression(value, `${path}.${key}`, depth + 1) : value
    }
    // processDocument has validated these exact categories against reference metadata.
    return out as unknown as ScalarField | VectorField | Domain
  }
  const sf = (value: Json, path: string, depth: number) => expression(value, path, depth) as ScalarField
  const vf = (value: Json, path: string, depth: number) => expression(value, path, depth) as VectorField
  const df = (value: Json, path: string, depth: number) => expression(value, path, depth) as Domain
  const material = (n: Node, path: string, depth: number): Material => {
    if (depth > 64) throw new Error(`${path}: texture nesting exceeds 64`)
    if (++count > 200) throw new Error('GPU supports at most 200 texture nodes')
    const child = (key: string) => material(node(n[key]), `${path}.${key}`, depth + 1)
    switch (n.type) {
      case 'scatter': return {type:n.type,dimensions:scalar(n.dimensions),seed:scalar(n.seed),minScale:scalar(n.minScale),maxScale:scalar(n.maxScale),rotation:scalar(n.rotation),density:sf(n.density,`${path}.density`,depth+1),source:child('source') }
      case 'blend': return {type:n.type,mode:n.mode as BlendMode,opacity:scalar(n.opacity),top:child('top'),bottom:child('bottom')}
      case 'colourise': return { type: n.type, field: sf(n.field, `${path}.field`, depth+1), ...mapped(n) }
      case 'vector-colour': return { type: n.type, field: vf(n.field, `${path}.field`, depth+1) }
      case 'domain': return { type: n.type, domain: df(n.domain, `${path}.domain`, depth+1), base: child('base') }
      case 'mix': return { type: n.type, mask: sf(n.mask, `${path}.mask`, depth+1), a: child('a'), b: child('b') }
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
  const result = material(document.texture, '$.texture', 0)
  // Nested generic fractals multiply source evaluations. Bound expanded work,
  // including vector components and domains, before submitting a shader.
  const cost = (value: unknown): number => {
    if (!value || typeof value !== 'object' || Array.isArray(value)) return 0
    const n = value as Record<string, unknown>
    const children = Object.values(n).reduce<number>((sum, v) => sum + cost(v), 0)
    if(typeof n.type==='string'&&n.type.startsWith('layout-')) return n.layout==='hex' ? 9 : 1
    if(n.type==='scatter') return (n.dimensions===2 ? 9 : 27)*(1+cost(n.density))+cost(n.source)
    if (n.type === 'reaction-diffusion') return 8
    if(n.type==='periodic-fractal') return Number(n.octaves)
    if (n.type === 'noise' || n.type === 'perlin' || n.type==='periodic-noise') return 1
    if (n.type === 'worley' || n.type === 'cell-value' || n.type === 'cell-id' || n.type === 'cell-colour') return n.dimensions===2 ? 49 : 343
    if (n.type === 'cell-edge') return n.dimensions===2 ? 227 : 2567
    const octaves = Math.max(1, Math.min(32, Number(n.octaves)))
    if (n.type === 'fractal' || n.type === 'absolute-fractal') {
      if (Number(n.octaves) > 32) throw new Error('GPU supports at most 32 octaves')
      return octaves * Math.max(1, children)
    }
    return children + (n.type === 'fbm' ? octaves : n.type === 'turbulence' ? 3*octaves : 0)
  }
  if (cost(result) > 4096) throw new Error('GPU supports at most 4096 expanded noise samples per point')
  return result
}

/** Feedback scheduling only: numbers do not change programs. Actual validation
 * and cache lookup still happen in the renderer, which reports unsupported edits.
 */
export function materialStructure(document: TextureDocument): string {
  const expressionShape = (value: unknown): unknown => {
    if (!value || typeof value !== 'object' || Array.isArray(value)) return null
    const n = value as Record<string, unknown>
    return [n.type, ...Object.entries(n).filter(([,v]) => v && typeof v === 'object' && !Array.isArray(v)).map(([k,v]) => [k,expressionShape(v)])]
  }
  const shape = (n: Material): unknown => {
    switch (n.type) {
      case 'scatter': return [n.type,expressionShape(n.density),shape(n.source)]
      case 'colourise': return [n.type,expressionShape(n.field),n.ramp.type,n.ramp.type === 'stops' ? n.ramp.stops.length : null]
      case 'vector-colour': return [n.type,expressionShape(n.field)]
      case 'domain': return [n.type,expressionShape(n.domain),shape(n.base)]
      case 'mix': return [n.type,expressionShape(n.mask),shape(n.a),shape(n.b)]
      case 'flat': return [n.type]
      case 'linear': case 'radial': case 'circular': case 'perlin': case 'fbm':
        return [n.type, n.ramp.type, n.ramp.type === 'stops' ? n.ramp.stops.length : null]
      case 'turbulence': return [n.type, shape(n.base)]
      case 'blend': case 'layer': return [n.type, shape(n.top), shape(n.bottom)]
      case 'tiled': return [n.type, shape(n.a), shape(n.b)]
      default: return unreachable(n)
    }
  }
  try { return JSON.stringify(shape(resolveMaterial(document))) }
  catch { return 'unsupported' }
}
function unreachable(value: never): never { throw new Error(`Unknown material ${String(value)}`) }
