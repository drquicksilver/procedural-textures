import type { Json, Node, TextureDocument } from '../types'
import { parseColour } from '../colour'
import { toLab } from '../oklab'
import { compileGeometry, shapeDefinitions, type DistanceNode } from './geometry'
import { helpers, mainShader, noiseLookup } from './shaders'

export type Diagnostic = 'material' | 'noise' | 'distance'
export interface CompiledMaterial { source: string; parameters: Float32Array }
export interface CompileOptions { diagnostic?: Diagnostic; renderMode?: 'slice' | 'scene'; shape?: string; geometry?: DistanceNode }

import metadata from '../metadata'
const builtinRamps = Object.fromEntries(metadata.ramps.map((r) => [r.id, r.ramp as Node]))

/** Compile the current texture model; numerical edits live in the data texture. */
export function compileMaterial(document: TextureDocument, options: CompileOptions = {}): CompiledMaterial {
  const geometry = options.geometry ?? shapeDefinitions[options.shape ?? 'bitten-cube']
  if (!geometry) throw new Error(`Unknown shape: ${options.shape}`)
  const geometrySource = compileGeometry(geometry)
  const values: number[] = Array.from(noiseLookup), functions: string[] = []
  let nodes = 0
  const noiseKeys = new Map<string, number>()
  const domains = new Map<string, number[]>()
  const slot = (v: number[]): number => {
    if (v.some((n) => !Number.isFinite(n) || !Number.isFinite(Math.fround(n)))) throw new Error('Non-finite GPU parameter')
    const index = values.length / 4
    values.push(...Array.from({ length: 4 }, (_, i) => v[i] ?? 0))
    return index
  }
  const scalar = (v: Json | undefined): number => {
    if (typeof v !== 'number' || !Number.isFinite(v)) throw new Error('Expected finite scalar')
    return v
  }
  const readVector = (v: Json | undefined): number[] => {
    if (!Array.isArray(v) || v.length !== 3) throw new Error('Expected 3-vector')
    return v.map(scalar)
  }
  const vector = (v: Json | undefined): string => `data(${slot(readVector(v))}).xyz`
  const child = (v: Json | undefined): Node => {
    if (!v || typeof v !== 'object' || Array.isArray(v) || typeof v.type !== 'string') throw new Error('Expected node')
    return v as Node
  }
  const colourSlot = (v: Json | undefined, lab = false): number => {
    const colour = parseColour(v ?? null)
    if (!lab) return slot([colour.r, colour.g, colour.b, colour.a])
    const c = toLab(colour)
    return slot([c.l, c.a, c.b, c.alpha])
  }
  const noiseConfig = (n: Node): number => {
    const count = scalar(n.octaves), persistence = scalar(n.persistence), lacunarity = scalar(n.lacunarity)
    if (!Number.isInteger(count) || count < 1 || count > 32) throw new Error('GPU supports 1–32 octaves')
    let total = 0, amplitude = 1
    for (let i = 0; i < count; i++) { total += amplitude; amplitude *= persistence }
    // Turbulence uses this closed-form normalisation in the reference.
    if (n.type === 'turbulence') total = persistence === 1 ? count : (1 - persistence ** count) / (1 - persistence)
    const start = slot([count, persistence, total, n.type === 'turbulence' ? lacunarity : n.style === 'billowy' ? 1 : n.style === 'ridged' ? 2 : 0])
    for (let i = 0; i < 32; i++) {
      const a = i * 0.83, frequency = i < count ? lacunarity ** i : 0
      for (const [x, y, z] of [[1, 0, 0], [0, 1, 0], [0, 0, 1]]) {
        const u = x * Math.cos(a) - y * Math.sin(a), v = x * Math.sin(a) + y * Math.cos(a)
        const w = v * Math.cos(a * 0.71) - z * Math.sin(a * 0.71), q = v * Math.sin(a * 0.71) + z * Math.cos(a * 0.71)
        slot([frequency * (u * Math.cos(a * 0.53) + q * Math.sin(a * 0.53)), frequency * w, frequency * (q * Math.cos(a * 0.53) - u * Math.sin(a * 0.53))])
      }
    }
    // Compare exact host configurations, not rounded FP32 values: very close
    // lacunarities can round alike while their higher octave matrices differ.
    const key = JSON.stringify([count, persistence, lacunarity])
    if (!noiseKeys.has(key)) noiseKeys.set(key, noiseKeys.size + 1)
    slot([noiseKeys.get(key)!]) // stable offset start+97; edits only update data
    return start
  }
  const rampExpression = (n: Node, value: string): string => {
    let r = child(n.ramp)
    if (r.type === 'named') {
      const named = document.ramps?.[String(r.name)]
      if (!named) throw new Error(`Missing named ramp ${r.name}`)
      r = named
    }
    if (r.type === 'builtin') {
      const builtin = builtinRamps[String(r.name)]
      if (!builtin) throw new Error(`Missing built-in ramp ${r.name}`)
      r = builtin
    }
    const mode = slot([n.mode === 'wrap' ? 1 : n.mode === 'mirror' ? 2 : 0])
    if (r.type === 'sinusoidal') {
      const from = colourSlot(r.from, true), to = colourSlot(r.to, true)
      return `mixLab(data(${from}),data(${to}),easeSinusoidal(rampMode(${value},0.0,1.0,int(data(${mode}).x))))`
    }
    if (r.type !== 'stops' || !Array.isArray(r.stops)) throw new Error(`Unsupported ramp: ${r.type}`)
    const stops = r.stops.map((v) => {
      if (!v || typeof v !== 'object' || Array.isArray(v)) throw new Error('Expected ramp stop')
      return { position: scalar(v.position), colour: v.colour }
    }).sort((a, b) => a.position - b.position)
    if (stops.length === 0) return 'vec4(0,0,0,1)'
    if (stops.length > 128) throw new Error('GPU supports at most 128 stops')
    const start = values.length / 4
    for (const stop of stops) { slot([stop.position]); colourSlot(stop.colour); colourSlot(stop.colour, true) }
    return `ramp(${value},${start},${stops.length},int(data(${mode}).x))`
  }
  const node = (n: Node, path = '$.texture'): string => {
    if (path.split('.').length > 66) throw new Error(`${path}: texture nesting exceeds 64`)
    if (++nodes > 200) throw new Error('GPU supports at most 200 texture nodes')
    const name = `material${nodes}`
    let body: string
    let configs: number[] = []
    const call = (name: string, point = 'p', valid = true): string => `${name}(${point},${valid ? 'cachedWarp,cachedConfig,cacheValid' : 'vec3(0),vec4(0),false'})`
    try { switch (n.type) {
      case 'flat': body = `return data(${colourSlot(n.colour)});`; break
      case 'linear': {
        const origin = readVector(n.from), direction = readVector(n.to).map((v, i) => v - origin[i])
        const len2 = direction.reduce((sum, v) => sum + v * v, 0)
        const from = vector(origin), gradient = vector(direction.map((v) => len2 <= 0 ? 0 : v / len2))
        body = `float t=dot(p-${from},${gradient}); return ${rampExpression(n, 't')};`
        break
      }
      case 'radial': {
        const centre = vector(n.centre), axis = vector(n.axis)
        body = `vec3 axis=safeNormalise(${axis}); vec3 north=vec3(0,-1,0)-dot(vec3(0,-1,0),axis)*axis; if(length(north)<1e-9) north=vec3(0,0,1)-dot(vec3(0,0,1),axis)*axis; north=safeNormalise(north); vec3 delta=p-${centre}; vec3 radial=delta-dot(delta,axis)*axis; float len=length(radial); float t=len<=0.0 ? 0.5 : (1.0-dot(north,radial)/len)/2.0; return ${rampExpression(n, 't')};`
        break
      }
      case 'circular': {
        const centre = vector(n.centre), radius = slot([scalar(n.radius)])
        body = `float radius=data(${radius}).x; float t=radius<=0.0 ? 0.0 : length(p-${centre})/radius; return ${rampExpression(n, 't')};`
        break
      }
      case 'perlin': body = `return ${rampExpression(n, `noise3(p*${vector(n.scale)})`)};`; break
      case 'fbm': {
        const scale = vector(n.scale), config = noiseConfig(n)
        body = `return ${rampExpression(n, `fractal(p*${scale},${config},false)`)};`
        break
      }
      case 'turbulence': {
        const base = node(child(n.base), `${path}.base`), amount = slot([scalar(n.amount)]), config = noiseConfig(n)
        configs = [config]
        body = `vec3 warp=cacheValid && all(equal(cachedConfig,data(${config + 97}))) ? cachedWarp : rawWarp(p,${config}); return ${call(base, `p+data(${amount}).x*warp`, false)};`
        break
      }
      case 'tiled': {
        const a = node(child(n.a), `${path}.a`), b = node(child(n.b), `${path}.b`), counts = slot([n.columns, n.rows, n.depth].map((v) => Math.max(1, scalar(v))))
        body = `vec3 cell=floor(p*data(${counts}).xyz); return mod(cell.x+cell.y+cell.z,2.0)==0.0 ? ${call(a)} : ${call(b)};`
        break
      }
      case 'layer': {
        const top = node(child(n.top), `${path}.top`), bottom = node(child(n.bottom), `${path}.bottom`)
        configs = [...domains.get(top)!, ...domains.get(bottom)!]
        const shared = configs.length > 1 ? `if(!cacheValid) { cachedConfig=data(${configs[0] + 97}); cachedWarp=rawWarp(p,${configs[0]}); cacheValid=true; } ` : ''
        body = `${shared}vec4 top=${call(top)}; if (top.a==1.0) return top; return over(top,${call(bottom)});`
        break
      }
      default: throw new Error(`Unsupported texture: ${n.type}`)
    } } catch (error) {
      if (error instanceof Error && error.message.startsWith('$.texture')) throw error
      throw new Error(`${path}: ${error instanceof Error ? error.message : error}`)
    }
    domains.set(name, configs)
    functions.push(`// ${path}: ${n.type}\nvec4 ${name}(vec3 p,vec3 cachedWarp,vec4 cachedConfig,bool cacheValid) { ${body} }`)
    return name
  }
  const root = node(document.texture)
  const parameters = new Float32Array(Math.ceil(Math.max(1, values.length / 4) / 256) * 256 * 4)
  parameters.set(values)
  const main = options.diagnostic ? `
uniform highp sampler2D samplePoints;
void main() {
  vec3 p=texelFetch(samplePoints,ivec2(int(gl_FragCoord.x),0),0).xyz;
  outputColour=${options.diagnostic === 'noise' ? 'vec4(noise3(p),0,0,1)' : options.diagnostic === 'distance' ? 'vec4(solid(p),0,0,1)' : 'material(p)'};
}` : mainShader('material', options.renderMode)
  // Share the first warp configuration across Layer branches in one domain.
  // Runtime equality keeps this valid through numerical edits; a warp's child
  // starts a fresh domain. Explicit arguments avoid mutable fragment arrays.
  const warpSource = `vec3 rawWarp(vec3 p,int config) { return vec3(fractal(p,config,true),fractal(p+vec3(19.1,7.7,3.3),config,true),fractal(p+vec3(5.2,13.8,29.6),config,true))-0.5; }\n`
  const entry = `vec4 material(vec3 p) { return ${root}(p,vec3(0),vec4(0),false); }\n`
  return { source: helpers + geometrySource + warpSource + functions.join('\n') + entry + main, parameters }
}
