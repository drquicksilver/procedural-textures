import type { Json, Node, TextureDocument } from '../types'
import { parseColour } from '../colour'
import { toLab } from '../oklab'
import { helpers, mainShader } from './shaders'

export interface CompiledMaterial { source: string; parameters: Float32Array }

/** Spike compiler: concrete ramps and the constructors used by its three examples. */
export function compileMaterial(document: TextureDocument): CompiledMaterial {
  const values: number[] = [], functions: string[] = []
  let nodes = 0
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
  const vector = (v: Json | undefined): string => {
    if (!Array.isArray(v) || v.length !== 3) throw new Error('Expected 3-vector')
    return `data(${slot(v.map(scalar))}).xyz`
  }
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
    if (!Number.isInteger(count) || count < 1 || count > 32) throw new Error('Spike supports 1–32 octaves')
    let total = 0, amplitude = 1
    for (let i = 0; i < count; i++) { total += amplitude; amplitude *= persistence }
    // Turbulence uses this closed-form normalisation in the reference.
    if (n.type === 'turbulence') total = persistence === 1 ? count : (1 - persistence ** count) / (1 - persistence)
    const start = slot([count, persistence, total, n.style === 'billowy' ? 1 : n.style === 'ridged' ? 2 : 0])
    for (let i = 0; i < 32; i++) {
      const a = i * 0.83, frequency = i < count ? lacunarity ** i : 0
      for (const [x, y, z] of [[1, 0, 0], [0, 1, 0], [0, 0, 1]]) {
        const u = x * Math.cos(a) - y * Math.sin(a), v = x * Math.sin(a) + y * Math.cos(a)
        const w = v * Math.cos(a * 0.71) - z * Math.sin(a * 0.71), q = v * Math.sin(a * 0.71) + z * Math.cos(a * 0.71)
        slot([frequency * (u * Math.cos(a * 0.53) + q * Math.sin(a * 0.53)), frequency * w, frequency * (q * Math.cos(a * 0.53) - u * Math.sin(a * 0.53))])
      }
    }
    return start
  }
  const rampExpression = (n: Node, value: string): string => {
    let r = child(n.ramp)
    if (r.type === 'named') {
      const named = document.ramps?.[String(r.name)]
      if (!named) throw new Error(`Missing named ramp ${r.name}`)
      r = named
    }
    const mode = slot([n.mode === 'wrap' ? 1 : n.mode === 'mirror' ? 2 : 0])
    if (r.type === 'sinusoidal') {
      const from = colourSlot(r.from, true), to = colourSlot(r.to, true)
      return `mixLab(data(${from}),data(${to}),0.5-0.5*cos(3.141592653589793*rampMode(${value},0.0,1.0,int(data(${mode}).x))))`
    }
    if (r.type !== 'stops' || !Array.isArray(r.stops)) throw new Error(`Unsupported spike ramp: ${r.type}`)
    const stops = r.stops.map((v) => {
      if (!v || typeof v !== 'object' || Array.isArray(v)) throw new Error('Expected ramp stop')
      return { position: scalar(v.position), colour: v.colour }
    }).sort((a, b) => a.position - b.position)
    if (stops.length === 0) return 'vec4(0,0,0,1)'
    if (stops.length > 128) throw new Error('Spike supports at most 128 stops')
    const start = values.length / 4
    for (const stop of stops) { slot([stop.position]); colourSlot(stop.colour); colourSlot(stop.colour, true) }
    return `ramp(${value},${start},${stops.length},int(data(${mode}).x))`
  }
  const node = (n: Node): string => {
    if (++nodes > 200) throw new Error('Spike supports at most 200 texture nodes')
    const name = `material${nodes}`
    let body: string
    switch (n.type) {
      case 'flat': body = `return data(${colourSlot(n.colour)});`; break
      case 'linear': {
        const from = vector(n.from), to = vector(n.to)
        body = `vec3 direction=${to}-${from}; float len2=dot(direction,direction); float t=len2<=0.0 ? 0.0 : dot(p-${from},direction)/len2; return ${rampExpression(n, 't')};`
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
        const base = node(child(n.base)), amount = slot([scalar(n.amount)]), config = noiseConfig(n)
        body = `vec3 warp=vec3(fractal(p,${config},true),fractal(p+vec3(19.1,7.7,3.3),${config},true),fractal(p+vec3(5.2,13.8,29.6),${config},true))-0.5; return ${base}(p+data(${amount}).x*warp);`
        break
      }
      case 'tiled': {
        const a = node(child(n.a)), b = node(child(n.b)), counts = slot([n.columns, n.rows, n.depth].map((v) => Math.max(1, scalar(v))))
        body = `vec3 cell=floor(p*data(${counts}).xyz); return mod(cell.x+cell.y+cell.z,2.0)==0.0 ? ${a}(p) : ${b}(p);`
        break
      }
      case 'layer': {
        const top = node(child(n.top)), bottom = node(child(n.bottom))
        body = `vec4 top=${top}(p); if (top.a==1.0) return top; return over(top,${bottom}(p));`
        break
      }
      default: throw new Error(`Unsupported spike texture: ${n.type}`)
    }
    functions.push(`vec4 ${name}(vec3 p) { ${body} }`)
    return name
  }
  const root = node(document.texture)
  const parameters = new Float32Array(Math.ceil(Math.max(1, values.length / 4) / 256) * 256 * 4)
  parameters.set(values)
  return { source: helpers + functions.join('\n') + mainShader(root), parameters }
}
