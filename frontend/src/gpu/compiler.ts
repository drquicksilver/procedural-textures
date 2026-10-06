import type { Rgba } from '../colour'
import type { TextureDocument } from '../types'
import { resolveMaterial, type Material, type ResolvedRamp, type RampMode } from './material'
import { ParameterWriter, NOISE } from './parameters'
import { compileGeometry, shapeDefinitions, type DistanceNode } from './geometry'
import { helpers, mainShader, noiseLookup } from './shaders'

export type Diagnostic = 'material' | 'noise' | 'distance'
export interface CompiledMaterial { source: string; parameters: Float32Array }
export interface CompileOptions { diagnostic?: Diagnostic; renderMode?: 'slice' | 'scene'; shape?: string; geometry?: DistanceNode }

/** Compile the current texture model; numerical edits live in the data texture. */
export function compileMaterial(document: TextureDocument, options: CompileOptions = {}): CompiledMaterial {
  const geometry = options.geometry ?? shapeDefinitions[options.shape ?? 'bitten-cube']
  if (!geometry) throw new Error(`Unknown shape: ${options.shape}`)
  const geometrySource = compileGeometry(geometry)
  const parameters = new ParameterWriter(noiseLookup)
  const functions: string[] = []
  let nodes = 0
  const slot = (values: readonly number[]) => parameters.slot(values)
  const vector = (value: readonly number[]): string => `data(${slot(value)}).xyz`
  const colourSlot = (colour: Rgba, lab = false): number => lab ? parameters.labColour(colour) : parameters.colour(colour)
  const rampExpression = (n: { ramp: ResolvedRamp; mode: RampMode }, value: string): string => {
    const r = n.ramp
    const mode = slot([n.mode === 'wrap' ? 1 : n.mode === 'mirror' ? 2 : 0])
    if (r.type === 'sinusoidal') {
      const from = colourSlot(r.from, true), to = colourSlot(r.to, true)
      return `mixLab(data(${from}),data(${to}),easeSinusoidal(rampMode(${value},0.0,1.0,int(data(${mode}).x))))`
    }
    if (r.stops.length === 0) return 'vec4(0,0,0,1)'
    if (r.stops.length > 128) throw new Error('GPU supports at most 128 stops')
    const start = parameters.length
    for (const [i, stop] of r.stops.entries()) parameters.rampStop(stop.position, stop.colour, r.stops[i + 1]?.colour)
    return `ramp(${value},${start},${r.stops.length},int(data(${mode}).x))`
  }
  const node = (n: Material, path = '$.texture'): string => {
    if (path.split('.').length > 66) throw new Error(`${path}: texture nesting exceeds 64`)
    if (++nodes > 200) throw new Error('GPU supports at most 200 texture nodes')
    const name = `material${nodes}`
    let body: string
    const call = (name: string, point = 'p', cache = 'cachedWarp,cachedConfig,cacheValid'): string => `${name}(${point},${cache})`
    try { switch (n.type) {
      case 'flat': body = `return data(${colourSlot(n.colour)});`; break
      case 'linear': {
        const origin = n.from, direction = n.to.map((v, i) => v - origin[i])
        const len2 = direction.reduce((sum, v) => sum + v * v, 0)
        const from = vector(origin), gradient = vector(direction.map((v) => len2 <= 0 ? 0 : v / len2))
        body = `float t=dot(p-${from},${gradient}); return ${rampExpression(n, 't')};`
        break
      }
      case 'radial': {
        const centre = vector(n.centre), axis = vector(n.axis)
        body = `
          vec3 axis=safeNormalise(${axis});
          vec3 north=vec3(0,-1,0)-dot(vec3(0,-1,0),axis)*axis;
          if (length(north)<1e-9) north=vec3(0,0,1)-dot(vec3(0,0,1),axis)*axis;
          north=safeNormalise(north);
          vec3 delta=p-${centre};
          vec3 radial=delta-dot(delta,axis)*axis;
          float len=length(radial);
          float t=len<=0.0 ? 0.5 : (1.0-dot(north,radial)/len)/2.0;
          return ${rampExpression(n, 't')};
        `
        break
      }
      case 'circular': {
        const centre = vector(n.centre), radius = slot([n.radius])
        body = `float radius=data(${radius}).x; float t=radius<=0.0 ? 0.0 : length(p-${centre})/radius; return ${rampExpression(n, 't')};`
        break
      }
      case 'perlin': body = `return ${rampExpression(n, `noise3(p*${vector(n.scale)})`)};`; break
      case 'fbm': {
        const scale = vector(n.scale), config = parameters.noise(n, n.style)
        body = `return ${rampExpression(n, `fractal(p*${scale},${config},false)`)};`
        break
      }
      case 'turbulence': {
        const base = node(n.base, `${path}.base`), amount = slot([n.amount]), config = parameters.noise(n, 'turbulence')
        body = `
          vec3 warp;
          if (cacheValid && all(equal(cachedConfig,data(${config + NOISE.identity})))) {
            warp=cachedWarp;
          } else {
            warp=rawWarp(p,${config});
            if (!cacheValid) {
              cachedWarp=warp;
              cachedConfig=data(${config + NOISE.identity});
              cacheValid=true;
            }
          }
          // A displaced child starts a new coordinate domain.
          vec3 childWarp=vec3(0);
          vec4 childConfig=vec4(0);
          bool childValid=false;
          return ${call(base, `p+data(${amount}).x*warp`, 'childWarp,childConfig,childValid')};
        `
        break
      }
      case 'tiled': {
        const a = node(n.a, `${path}.a`), b = node(n.b, `${path}.b`), counts = slot([n.columns, n.rows, n.depth].map((v) => Math.max(1, v)))
        body = `vec3 cell=floor(p*data(${counts}).xyz); return mod(cell.x+cell.y+cell.z,2.0)==0.0 ? ${call(a)} : ${call(b)};`
        break
      }
      case 'layer': {
        const top = node(n.top, `${path}.top`), bottom = node(n.bottom, `${path}.bottom`)
        body = `
          vec4 top=${call(top)};
          if (top.a==1.0) return top;
          vec4 bottom=${call(bottom)};
          return over(top,bottom);
        `
        break
      }
      default: return assertNever(n)
    } } catch (error) {
      if (error instanceof Error && error.message.startsWith('$.texture')) throw error
      throw new Error(`${path}: ${error instanceof Error ? error.message : error}`)
    }
    functions.push(`// ${path}: ${n.type}\nvec4 ${name}(vec3 p,inout vec3 cachedWarp,inout vec4 cachedConfig,inout bool cacheValid) { ${body} }`)
    return name
  }
  const root = node(resolveMaterial(document))
  const main = options.diagnostic ? `
uniform highp sampler2D samplePoints;
void main() {
  vec3 p=texelFetch(samplePoints,ivec2(int(gl_FragCoord.x),0),0).xyz;
  outputColour=${options.diagnostic === 'noise' ? 'vec4(noise3(p),0,0,1)' : options.diagnostic === 'distance' ? 'vec4(solid(p),0,0,1)' : 'material(p)'};
}` : mainShader('material', options.renderMode)
  // The first visible warp lazily populates this coordinate-domain cache.
  // Explicit inout arguments carry it across branches without fragment arrays.
  const warpSource = `vec3 rawWarp(vec3 p,int config) { return vec3(fractal(p,config,true),fractal(p+vec3(19.1,7.7,3.3),config,true),fractal(p+vec3(5.2,13.8,29.6),config,true))-0.5; }\n`
  const entry = `vec4 material(vec3 p) { vec3 warp=vec3(0); vec4 config=vec4(0); bool valid=false; return ${root}(p,warp,config,valid); }\n`
  return { source: helpers + geometrySource + warpSource + functions.join('\n') + entry + main, parameters: parameters.finish() }
}

function assertNever(value: never): never { throw new Error(`Unimplemented material: ${String(value)}`) }
