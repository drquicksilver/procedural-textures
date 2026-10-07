import type { Rgba } from '../colour'
import type { TextureDocument } from '../types'
import { resolveMaterial, type Material, type ScalarField, type VectorField, type Domain, type ResolvedRamp, type RampMode } from './material'
import { cellularHelpers } from './cellular'
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
  const fieldFunction = (type: 'float' | 'vec3', body: string, label: string): string => {
    const name = `field${++nodes}`
    if (nodes > 200) throw new Error('GPU supports at most 200 expression nodes')
    functions.push(`// ${label}\n${type} ${name}(vec3 p) { ${body} }`)
    return name
  }
  const cellConfig = (n: { dimensions: number; jitter: number; seed: number }) => `data(${slot([n.dimensions,n.jitter,n.seed & 65535,n.seed >>> 16])})`
  const scalarField = (n: ScalarField): string => {
    let body: string
    const sample = (source: ScalarField) => `${scalarField(source)}(p)`
    switch (n.type) {
      case 'worley': {
        const config=cellConfig(n), modes=slot([['euclidean','manhattan','chebyshev'].indexOf(n.metric),['f1','f2','gap'].indexOf(n.output)])
        body=`vec2 mode=data(${modes}).xy; CellSample s=cellular(p,${config},int(mode.x)); return mode.y==0.0 ? s.first : mode.y==1.0 ? s.second : s.second-s.first;`; break
      }
      case 'cell-value': body=`vec4 c=${cellConfig(n)}; CellSample s=cellular(p,c,0); return float(cellHash(s.id,cellSeed(c))&65535u)/65536.0;`; break
      case 'cell-edge': body=`return cellEdge(p,${cellConfig(n)});`; break
      case 'sphere': body=`return sdfSphere(p,${vector(n.centre)},data(${slot([n.radius])}).x);`; break
      case 'box': body=`return sdfBox(p,${vector(n.centre)},${vector(n.half)});`; break
      case 'cylinder': body=`vec2 c=data(${slot([n.radius,n.height])}).xy; return sdfCylinder(p,${vector(n.centre)},c.x,c.y);`; break
      case 'torus': body=`vec2 c=data(${slot([n.major,n.minor])}).xy; return sdfTorus(p,${vector(n.centre)},c.x,c.y);`; break
      case 'plane': body=`return sdfPlane(p,${vector(n.normal)},data(${slot([n.offset])}).x);`; break
      case 'sdf-union': case 'sdf-intersection': case 'sdf-difference': {
        const a=sample(n.a),b=sample(n.b),k=`data(${slot([n.amount])}).x`
        body=`return ${n.type==='sdf-union' ? `smoothMinimum(${a},${b},${k})` : `-smoothMinimum(-${a},${n.type==='sdf-difference' ? b : `-${b}`},${k})`};`; break
      }
      case 'constant': body = `return data(${slot([n.value])}).x;`; break
      case 'noise': body = 'return noise3(p);'; break
      case 'planar': {
        const direction = n.to.map((v,i) => v-n.from[i]), len2 = direction.reduce((a,b) => a+b*b,0)
        body = `return dot(p-${vector(n.from)},${vector(direction.map((v) => len2 <= 0 ? 0 : v/len2))});`; break
      }
      case 'distance': body = `float r=data(${slot([n.radius])}).x; return r<=0.0 ? 0.0 : length(p-${vector(n.centre)})/r;`; break
      case 'angular': body = `return angularField(p,${vector(n.centre)},${vector(n.axis)});`; break
      case 'scalar-domain': body = `return ${scalarField(n.source)}(${domainField(n.domain)}(p));`; break
      case 'add': case 'multiply': case 'min': case 'max': {
        const a=sample(n.a), b=sample(n.b)
        body = `return ${n.type === 'add' ? `${a}+${b}` : n.type === 'multiply' ? `${a}*${b}` : `${n.type}(${a},${b})`};`; break
      }
      case 'remap': {
        const source=sample(n.source), config=slot([n.low,n.high,n.outLow,n.outHigh])
        body = `vec4 c=data(${config}); return c.x==c.y ? c.z : c.z+(${source}-c.x)/(c.y-c.x)*(c.w-c.z);`; break
      }
      case 'threshold': {
        const source=sample(n.source), config=slot([n.low,n.high])
        body = `vec2 c=data(${config}).xy; float v=${source}; float t=c.x==c.y ? (v<c.x ? 0.0 : 1.0) : clamp((v-c.x)/(c.y-c.x),0.0,1.0); return t*t*(3.0-2.0*t);`; break
      }
      case 'fractal': case 'absolute-fractal': {
        const absolute=n.type === 'absolute-fractal', source=scalarField(n.source), config=parameters.noise({ ...n, octaves: Math.max(1,n.octaves) },absolute ? 'turbulence' : n.style)
        body = `
          vec4 c=data(${config}); float sum=0.0; float amp=1.0;
          for(int i=0;i<32;i++) {
            if(i>=int(c.x)) break;
            int at=${config}+${NOISE.matrices}+i*${NOISE.matrixStride};
            vec3 q=mat3(data(at).xyz,data(at+1).xyz,data(at+2).xyz)*p;
            ${absolute ? '' : 'q+=vec3(31.7,17.3,11.9)*float(i);'}
            float n=${source}(q);
            float shaped=${absolute ? 'abs(2.0*n-1.0)' : 'int(c.w)==1 ? abs(2.0*n-1.0) : int(c.w)==2 ? (1.0-abs(2.0*n-1.0))*(1.0-abs(2.0*n-1.0)) : n'};
            sum+=amp*shaped; amp*=c.y;
          }
          float value=c.z<=0.0 ? ${absolute ? '0.0' : '0.5'} : sum/c.z;
          ${absolute ? 'return value;' : 'if(int(c.w)==0) value=0.5+(value-0.5)*2.0; else if(int(c.w)==1) value*=1.75; else value=(value-0.2)/0.72; return clamp(value,0.0,1.0);'}
        `; break
      }
      default: return assertNever(n)
    }
    return fieldFunction('float',body,n.type)
  }
  const vectorField = (n: VectorField): string => {
    let body: string
    switch(n.type) {
      case 'cell-id': body=`return vec3(cellular(p,${cellConfig(n)},0).id);`; break
      case 'cell-colour': body=`vec4 c=${cellConfig(n)}; CellSample s=cellular(p,c,0); return 2.0*cellRandom(cellHash(s.id,cellSeed(c)))-1.0;`; break
      case 'vector-constant': body=`return ${vector(n.value)};`; break
      case 'position': body='return p;'; break
      case 'components': body=`return vec3(${scalarField(n.x)}(p),${scalarField(n.y)}(p),${scalarField(n.z)}(p));`; break
      case 'vector-add': body=`return ${vectorField(n.a)}(p)+${vectorField(n.b)}(p);`; break
      case 'vector-scale': body=`return ${scalarField(n.amount)}(p)*${vectorField(n.source)}(p);`; break
      case 'vector-domain': body=`return ${vectorField(n.source)}(${domainField(n.domain)}(p));`; break
      default: return assertNever(n)
    }
    return fieldFunction('vec3',body,n.type)
  }
  const domainField = (n: Domain): string => {
    let body: string
    switch(n.type) {
      case 'translate': body=`return p-${vector(n.offset)};`; break
      case 'scale': body=`vec3 s=${vector(n.scale)}; return vec3(s.x==0.0 ? 0.0 : p.x/s.x,s.y==0.0 ? 0.0 : p.y/s.y,s.z==0.0 ? 0.0 : p.z/s.z);`; break
      case 'rotate': body=`return inverseEuler(p,${vector(n.rotation)});`; break
      case 'repeat': body=`vec3 s=${vector(n.period)}; return vec3(repeatAxis(p.x,s.x),repeatAxis(p.y,s.y),repeatAxis(p.z,s.z));`; break
      case 'mirror': body=`vec3 c=${vector(n.centre)}; vec3 q=p-c; vec3 a=${vector(n.axes)}; return c+vec3(a.x>=0.5 ? abs(q.x) : q.x,a.y>=0.5 ? abs(q.y) : q.y,a.z>=0.5 ? abs(q.z) : q.z);`; break
      case 'polar-repeat': body=`vec3 c=${vector(n.centre)}; vec3 q=p-c; float r=length(q.xy); float a=repeatAxis(r==0.0 ? 0.0 : atan(q.y,q.x),6.283185307179586/max(1.0,data(${slot([n.count])}).x)); return c+vec3(r*cos(a),r*sin(a),q.z);`; break
      case 'radial-repeat': body=`vec3 c=${vector(n.centre)}; vec3 q=p-c; float r=length(q.xy); float period=data(${slot([n.period])}).x; float v=period<=0.0 ? r : mod(r,period); return c+vec3(r==0.0 ? vec2(0) : q.xy*(v/r),q.z);`; break
      case 'twist': case 'bend': body=`vec3 c=${vector(n.centre)}; vec3 q=p-c; return c+rotateZ(q,-data(${slot([n.amount])}).x*q.${n.type === 'twist' ? 'z' : 'x'});`; break
      case 'compose': body=`return ${domainField(n.second)}(${domainField(n.first)}(p));`; break
      case 'warp': body=`return p+data(${slot([n.amount])}).x*${vectorField(n.field)}(p);`; break
      default: return assertNever(n)
    }
    return fieldFunction('vec3',body,n.type)
  }
  const node = (n: Material, path = '$.texture'): string => {
    if (path.split('.').length > 66) throw new Error(`${path}: texture nesting exceeds 64`)
    if (++nodes > 200) throw new Error('GPU supports at most 200 texture nodes')
    const name = `material${nodes}`
    let body: string
    const call = (name: string, point = 'p', cache = 'cachedWarp,cachedConfig,cacheValid'): string => `${name}(${point},${cache})`
    try { switch (n.type) {
      case 'colourise': body=`return ${rampExpression(n,`${scalarField(n.field)}(p)`)};`; break
      case 'vector-colour': body=`return vec4(clamp(0.5+0.5*${vectorField(n.field)}(p),0.0,1.0),1);`; break
      case 'domain': {
        const base=node(n.base,`${path}.base`), domain=domainField(n.domain)
        body=`vec3 childWarp=vec3(0); vec4 childConfig=vec4(0); bool childValid=false; return ${call(base,`${domain}(p)`,'childWarp,childConfig,childValid')};`; break
      }
      case 'mix': {
        const mask=scalarField(n.mask), a=node(n.a,`${path}.a`), b=node(n.b,`${path}.b`)
        body=`float t=clamp(${mask}(p),0.0,1.0); if(t==0.0) return ${call(b)}; if(t==1.0) return ${call(a)}; return mix(${call(b)},${call(a)},t);`; break
      }
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
  return { source: helpers + cellularHelpers + fieldHelpers + geometrySource + warpSource + functions.join('\n') + entry + main, parameters: parameters.finish() }
}

function assertNever(value: never): never { throw new Error(`Unimplemented material: ${String(value)}`) }

const fieldHelpers = `
float repeatAxis(float x,float period) { return period<=0.0 ? x : x-period*floor(x/period+0.5); }
vec3 rotateZ(vec3 p,float degrees) { float a=radians(degrees); return vec3(p.x*cos(a)-p.y*sin(a),p.x*sin(a)+p.y*cos(a),p.z); }
vec3 inverseEuler(vec3 p,vec3 degrees) {
  vec3 a=radians(-degrees); p=rotateZ(p,-degrees.z);
  p=vec3(p.x*cos(a.y)+p.z*sin(a.y),p.y,p.z*cos(a.y)-p.x*sin(a.y));
  return vec3(p.x,p.y*cos(a.x)-p.z*sin(a.x),p.y*sin(a.x)+p.z*cos(a.x));
}
float angularField(vec3 p,vec3 centre,vec3 direction) {
  vec3 axis=safeNormalise(direction);
  vec3 north=vec3(0,-1,0)-dot(vec3(0,-1,0),axis)*axis;
  if(length(north)<1e-9) north=vec3(0,0,1)-dot(vec3(0,0,1),axis)*axis;
  north=safeNormalise(north); vec3 delta=p-centre;
  vec3 radial=delta-dot(delta,axis)*axis; float len=length(radial);
  return len<=0.0 ? 0.5 : (1.0-dot(north,radial)/len)/2.0;
}
`;
