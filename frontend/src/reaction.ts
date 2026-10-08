import {fieldWork} from './field-work'
import type {ScalarField} from './gpu/material'
import {compileScalar} from './fields-cpu'
/** Float32 arithmetic and a six-neighbour periodic stencil shared with Haskell. */
export interface ReactionConfig {
  dimensions?:number;seedField?:ScalarField;feedField?:ScalarField;killField?:ScalarField
  resolution: number; iterations: number; feed: number; kill: number
  diffusionU: number; diffusionV: number; timeStep: number; seed: number
  initial: 'noise' | 'spots' | 'slab'
}
export const reactionKeys = ['resolution','iterations','feed','kill','diffusionU','diffusionV','timeStep','seed','initial'] as const
export function reactionKey(config: ReactionConfig): string { return config.seedField?JSON.stringify([[config.resolution,config.iterations,config.diffusionU,config.diffusionV,config.timeStep],config.dimensions,canonical(config.seedField),canonical(config.feedField),canonical(config.killField)]):JSON.stringify(reactionKeys.map(k=>config[k])) }
function canonical(value:unknown):unknown {if(Array.isArray(value))return value.map(canonical);if(value&&typeof value==='object')return Object.fromEntries(Object.entries(value).sort(([a],[b])=>a.localeCompare(b)).map(([k,v])=>[k,canonical(v)]));return value}
/** Dependencies are finite document expressions; output U/V share one volume. */
export function reactionDependencies(c:ReactionConfig):ReactionConfig[]{
 const found=new Map<string,ReactionConfig>()
 const visit=(value:unknown)=>{if(!value||typeof value!=='object'||Array.isArray(value))return;const n=value as Record<string,unknown>
 if(n.type==='field-reaction'||n.type==='reaction-diffusion'){const config=(n.type==='field-reaction'?{...n,feed:0,kill:0,seed:0,initial:'noise'}:n) as unknown as ReactionConfig;found.set(reactionKey(config),config)}
 for(const v of Object.values(n))visit(v)}
 found.set(reactionKey(c),c);[c.seedField,c.feedField,c.killField].forEach(visit)
 return [...found.values()]
}
export function validateReaction(c: ReactionConfig): void {
  if(c.seedField){
    if(reactionDependencies(c).length>4)throw new Error('Field reaction supports at most four distinct prepared dependencies')
    const dims=c.dimensions;if(dims!==2&&dims!==3)throw new Error('Field reaction dimensions must be 2 or 3')
    if(!c.feedField||!c.killField)throw new Error('Field reaction requires all three inputs')
    if(!Number.isInteger(c.resolution)||c.resolution<8||c.resolution>(dims===2?256:64)||!Number.isInteger(c.iterations)||c.iterations<0||c.iterations>4096||c.resolution**dims*c.iterations>64_000_000)throw new Error('Field reaction exceeds dimensional work limits')
    if(c.resolution**dims*(fieldWork(c.seedField)+fieldWork(c.feedField)+fieldWork(c.killField))>8_000_000)throw new Error('Field reaction input preparation exceeds 8000000 evaluations')
    validateReaction({...c,seedField:undefined,resolution:8,iterations:0});return
  }
  if(!Number.isInteger(c.resolution) || c.resolution<8 || c.resolution>64) throw new Error('Reaction resolution must be an integer from 8 to 64')
  if(!Number.isInteger(c.iterations) || c.iterations<0 || c.iterations>4096 || c.resolution**3*c.iterations>64_000_000) throw new Error('Reaction iterations must be 0–4096, with at most 64000000 voxel updates')
  for(const key of ['feed','kill','diffusionU','diffusionV','timeStep'] as const) if(!Number.isFinite(c[key]) || c[key]<0 || c[key]>(key==='feed'||key==='kill' ? .1 : 1)) throw new Error(`Reaction ${key} is outside its supported range`)
  if(!Number.isInteger(c.seed) || c.seed<0 || c.seed>4294967295) throw new Error('Reaction seed must be an unsigned 32-bit integer')
  if(!['noise','spots','slab'].includes(c.initial)) throw new Error('Unknown reaction initial condition')
}
const f=Math.fround
const mix=(x:number):number => { x=Math.imul(x^(x>>>16),0x7feb352d); x=Math.imul(x^(x>>>15),0x846ca68b); return (x^(x>>>16))>>>0 }
export interface ReactionInputs {seed:Float32Array;feed:Float32Array;kill:Float32Array}
export function prepareReactionInputs(c:ReactionConfig,dependencies=new Map<string,Float32Array>()):ReactionInputs|undefined{
 if(!c.seedField)return undefined
 validateReaction(c)
 const sample=(config:unknown,p:readonly number[],lane:number)=>{const source=config as ReactionConfig,key=reactionKey(source);let values=dependencies.get(key);if(!values){if(dependencies.size>=4)throw new Error('At most four reaction dependencies');values=simulateReaction(source,dependencies);dependencies.set(key,values)}return sampleReaction(values,source.resolution,p,lane,source.dimensions??3)}
 const fns=[c.seedField,c.feedField!,c.killField!].map(n=>compileScalar(n,sample)),n=c.resolution,nz=c.dimensions===2?1:n,count=n*n*nz
 const arrays=[new Float32Array(count),new Float32Array(count),new Float32Array(count)]
 for(let z=0;z<nz;z++)for(let y=0;y<n;y++)for(let x=0;x<n;x++){const p=[(x+.5)/n,(y+.5)/n,c.dimensions===2?0:(z+.5)/n],at=x+n*(y+n*z);for(let lane=0;lane<3;lane++){const v=fns[lane](p);arrays[lane][at]=Math.max(0,Math.min(lane===0?1:.1,Number.isFinite(v)?v:0))}}
 return{seed:arrays[0],feed:arrays[1],kill:arrays[2]}
}
export function initialReaction(c: ReactionConfig,inputs?:ReactionInputs): Float32Array<ArrayBuffer> {
  validateReaction(c)
  if(c.seedField&&!inputs)inputs=prepareReactionInputs(c)
  const n=c.resolution, values=new Float32Array(n*n*(c.dimensions===2?1:n)*2)
  if(inputs){for(let i=0;i<inputs.seed.length;i++){values[2*i]=f(1-f(f(.5)*inputs.seed[i]));values[2*i+1]=f(f(.25)*inputs.seed[i])}return values}
  for(let z=0;z<n;z++) for(let y=0;y<n;y++) for(let x=0;x<n;x++) {
    const h=mix(c.seed^Math.imul(x,0x8da6b343)^Math.imul(y,0xd8163841)^Math.imul(z,0xcb1ab31f))
    const random=(h&65535)/65536, i=2*(x+n*(y+n*z))
    const spot=(x%Math.max(4,Math.floor(n/2))<Math.max(2,Math.floor(n/8))) && (y%Math.max(4,Math.floor(n/2))<Math.max(2,Math.floor(n/8))) && (z%Math.max(4,Math.floor(n/2))<Math.max(2,Math.floor(n/8)))
    const block=Math.max(2,Math.floor(n/6)),coarse=mix(c.seed^Math.imul(Math.floor(x/block),0x8da6b343)^Math.imul(Math.floor(y/block),0xd8163841)^Math.imul(Math.floor(z/block),0xcb1ab31f))
    const active=c.initial==='noise' ? (coarse&255)<64 : c.initial==='spots' ? spot : z<n/5
    values[i]=active ? f(.5+f(f(.1)*f(random-.5))) : 1
    values[i+1]=active ? f(.25+f(f(.05)*f(random-.5))) : 0
  }
  return values
}
export function stepReaction(c: ReactionConfig, source: Float32Array, target: Float32Array,inputs?:ReactionInputs): void {
  const n=c.resolution, feed=f(c.feed),kill=f(c.kill),du=f(c.diffusionU),dv=f(c.diffusionV),dt=f(c.timeStep)
  for(let z=0;z<(c.dimensions===2?1:n);z++) for(let y=0;y<n;y++) for(let x=0;x<n;x++) {
    const i=2*(x+n*(y+n*z)),u=source[i],v=source[i+1]
    const a=2*((x+1)%n+n*(y+n*z)),b=2*((x+n-1)%n+n*(y+n*z)),d=2*(x+n*((y+1)%n+n*z)),e=2*(x+n*((y+n-1)%n+n*z)),g=2*(x+n*(y+n*((z+1)%n))),h=2*(x+n*(y+n*((z+n-1)%n)))
    const lap=(lane:number) => c.dimensions===2?f(f(f(f(f(source[a+lane]+source[b+lane])+source[d+lane])+source[e+lane])/4)-source[i+lane]):f(f(f(f(f(f(f(source[a+lane]+source[b+lane])+source[d+lane])+source[e+lane])+source[g+lane])+source[h+lane])/6)-source[i+lane])
    const localFeed=inputs?inputs.feed[i/2]:feed,localKill=inputs?inputs.kill[i/2]:kill
    const reaction=f(f(u*v)*v)
    const nextU=f(u+f(dt*f(f(f(du*lap(0))-reaction)+f(localFeed*f(1-u)))))
    const nextV=f(v+f(dt*f(f(f(dv*lap(1))+reaction)-f(f(localFeed+localKill)*v))))
    target[i]=Math.max(0,Math.min(1,nextU)); target[i+1]=Math.max(0,Math.min(1,nextV))
  }
}
export function simulateReaction(c: ReactionConfig,dependencies=new Map<string,Float32Array>()): Float32Array<ArrayBuffer> {
  const inputs=prepareReactionInputs(c,dependencies)
  let source=initialReaction(c,inputs),target=new Float32Array(source.length)
  for(let i=0;i<c.iterations;i++) { stepReaction(c,source,target,inputs); [source,target]=[target,source] }
  return source
}
/** Voxel centres live at (i+.5)/n; manual periodic trilinear interpolation. */
export function sampleReaction(values: Float32Array,n:number,point:readonly number[],lane=1,dimensions=3): number {
  const p=point.map(v=>((v%1)+1)%1*n-.5),base=p.map(Math.floor),t=p.map((v,i)=>v-base[i])
  const at=(x:number,y:number,z:number) => values[2*(((x%n)+n)%n+n*(((y%n)+n)%n+n*(((z%n)+n)%n)))+lane]
  const lerp=(a:number,b:number,t:number)=>a+(b-a)*t
  const xy=(z:number)=>lerp(lerp(at(base[0],base[1],z),at(base[0]+1,base[1],z),t[0]),lerp(at(base[0],base[1]+1,z),at(base[0]+1,base[1]+1,z),t[0]),t[1])
  return dimensions===2?xy(0):lerp(xy(base[2]),xy(base[2]+1),t[2])
}
