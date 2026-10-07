/** Float32 arithmetic and a six-neighbour periodic stencil shared with Haskell. */
export interface ReactionConfig {
  resolution: number; iterations: number; feed: number; kill: number
  diffusionU: number; diffusionV: number; timeStep: number; seed: number
  initial: 'noise' | 'spots' | 'slab'
}
export const reactionKeys = ['resolution','iterations','feed','kill','diffusionU','diffusionV','timeStep','seed','initial'] as const
export function reactionKey(config: ReactionConfig): string { return JSON.stringify(reactionKeys.map(k=>config[k])) }
export function validateReaction(c: ReactionConfig): void {
  if(!Number.isInteger(c.resolution) || c.resolution<8 || c.resolution>64) throw new Error('Reaction resolution must be an integer from 8 to 64')
  if(!Number.isInteger(c.iterations) || c.iterations<0 || c.iterations>4096 || c.resolution**3*c.iterations>64_000_000) throw new Error('Reaction iterations must be 0–4096, with at most 64000000 voxel updates')
  for(const key of ['feed','kill','diffusionU','diffusionV','timeStep'] as const) if(!Number.isFinite(c[key]) || c[key]<0 || c[key]>(key==='feed'||key==='kill' ? .1 : 1)) throw new Error(`Reaction ${key} is outside its supported range`)
  if(!Number.isInteger(c.seed) || c.seed<0 || c.seed>4294967295) throw new Error('Reaction seed must be an unsigned 32-bit integer')
  if(!['noise','spots','slab'].includes(c.initial)) throw new Error('Unknown reaction initial condition')
}
const f=Math.fround
const mix=(x:number):number => { x=Math.imul(x^(x>>>16),0x7feb352d); x=Math.imul(x^(x>>>15),0x846ca68b); return (x^(x>>>16))>>>0 }
export function initialReaction(c: ReactionConfig): Float32Array<ArrayBuffer> {
  validateReaction(c)
  const n=c.resolution, values=new Float32Array(n*n*n*2)
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
export function stepReaction(c: ReactionConfig, source: Float32Array, target: Float32Array): void {
  const n=c.resolution, feed=f(c.feed),kill=f(c.kill),du=f(c.diffusionU),dv=f(c.diffusionV),dt=f(c.timeStep)
  for(let z=0;z<n;z++) for(let y=0;y<n;y++) for(let x=0;x<n;x++) {
    const i=2*(x+n*(y+n*z)),u=source[i],v=source[i+1]
    const a=2*((x+1)%n+n*(y+n*z)),b=2*((x+n-1)%n+n*(y+n*z)),d=2*(x+n*((y+1)%n+n*z)),e=2*(x+n*((y+n-1)%n+n*z)),g=2*(x+n*(y+n*((z+1)%n))),h=2*(x+n*(y+n*((z+n-1)%n)))
    const lap=(lane:number) => f(f(f(f(f(f(f(source[a+lane]+source[b+lane])+source[d+lane])+source[e+lane])+source[g+lane])+source[h+lane])/6)-source[i+lane])
    const reaction=f(f(u*v)*v)
    const nextU=f(u+f(dt*f(f(f(du*lap(0))-reaction)+f(feed*f(1-u)))))
    const nextV=f(v+f(dt*f(f(f(dv*lap(1))+reaction)-f(f(feed+kill)*v))))
    target[i]=Math.max(0,Math.min(1,nextU)); target[i+1]=Math.max(0,Math.min(1,nextV))
  }
}
export function simulateReaction(c: ReactionConfig): Float32Array<ArrayBuffer> {
  let source=initialReaction(c),target=new Float32Array(source.length)
  for(let i=0;i<c.iterations;i++) { stepReaction(c,source,target); [source,target]=[target,source] }
  return source
}
/** Voxel centres live at (i+.5)/n; manual periodic trilinear interpolation. */
export function sampleReaction(values: Float32Array,n:number,point:readonly number[],lane=1): number {
  const p=point.map(v=>((v%1)+1)%1*n-.5),base=p.map(Math.floor),t=p.map((v,i)=>v-base[i])
  const at=(x:number,y:number,z:number) => values[2*(((x%n)+n)%n+n*(((y%n)+n)%n+n*(((z%n)+n)%n)))+lane]
  const lerp=(a:number,b:number,t:number)=>a+(b-a)*t
  const xy=(z:number)=>lerp(lerp(at(base[0],base[1],z),at(base[0]+1,base[1],z),t[0]),lerp(at(base[0],base[1]+1,z),at(base[0]+1,base[1]+1,z),t[0]),t[1])
  return lerp(xy(base[2]),xy(base[2]+1),t[2])
}
