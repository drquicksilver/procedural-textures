// @vitest-environment node
import {expect,it} from 'vitest'
import {simulateReaction,stepReaction,sampleReaction,validateReaction,type ReactionConfig} from './reaction'
import fixtures from '../../test-vectors/reaction.json'
for(const [i,test] of fixtures.cases.entries()) it(`matches every Haskell Float32 voxel in solver fixture ${i}`,()=>{
  expect([...simulateReaction(test.config as ReactionConfig)]).toEqual(test.values)
})
const config:ReactionConfig={resolution:8,iterations:12,feed:.02,kill:.05,diffusionU:.9,diffusionV:.45,timeStep:1,seed:42,initial:'spots'}
it('advances a uniform state by the analytical reaction and feed terms',()=>{
  const a=new Float32Array(1024),b=new Float32Array(1024)
  for(let i=0;i<a.length;i+=2) {a[i]=.5;a[i+1]=.25}
  stepReaction(config,a,b);expect(b[0]).toBeCloseTo(.47875,7);expect(b[1]).toBeCloseTo(.26375,7)
  expect(b[100]).toBe(b[0]);expect(b[101]).toBe(b[1])
})
it('samples voxel centres, wraps negative positions and trilinearly interpolates',()=>{
  const v=new Float32Array([0,0,0,1,0,2,0,3,0,4,0,5,0,6,0,7])
  expect(sampleReaction(v,2,[.25,.25,.25])).toBe(0)
  expect(sampleReaction(v,2,[.5,.5,.5])).toBe(3.5)
  expect(sampleReaction(v,2,[-.5,1.5,2.5])).toBe(3.5)
})
it('rejects invalid simulation work, seeds and non-finite chemistry',()=>{
  for(const patch of [{resolution:7},{resolution:64,iterations:4096},{feed:NaN},{seed:-1},{initial:'unknown'}]) expect(()=>validateReaction({...config,...patch} as ReactionConfig)).toThrow()
})

for(const [i,test] of fixtures.fieldCases.entries()) it(`matches every field-driven Haskell Float32 voxel ${i}`,()=>{
 const c={...test.config,feed:0,kill:0,seed:0,initial:'noise'} as ReactionConfig
 expect([...simulateReaction(c)]).toEqual(test.values)
 const values=simulateReaction(c)
 if(c.dimensions===2)expect(sampleReaction(values,c.resolution,[.2,.3,-99],1,2)).toBe(sampleReaction(values,c.resolution,[.2,.3,99],1,2))
})

it('field inputs participate in stable keys, preparation limits and nested dependencies', async()=>{
 const {reactionKey}=await import('./reaction')
 const {prepareReactionInputs}=await import('./reaction')
 const c:ReactionConfig={...config,iterations:0,dimensions:2,seedField:{type:'constant',value:.4},feedField:{type:'constant',value:.02},killField:{type:'constant',value:.05}}
 expect(reactionKey(c)).not.toBe(reactionKey({...c,seedField:{type:'constant',value:.5}}))
 expect(reactionKey(c)).toBe(reactionKey({...c,seedField:{value:.4,type:'constant'}}))
 const child={...c,type:'field-reaction',dimensions:2,seedField:c.seedField!,feedField:c.feedField!,killField:c.killField!,output:'v'} as const
 const nested={...c,seedField:child}
 expect(prepareReactionInputs(nested)!.seed[0]).toBeCloseTo(.1,7)
 expect(simulateReaction(nested)[1]).toBeCloseTo(.025,7)
 expect(()=>validateReaction({...c,resolution:256,seedField:{type:'cell-edge',dimensions:3,jitter:1,seed:0}})).toThrow(/preparation/)
 expect(()=>validateReaction({...c,resolution:256,iterations:4096})).toThrow(/work limits/)
})
