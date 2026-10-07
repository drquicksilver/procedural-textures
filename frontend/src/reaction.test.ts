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
