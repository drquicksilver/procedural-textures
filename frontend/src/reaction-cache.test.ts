// @vitest-environment node
import {afterEach,expect,it,vi} from 'vitest'
import {ReactionCache} from './reaction-cache'
import {initialReaction,type ReactionConfig} from './reaction'
const config:ReactionConfig={resolution:8,iterations:0,feed:.022,kill:.051,diffusionU:.9,diffusionV:.45,timeStep:1,seed:42,initial:'spots'}
class FakeWorker {
  onmessage:((event:MessageEvent)=>void)|null=null;onerror:((event:ErrorEvent)=>void)|null=null
  config!:ReactionConfig;terminated=false
  postMessage(c:ReactionConfig) {this.config=c}
  terminate() {this.terminated=true}
  finish() {this.onmessage?.({data:{values:initialReaction(this.config).buffer}} as MessageEvent)}
}
afterEach(()=>vi.useRealTimers())
it('deduplicates shared requests, reuses completed results and bounds workers',async()=>{
  const workers:FakeWorker[]=[];const cache=new ReactionCache(()=>{const w=new FakeWorker();workers.push(w);return w})
  const a=cache.acquire([config]),b=cache.acquire([config]),c=cache.acquire([{...config,seed:43}]),d=cache.acquire([{...config,seed:44}])
  expect(workers).toHaveLength(2);expect(cache.stats.running).toBe(2)
  workers[0].finish();workers[1].finish();expect(workers).toHaveLength(3);workers[2].finish()
  await Promise.all([a.ready,b.ready,c.ready,d.ready]);a.cancel();b.cancel();c.cancel();d.cancel()
  const warm=cache.acquire([config]);await warm.ready;warm.cancel();expect(cache.computations).toBe(3);cache.dispose()
})
it('cancels obsolete work while preserving same-config camera changes',async()=>{
  vi.useFakeTimers();const workers:FakeWorker[]=[];const cache=new ReactionCache(()=>{const w=new FakeWorker();workers.push(w);return w})
  const a=cache.acquire([config]);a.cancel();const b=cache.acquire([config])
  vi.advanceTimersByTime(150);expect(workers[0].terminated).toBe(false);workers[0].finish();await Promise.all([a.ready,b.ready]);b.cancel()
  const obsolete=cache.acquire([{...config,seed:99}]);const rejected=expect(obsolete.ready).rejects.toThrow('cancelled');obsolete.cancel()
  vi.advanceTimersByTime(150);await rejected;expect(workers[1].terminated).toBe(true);cache.dispose()
})
it('evicts only unpinned completed volumes and reports worker failures',async()=>{
  const workers:FakeWorker[]=[];const cache=new ReactionCache(()=>{const w=new FakeWorker();workers.push(w);return w},4096)
  const a=cache.acquire([config]),b=cache.acquire([{...config,seed:43}]);workers[0].finish();workers[1].finish();await Promise.all([a.ready,b.ready]);a.cancel();b.cancel()
  expect(cache.stats.bytes).toBeLessThanOrEqual(4096)
  const bad=cache.acquire([{...config,seed:99}]);const rejected=expect(bad.ready).rejects.toThrow('broken');workers[2].onmessage?.({data:{error:'broken'}} as MessageEvent);await rejected;bad.cancel();cache.dispose()
})
it('starts queued work after worker startup fails and ignores late failed-worker messages',async()=>{
  const workers:FakeWorker[]=[];let starts=0
  const cache=new ReactionCache(()=>{if(++starts===1) throw new Error('startup');const w=new FakeWorker();workers.push(w);return w},4096,1)
  const failed=cache.acquire([config,{...config,seed:43}]);const rejected=expect(failed.ready).rejects.toThrow('startup')
  expect(workers).toHaveLength(1);workers[0].finish();await rejected;failed.cancel()
  const broken=cache.acquire([config]);const failure=expect(broken.ready).rejects.toThrow('broken')
  const old=workers[1];old.onmessage?.({data:{error:'broken'}} as MessageEvent);await failure;broken.cancel()
  const retry=cache.acquire([config]);old.finish();expect(()=>cache.peek(config)).toThrow('not been prepared')
  workers[2].finish();await retry.ready;retry.cancel();cache.dispose()
})

it('deduplicates field inputs, invalidates changed seeds and accepts compact 2D results',async()=>{
 const workers:FakeWorker[]=[];const cache=new ReactionCache(()=>{const w=new FakeWorker();workers.push(w);return w})
 const field:ReactionConfig={...config,dimensions:2,seedField:{type:'constant',value:.4},feedField:{type:'constant',value:.02},killField:{type:'constant',value:.05}}
 const a=cache.acquire([field]),b=cache.acquire([{...field}]);expect(workers).toHaveLength(1)
 workers[0].finish();await Promise.all([a.ready,b.ready]);expect(cache.peek(field)).toHaveLength(128)
 expect(cache.peek(field)[1]).toBeCloseTo(.1,7);a.cancel();b.cancel()
 const changed=cache.acquire([{...field,seedField:{type:'constant',value:.6}}]);expect(workers).toHaveLength(2)
 workers[1].finish();await changed.ready;changed.cancel();cache.dispose()
})
