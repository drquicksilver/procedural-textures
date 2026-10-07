import {reactionKey,validateReaction,type ReactionConfig} from './reaction'
export interface Preparation { ready: Promise<void>; cancel:()=>void }
interface WorkerLike { onmessage:((event:MessageEvent)=>void)|null; onerror:((event:ErrorEvent)=>void)|null; postMessage:(c:ReactionConfig)=>void; terminate:()=>void }
interface Entry { config:ReactionConfig; refs:number; values?:Float32Array; ready:Promise<void>; resolve:()=>void; reject:(e:Error)=>void; worker?:WorkerLike; abort?:ReturnType<typeof setTimeout> }
/** Shared, bounded worker cache. Acquisitions pin volumes until rendering ends. */
export class ReactionCache {
  private entries=new Map<string,Entry>()
  computations=0
  private factory:()=>WorkerLike
  private bytesLimit:number
  private workersLimit:number
  constructor(factory:()=>WorkerLike,bytesLimit=8*1024*1024,workersLimit=2) {this.factory=factory;this.bytesLimit=bytesLimit;this.workersLimit=workersLimit}
  acquire(configs:ReactionConfig[]): Preparation {
    configs.forEach(validateReaction)
    const selected:Entry[]=[]
    for(const c of configs) {
      const key=reactionKey(c);let entry=this.entries.get(key)
      if(!entry) {
        let resolve!:()=>void,reject!:(e:Error)=>void
        const ready=new Promise<void>((yes,no)=>{resolve=yes;reject=no})
        entry={config:c,refs:0,ready,resolve,reject};this.entries.set(key,entry)
      }
      if(entry.abort) {clearTimeout(entry.abort);entry.abort=undefined}
      this.entries.delete(key);this.entries.set(key,entry);entry.refs++;selected.push(entry)
    }
    this.pump()
    let released=false
    return {ready:Promise.all(selected.map(e=>e.ready)).then(()=>{}),cancel:()=>{
      if(released) return;released=true
      for(const entry of selected) {
        entry.refs--
        if(!entry.refs && !entry.values && this.entries.get(reactionKey(entry.config))===entry) entry.abort=setTimeout(()=>{
          if(entry.refs || this.entries.get(reactionKey(entry.config))!==entry) return
          entry.worker?.terminate();entry.worker=undefined
          this.entries.delete(reactionKey(entry.config));entry.reject(new Error('Simulation cancelled'));this.pump()
        },100)
      }
      this.trim()
    }}
  }
  peek(c:ReactionConfig): Float32Array {
    const entry=this.entries.get(reactionKey(c))
    if(!entry?.values) throw new Error('Reaction volume has not been prepared')
    return entry.values
  }
  private pump(): void {
    let running=[...this.entries.values()].filter(e=>e.worker).length
    for(const [key,entry] of this.entries) {
      if(running>=this.workersLimit) break
      if(entry.worker || entry.values || !entry.refs) continue
      try {
        const worker=this.factory();entry.worker=worker;running++;this.computations++
        const fail=(error:Error)=>{if(entry.worker!==worker) return;worker.terminate();entry.worker=undefined;if(entry.abort) clearTimeout(entry.abort);this.entries.delete(key);entry.reject(error);this.pump()}
        worker.onerror=(event)=>fail(new Error(event.message || 'Simulation worker failed'))
        worker.onmessage=(event)=>{
          if(entry.worker!==worker) return
          if(event.data.error) {fail(new Error(event.data.error));return}
          if(!event.data.values) return
          const values=new Float32Array(event.data.values)
          if(values.length!==2*entry.config.resolution**3 || values.some(v=>!Number.isFinite(v)||v<0||v>1)) {fail(new Error('Invalid simulation worker result'));return}
          worker.terminate();entry.worker=undefined;entry.values=values;entry.resolve();this.trim();this.pump()
        }
        worker.postMessage(entry.config)
      } catch(error) {entry.worker?.terminate();entry.worker=undefined;if(entry.abort) clearTimeout(entry.abort);this.entries.delete(key);entry.reject(error instanceof Error ? error : new Error(String(error)));running=[...this.entries.values()].filter(e=>e.worker).length}
    }
  }
  private trim():void {
    let bytes=[...this.entries.values()].reduce((sum,e)=>sum+(e.values?.byteLength??0),0)
    for(const [key,e] of this.entries) if(bytes>this.bytesLimit && !e.refs && e.values) {bytes-=e.values.byteLength;this.entries.delete(key)}
  }
  dispose():void {
    for(const e of this.entries.values()) {if(e.abort) clearTimeout(e.abort);e.worker?.terminate();if(!e.values) e.reject(new Error('Simulation cache disposed'))}
    this.entries.clear()
  }
  get stats() {return {entries:this.entries.size,running:[...this.entries.values()].filter(e=>e.worker).length,bytes:[...this.entries.values()].reduce((n,e)=>n+(e.values?.byteLength??0),0),computations:this.computations}}
}
export const reactionCache=new ReactionCache(()=>new Worker(new URL('./reaction-worker.ts',import.meta.url),{type:'module'}))
