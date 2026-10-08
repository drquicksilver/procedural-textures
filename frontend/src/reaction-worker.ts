import {initialReaction,stepReaction,prepareReactionInputs,type ReactionConfig} from './reaction'
self.onmessage=async (event:MessageEvent<ReactionConfig>) => {
  try {
    const c=event.data;const inputs=prepareReactionInputs(c);let a=initialReaction(c,inputs),b=new Float32Array(a.length)
    for(let step=0;step<c.iterations;step++) {
      stepReaction(c,a,b,inputs); [a,b]=[b,a]
      if(step%16===15) { self.postMessage({progress:step+1}); await new Promise(resolve=>setTimeout(resolve,0)) }
    }
    self.postMessage({values:a.buffer}, {transfer:[a.buffer]})
  } catch(error) { self.postMessage({error:error instanceof Error ? error.message : String(error)}) }
}
