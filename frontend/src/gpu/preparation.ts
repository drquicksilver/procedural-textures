import type {TextureDocument} from '../types'
import {compileMaterial} from './compiler'
import {reactionCache,type Preparation} from '../reaction-cache'
import type {ReactionConfig} from '../reaction'
const configs=new WeakMap<TextureDocument,ReactionConfig[]>()
export function prepareDocument(document:TextureDocument): Preparation|undefined {
  let volumes=configs.get(document)
  if(!volumes) {
    const pending:unknown[]=[document.texture];let found=false,visits=0
    while(pending.length && ++visits<=4096) {
      const node=pending.pop();if(!node || typeof node!=='object' || Array.isArray(node)) continue
      if('type' in node && (node.type==='reaction-diffusion'||node.type==='field-reaction')) {found=true;break}
      pending.push(...Object.values(node))
    }
    volumes=found ? compileMaterial(document).volumes??[] : [];configs.set(document,volumes)
  }
  return volumes.length ? reactionCache.acquire(volumes) : undefined
}
