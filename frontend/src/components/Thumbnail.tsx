import {prepareDocument} from '../gpu/preparation'
import type {Preparation} from '../reaction-cache'
import { useEffect, useRef } from 'preact/hooks'
import { draw, enqueue, onContextChange } from '../gpu/editor'
import { defaultView } from '../view'
import type { Node, ExampleGuide } from '../types'
import { currentVersion } from '../version'

const SIZE = 96, DEBOUNCE_MS = 250, CACHE_LIMIT = 300
// Bounded canvas snapshots: no PNG encoding, readback or object URLs.
const cache = new Map<string, HTMLCanvasElement>()
export function Thumbnail({ texture, ramps, preview }: { texture: Node; ramps?: Record<string, Node>; preview?: ExampleGuide['preview'] }) {
  const canvasRef = useRef<HTMLCanvasElement>(null)
  useEffect(() => {
    const view = { ...defaultView, mode: 'slice' as const, axis: preview?.axis ?? 'xy', position: preview?.position ?? 0 }
    const key = JSON.stringify([texture, ramps ?? {}, view.axis, view.position])
    let cancel: (() => void) | undefined
    let timer: ReturnType<typeof setTimeout> | undefined
    let visible = false
    let generation=0,preparation:Preparation|undefined
    const load = () => {
      const target = canvasRef.current!
      const cached = cache.get(key)
      if (cached) {
        cache.delete(key); cache.set(key, cached)
        target.width = target.height = SIZE
        target.getContext('2d')!.drawImage(cached, 0, 0)
        target.dataset.rendered = 'true'
        return
      }
      const id=generation
      const render=() => {
        try {
          draw(target, { version: currentVersion, name: '', description: '', ramps, texture }, view, SIZE, false)
          const snapshot = window.document.createElement('canvas'); snapshot.width = snapshot.height = SIZE
          snapshot.getContext('2d')!.drawImage(target, 0, 0)
          const previous = cache.get(key)
          if (previous) previous.width = previous.height = 0
          cache.delete(key); cache.set(key, snapshot)
          while (cache.size > CACHE_LIMIT) {
            const [oldestKey, oldest] = cache.entries().next().value!
            cache.delete(oldestKey); oldest.width = oldest.height = 0
          }
          target.removeAttribute('title')
        } catch (error) { target.title = error instanceof Error ? error.message : String(error) }
        finally {preparation?.cancel();preparation=undefined}
      }
      cancel=enqueue(()=>{
        try {
          preparation=prepareDocument({version:currentVersion,name:'',description:'',ramps,texture})
          if(!preparation) {render();return}
          preparation.ready.then(()=>{if(id===generation) cancel=enqueue(render,1)},error=>{if(id===generation) {target.title=String(error);preparation?.cancel();preparation=undefined}})
        } catch(error) {target.title=String(error)}
      },1)
    }
    const cancelPending = () => {
      generation++;preparation?.cancel();preparation=undefined
      if (timer !== undefined) clearTimeout(timer)
      timer = undefined
      cancel?.(); cancel = undefined
    }
    const schedule = () => {
      cancelPending()
      if (!visible) return
      if (cache.has(key)) load()
      else timer = setTimeout(() => { timer = undefined; load() }, DEBOUNCE_MS)
    }
    // Warm only visible/nearby cards. Offscreen library work cannot evict useful
    // programs or begin an expensive compile while the user selects a material.
    const observer = typeof IntersectionObserver === 'undefined' ? null : new IntersectionObserver(([entry]) => {
      visible = entry.isIntersecting
      schedule()
    }, { rootMargin: '128px' })
    if (observer) observer.observe(canvasRef.current!.parentElement!)
    else { visible = true; schedule() }
    const unsubscribe = onContextChange((restored) => { if (restored) schedule() })
    return () => { observer?.disconnect(); cancelPending(); unsubscribe() }
  }, [texture, ramps, preview?.axis, preview?.position])
  return <div class="thumbnail checkerboard"><canvas ref={canvasRef} aria-hidden="true" /></div>
}
