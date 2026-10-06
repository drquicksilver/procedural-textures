import { useEffect, useRef } from 'preact/hooks'
import { draw, enqueue, onContextChange } from '../gpu/editor'
import { defaultView } from '../view'
import type { Node } from '../types'
import { currentVersion } from '../version'

const SIZE = 96, DEBOUNCE_MS = 250, CACHE_LIMIT = 300
// Bounded canvas snapshots: no PNG encoding, readback or object URLs.
const cache = new Map<string, HTMLCanvasElement>()
const view = { ...defaultView, mode: 'slice' as const, axis: 'xy' as const, position: 0 }

export function Thumbnail({ texture, ramps }: { texture: Node; ramps?: Record<string, Node> }) {
  const canvasRef = useRef<HTMLCanvasElement>(null)
  useEffect(() => {
    const key = JSON.stringify([texture, ramps ?? {}])
    let cancel: (() => void) | undefined
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
      cancel = enqueue(() => {
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
      }, 1)
    }
    const timer = cache.has(key) ? undefined : setTimeout(load, DEBOUNCE_MS)
    if (cache.has(key)) load()
    const unsubscribe = onContextChange((restored) => { if (restored) { cancel?.(); load() } })
    return () => { if (timer !== undefined) clearTimeout(timer); cancel?.(); unsubscribe() }
  }, [texture, ramps])
  return <div class="thumbnail checkerboard"><canvas ref={canvasRef} aria-hidden="true" /></div>
}
