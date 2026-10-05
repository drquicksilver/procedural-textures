import { useEffect, useState } from 'preact/hooks'
import { renderDocument } from '../api'
import type { Node } from '../types'
import { currentVersion } from '../version'

const SIZE = 96
const DEBOUNCE_MS = 250
const CACHE_LIMIT = 300

// Thumbnails are cached by texture content, so unchanged subtrees and
// revisited lists render instantly. The oldest entries are dropped (and their
// object URLs released) beyond CACHE_LIMIT.
const cache = new Map<string, Promise<string>>()

function evictOldest(): void {
  while (cache.size > CACHE_LIMIT) {
    const [oldestKey, oldest] = cache.entries().next().value!
    cache.delete(oldestKey)
    oldest.then((u) => URL.revokeObjectURL(u), () => {})
  }
}

function thumbnailUrl(texture: Node): Promise<string> {
  const key = JSON.stringify(texture)
  let url = cache.get(key)
  if (!url) {
    const document = { version: currentVersion, name: '', description: '', texture }
    url = renderDocument(document, SIZE).then((blob) => URL.createObjectURL(blob))
    url.catch(() => cache.delete(key))
    cache.set(key, url)
    evictOldest()
  }
  return url
}

/**
 * A small render of a texture. Uncached textures wait until they stop
 * changing for a moment, so dragging a slider doesn't flood the server; the
 * previous image stays up meanwhile.
 */
export function Thumbnail({ texture }: { texture: Node }) {
  const [url, setUrl] = useState<string | null>(null)

  useEffect(() => {
    let live = true
    const load = () =>
      thumbnailUrl(texture).then(
        (u) => live && setUrl(u),
        () => {},
      )
    const cached = cache.has(JSON.stringify(texture))
    const timer = cached ? undefined : setTimeout(load, DEBOUNCE_MS)
    if (cached) void load()
    return () => {
      live = false
      if (timer !== undefined) clearTimeout(timer)
    }
  }, [texture])

  return <div class="thumbnail checkerboard">{url && <img src={url} alt="" draggable={false} />}</div>
}
