import { useEffect, useState } from 'preact/hooks'
import { renderDocument } from '../api'
import type { TextureDocument } from '../types'

const SIZE = 96

// Thumbnails are cached by document content, so revisiting a list is instant.
const cache = new Map<string, Promise<string>>()

function thumbnailUrl(document: TextureDocument): Promise<string> {
  const key = JSON.stringify(document.texture)
  let url = cache.get(key)
  if (!url) {
    url = renderDocument(document, SIZE).then((blob) => URL.createObjectURL(blob))
    url.catch(() => cache.delete(key))
    cache.set(key, url)
  }
  return url
}

export function Thumbnail({ document }: { document: TextureDocument }) {
  const [url, setUrl] = useState<string | null>(null)

  useEffect(() => {
    let live = true
    thumbnailUrl(document).then(
      (u) => live && setUrl(u),
      () => live && setUrl(null),
    )
    return () => {
      live = false
    }
  }, [document])

  return <div class="thumbnail checkerboard">{url && <img src={url} alt="" draggable={false} />}</div>
}
