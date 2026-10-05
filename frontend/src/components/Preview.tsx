import type { ComponentChildren } from 'preact'
import { useEffect, useRef, useState } from 'preact/hooks'
import { renderDocument } from '../api'
import { PreviewScheduler } from '../preview'
import type { TextureDocument } from '../types'

const LOW_SIZE = 96
const MAX_SIZE = 1024
const SETTLE_MS = 180

interface Props {
  document: TextureDocument
  /** Drawn over the image, in a box exactly covering it (for handles). */
  overlay?: ComponentChildren
}

/** A square, live-rendered view of a document. */
export function Preview({ document, overlay }: Props) {
  const frameRef = useRef<HTMLDivElement>(null)
  const schedulerRef = useRef<PreviewScheduler | null>(null)
  const [imageUrl, setImageUrl] = useState<string | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [busy, setBusy] = useState(false)
  const [fullSize, setFullSize] = useState(512)

  useEffect(() => {
    const scheduler = new PreviewScheduler(
      renderDocument,
      (result) => {
        setError(null)
        setImageUrl((previous) => {
          if (previous) URL.revokeObjectURL(previous)
          return URL.createObjectURL(result.blob)
        })
      },
      (failure) => setError(failure instanceof Error ? failure.message : String(failure)),
      setBusy,
      { lowSize: LOW_SIZE, fullSize, settleMs: SETTLE_MS },
    )
    schedulerRef.current = scheduler
    return () => scheduler.dispose()
    // The scheduler lives as long as the component; size changes go through setOptions.
  }, [])

  // Render at the frame's size in device pixels, in steps of 64 so that small
  // layout changes don't trigger re-renders.
  useEffect(() => {
    const frame = frameRef.current
    if (!frame) return
    const observer = new ResizeObserver(([entry]) => {
      const pixels = entry.contentRect.width * window.devicePixelRatio
      const size = Math.min(MAX_SIZE, Math.max(64, Math.ceil(pixels / 64) * 64))
      setFullSize(size)
    })
    observer.observe(frame)
    return () => observer.disconnect()
  }, [])

  useEffect(() => {
    schedulerRef.current?.setOptions({ lowSize: LOW_SIZE, fullSize, settleMs: SETTLE_MS })
  }, [fullSize])

  useEffect(() => {
    schedulerRef.current?.update(document)
  }, [document])

  return (
    <div class="preview" ref={frameRef}>
      <div class="preview-image checkerboard">
        {imageUrl && <img src={imageUrl} alt={document.name} draggable={false} />}
      </div>
      {overlay && <div class="preview-overlay">{overlay}</div>}
      <div class={`preview-busy ${busy ? 'is-busy' : ''}`} aria-hidden="true" />
      {error && (
        <div class="preview-error" role="alert">
          {error}
        </div>
      )}
    </div>
  )
}
