import type { ComponentChildren } from 'preact'
import { useEffect, useRef, useState } from 'preact/hooks'
import { CanvasPreview } from '../canvas-preview'
import { draw, enqueue, onContextChange } from '../gpu/editor'
import { defaultView, type ViewOptions } from '../view'
import type { TextureDocument } from '../types'

const LOW_SIZE = 96, MAX_SIZE = 1024, SETTLE_MS = 180
interface State { document: TextureDocument; view: ViewOptions }
interface Props { document: TextureDocument; overlay?: ComponentChildren; view?: ViewOptions; interactive?: boolean }

/** Display GPU frames directly; projected handles share the exact canvas square. */
export function Preview({ document, overlay, view = defaultView, interactive = false }: Props) {
  const frameRef = useRef<HTMLDivElement>(null)
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const latest = useRef<State>({ document, view }); latest.current = { document, view }
  const schedulerRef = useRef<CanvasPreview<State> | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [busy, setBusy] = useState(false)
  const [fullSize, setFullSize] = useState(512)

  useEffect(() => {
    const scheduler = new CanvasPreview<State>(
      (state, size) => { draw(canvasRef.current!, state.document, state.view, size); setError(null) },
      (work) => enqueue(work),
      (failure) => setError(failure instanceof Error ? failure.message : String(failure)),
      setBusy,
      { lowSize: LOW_SIZE, fullSize, settleMs: SETTLE_MS, interactive },
    )
    schedulerRef.current = scheduler
    const unsubscribe = onContextChange((restored) => {
      if (restored) scheduler.update(latest.current)
      else { scheduler.dispose(); setError('Graphics context lost. Waiting for the browser to restore it…') }
    })
    return () => { unsubscribe(); scheduler.dispose(); schedulerRef.current = null }
  }, [])

  useEffect(() => {
    const observer = new ResizeObserver(([entry]) => {
      const pixels = entry.contentRect.width * window.devicePixelRatio
      setFullSize(Math.min(MAX_SIZE, Math.max(64, Math.ceil(pixels / 64) * 64)))
    })
    observer.observe(frameRef.current!)
    return () => observer.disconnect()
  }, [])
  useEffect(() => {
    schedulerRef.current?.setOptions({ lowSize: LOW_SIZE, fullSize, settleMs: SETTLE_MS, interactive })
  }, [fullSize, interactive])
  useEffect(() => { schedulerRef.current?.update({ document, view }) }, [document, view])

  return <div class="preview" ref={frameRef}>
    <div class="preview-image checkerboard"><canvas ref={canvasRef} role="img" aria-label={document.name} /></div>
    {overlay && <div class="preview-overlay">{overlay}</div>}
    <div class={`preview-busy ${busy ? 'is-busy' : ''}`} aria-hidden="true" />
    {error && <div class="preview-error" role="alert">{error}</div>}
  </div>
}
