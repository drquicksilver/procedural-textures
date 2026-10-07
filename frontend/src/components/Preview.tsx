import {prepareDocument} from '../gpu/preparation'
import type { ComponentChildren } from 'preact'
import { useEffect, useLayoutEffect, useMemo, useRef, useState } from 'preact/hooks'
import { AdaptiveResolution } from '../resolution'
import { CanvasPreview } from '../canvas-preview'
import { draw, enqueue, onContextChange } from '../gpu/editor'
import { defaultView, type ViewOptions } from '../view'
import { materialStructure } from '../gpu/material'
import type { TextureDocument } from '../types'

const MAX_SIZE = 1024, SETTLE_MS = 180
interface State { document: TextureDocument; view: ViewOptions; structure: string }
interface Props { document: TextureDocument; overlay?: ComponentChildren; view?: ViewOptions; interactive?: boolean }

/** Display GPU frames directly; projected handles share the exact canvas square. */
export function Preview({ document, overlay, view = defaultView, interactive = false }: Props) {
  const frameRef = useRef<HTMLDivElement>(null)
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const structure = useMemo(() => JSON.stringify([view.mode, view.shape, materialStructure(document)]), [document, view.mode, view.shape])
  const presented = useRef('')
  const latest = useRef<State>({ document, view, structure }); latest.current = { document, view, structure }
  const resolution = useRef(new AdaptiveResolution(15))
  const sequence = useRef(0)
  const epoch = useRef(0)
  const schedulerRef = useRef<CanvasPreview<State> | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [simulating,setSimulating]=useState(false)
  const [busy, setBusy] = useState(false)
  const [fullSize, setFullSize] = useState(512)
  const sizeRef = useRef(fullSize); sizeRef.current = fullSize
  const adaptiveSize = useRef(() => resolution.current.size(sizeRef.current))

  useLayoutEffect(() => {
    const scheduler = new CanvasPreview<State>(
      (state, size) => {
        const id = ++sequence.current, currentEpoch = epoch.current
        draw(canvasRef.current!, state.document, state.view, size, true, (ms, source) => {
          if (epoch.current !== currentEpoch) return
          resolution.current.sample(size, ms, id)
          canvasRef.current!.dataset.frameMs = ms.toFixed(2)
          canvasRef.current!.dataset.timingSource = source
          canvasRef.current!.dataset.budgetMs = String(resolution.current.budgetMs)
          canvasRef.current!.dataset.adaptiveSize = String(adaptiveSize.current())
        })
        presented.current = state.structure
        setError(null);setSimulating(false)
      },
      (work) => enqueue(work, 0, latest.current.structure !== presented.current),
      (failure) => {setSimulating(false);setError(failure instanceof Error ? failure.message : String(failure))},
      setBusy,
      { previewSize: adaptiveSize.current, fullSize, settleMs: SETTLE_MS, interactive },
      (state)=>{const task=prepareDocument(state.document);if(task) setSimulating(true);return task},
    )
    schedulerRef.current = scheduler
    const unsubscribe = onContextChange((restored) => {
      epoch.current++; presented.current = ''; resolution.current.reset()
      if (restored) scheduler.update(latest.current)
      else { scheduler.dispose(); setError('Graphics context lost. Waiting for the browser to restore it…') }
    })
    return () => { epoch.current++; unsubscribe(); scheduler.dispose(); schedulerRef.current = null }
  }, [])

  useEffect(() => {
    const observer = new ResizeObserver(([entry]) => {
      const pixels = entry.contentRect.width * window.devicePixelRatio
      setFullSize(Math.min(MAX_SIZE, Math.max(64, Math.ceil(pixels / 64) * 64)))
    })
    observer.observe(frameRef.current!)
    return () => observer.disconnect()
  }, [])
  useLayoutEffect(() => {
    schedulerRef.current?.setOptions({ previewSize: adaptiveSize.current, fullSize, settleMs: SETTLE_MS, interactive })
  }, [fullSize, interactive])
  useLayoutEffect(() => { schedulerRef.current?.update({ document, view, structure }) }, [document, view])

  return <div class="preview" ref={frameRef}>
    <div class="preview-image checkerboard"><canvas ref={canvasRef} role="img" aria-label={document.name} /></div>
    {overlay && <div class="preview-overlay">{overlay}</div>}
    <div class={`preview-busy ${busy ? 'is-busy' : ''} ${busy && structure !== presented.current ? 'is-preparing' : ''}`} role="status" aria-label={busy ? 'Preparing preview' : undefined} aria-hidden={!busy} />
    {busy && simulating && <div class="simulation-status" role="status">Simulating material…</div>}
    {error && <div class="preview-error" role="alert">{error}</div>}
  </div>
}
