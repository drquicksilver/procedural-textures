import { useEffect, useMemo, useRef, useState } from 'preact/hooks'
import { fetchShapes } from '../api'
import { exportPng } from '../gpu/editor'
import { defaultView, orbit, zoom, type ViewOptions, type ShapeOption, type SliceAxis } from '../view'
import type { TextureDocument, Schema, Node, Json } from '../types'
import { inspectionTexture } from '../tree'
import { Preview } from './Preview'
import { Handles } from './Handles'

interface Props {
  document: TextureDocument
  schema: Schema
  node: Node | null
  onChange: (field: string, value: Json) => void
}

export function Viewer({ document, schema, node, onChange }: Props) {
  const [inspect, setInspect] = useState(false)
  const previewDocument = useMemo(() => inspect && node ? { ...document, texture: inspectionTexture(schema, node) } : document, [inspect,node,document,schema])
  const [view, setView] = useState<ViewOptions>(defaultView)
  const [shapes, setShapes] = useState<ShapeOption[]>([])
  const [shapeError, setShapeError] = useState<string | null>(null)
  const [exportSize, setExportSize] = useState(512)
  const [exporting, setExporting] = useState(false)
  const [exportError, setExportError] = useState<string | null>(null)
  const download = async () => {
    setExporting(true); setExportError(null)
    try {
      const blob = await exportPng(previewDocument, { ...view }, exportSize)
      const url = URL.createObjectURL(blob), link = window.document.createElement('a')
      link.href = url
      const name = document.name.replace(/[^a-z0-9_-]+/gi, '-').replace(/^-|-$/g, '') || 'texture'
      link.download = `${name}${inspect ? '-field' : ''}-${view.mode === 'scene' ? view.shape : `${view.axis}-${view.position.toFixed(3)}`}-${exportSize}.png`
      link.click(); setTimeout(() => URL.revokeObjectURL(url), 1000)
    } catch (error) { setExportError(error instanceof Error ? error.message : String(error)) }
    finally { setExporting(false) }
  }
  const [dragging, setDragging] = useState(false)
  const [scrubbing, setScrubbing] = useState(false)
  const pointer = useRef<[number, number] | null>(null)
  const wheelTimer = useRef<ReturnType<typeof setTimeout> | null>(null)
  const patch = (update: Partial<ViewOptions>) => setView((old) => ({ ...old, ...update }))
  useEffect(() => {
    let live = true
    fetchShapes().then((values) => { if (live) setShapes(values) }, (error) => { if (live) setShapeError(String(error)) })
    return () => { live = false; if (wheelTimer.current) clearTimeout(wheelTimer.current) }
  }, [])
  const endDrag = () => { pointer.current = null; setDragging(false) }
  return <div class="viewer">
    <div class="viewer-controls">
      <label><input type="checkbox" aria-label="Inspect selected field" checked={inspect} onChange={(e) => setInspect(e.currentTarget.checked)} /> Inspect selected field</label>
      <label>View <select aria-label="View" value={view.mode} onChange={(e) => patch({ mode: e.currentTarget.value as 'scene' | 'slice' })}>
        <option value="scene">3D solid</option><option value="slice">2D slice</option>
      </select></label>
      {view.mode === 'scene' ? <>
        <label>Shape <select aria-label="Shape" value={view.shape} onChange={(e) => patch({ shape: e.currentTarget.value })}>
          {shapes.map((shape) => <option key={shape.id} value={shape.id}>{shape.label}</option>)}
        </select></label>
        <button onClick={() => setView({ ...view, yaw: defaultView.yaw, pitch: defaultView.pitch, distance: defaultView.distance })}>Reset camera</button>
      </> : <>
        <label>Plane <select aria-label="Slice plane" value={view.axis} onChange={(e) => patch({ axis: e.currentTarget.value as SliceAxis })}>
          <option value="xy">XY · depth z</option><option value="xz">XZ · depth y</option><option value="yz">YZ · depth x</option>
        </select></label>
        <label class="slice-position">Position <input aria-label="Slice position" type="range" min="0" max="1" step="0.005" value={view.position}
          onPointerDown={(e) => { e.currentTarget.setPointerCapture(e.pointerId); setScrubbing(true) }} onPointerUp={() => setScrubbing(false)} onPointerCancel={() => setScrubbing(false)} onBlur={() => setScrubbing(false)}
          onInput={(e) => patch({ position: Number(e.currentTarget.value) })} /><output>{view.position.toFixed(3)}</output></label>
      </>}
    </div>
    <div class="viewer-export">
      <label>PNG size <select aria-label="PNG resolution" value={exportSize} onChange={(e) => setExportSize(Number(e.currentTarget.value))}>
        {[256, 512, 1024, 2048].map((size) => <option key={size} value={size}>{size} × {size}</option>)}
      </select></label>
      <button onClick={() => void download()} disabled={exporting}>{exporting ? 'Exporting…' : 'Download PNG'}</button>
    </div>
    {exportError && <p role="alert">{exportError}</p>}
    {shapeError && <p role="alert">Could not load shapes: {shapeError}</p>}
    <div class={`viewer-surface ${view.mode === 'scene' ? 'is-orbit' : ''}`} tabIndex={view.mode === 'scene' ? 0 : undefined}
      role={view.mode === 'scene' ? 'group' : undefined} aria-label={view.mode === 'scene' ? '3D camera controls' : undefined}
      onPointerDown={(e) => {
        if (view.mode !== 'scene' || e.button !== 0) return
        e.preventDefault(); e.currentTarget.setPointerCapture(e.pointerId)
        pointer.current = [e.clientX, e.clientY]; setDragging(true)
      }}
      onPointerMove={(e) => {
        if (!pointer.current) return
        const [x, y] = pointer.current
        pointer.current = [e.clientX, e.clientY]
        setView((old) => orbit(old, e.clientX - x, e.clientY - y))
      }}
      onPointerUp={endDrag} onPointerCancel={endDrag} onLostPointerCapture={endDrag}
      onWheel={(e) => {
        if (view.mode !== 'scene') return
        e.preventDefault(); setScrubbing(true)
        setView((old) => zoom(old, e.deltaY))
        if (wheelTimer.current) clearTimeout(wheelTimer.current)
        wheelTimer.current = setTimeout(() => setScrubbing(false), 160)
      }}
      onKeyDown={(e) => {
        if (view.mode !== 'scene') return
        if (['ArrowLeft','ArrowRight','ArrowUp','ArrowDown','+','-','Home'].includes(e.key)) e.preventDefault()
        if (e.key === 'ArrowLeft') setView((old) => orbit(old, -10, 0))
        if (e.key === 'ArrowRight') setView((old) => orbit(old, 10, 0))
        if (e.key === 'ArrowUp') setView((old) => orbit(old, 0, -10))
        if (e.key === 'ArrowDown') setView((old) => orbit(old, 0, 10))
        if (e.key === '+') setView((old) => zoom(old, -50))
        if (e.key === '-') setView((old) => zoom(old, 50))
        if (e.key === 'Home') patch({ yaw: defaultView.yaw, pitch: defaultView.pitch, distance: defaultView.distance })
      }}>
      <Preview document={previewDocument} view={view} interactive={dragging || scrubbing}
        overlay={view.mode === 'slice' && node ? <Handles schema={schema} node={node} axis={view.axis} position={view.position} onChange={onChange} /> : undefined} />
    </div>
    <p class="viewer-hint">{view.mode === 'scene' ? 'Drag to orbit · scroll to zoom · arrow keys orbit · +/− zoom · Home resets' : 'Move the plane through the material · drag projected points to edit'}</p>
  </div>
}
