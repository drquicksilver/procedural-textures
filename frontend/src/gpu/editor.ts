import type { TextureDocument } from '../types'
import type { ViewOptions } from '../view'
import { GpuRenderer } from './renderer'

interface Job { work: () => void; priority: number }
const jobs: Job[] = []
let frame: number | null = null

/** One submission per animation frame; viewer/export work precedes thumbnails. */
export function enqueue(work: () => void, priority = 0): () => void {
  const job = { work, priority }
  jobs.push(job)
  pump()
  return () => {
    const index = jobs.indexOf(job)
    if (index >= 0) jobs.splice(index, 1)
    if (!jobs.length && frame !== null) { cancelAnimationFrame(frame); frame = null }
  }
}
function pump(): void {
  if (frame !== null || !jobs.length) return
  frame = requestAnimationFrame(() => {
    frame = null
    jobs.sort((a, b) => a.priority - b.priority)
    const job = jobs.shift()!
    try { job.work() } finally { pump() }
  })
}
let renderer: GpuRenderer | null = null
let canvas: HTMLCanvasElement | null = null
const listeners = new Set<(restored: boolean) => void>()

function getRenderer(): GpuRenderer {
  if (renderer) return renderer
  const source = document.createElement('canvas')
  const gl = source.getContext('webgl2', { antialias: false, alpha: true, premultipliedAlpha: false, preserveDrawingBuffer: false })
  if (!gl) throw new Error('WebGL2 is unavailable. Enable hardware acceleration or use a browser with WebGL2 support.')
  renderer = new GpuRenderer(gl)
  canvas = source
  source.hidden = true; source.dataset.renderer = 'shared'
  document.body.append(source)
  installCleanup()
  // GpuRenderer registers first: resources have been recreated before redraws.
  source.addEventListener('webglcontextlost', () => { for (const listener of listeners) listener(false) })
  source.addEventListener('webglcontextrestored', () => { for (const listener of listeners) listener(true) })
  return renderer
}
export function onContextChange(listener: (restored: boolean) => void): () => void {
  listeners.add(listener)
  return () => { listeners.delete(listener) }
}

/** GPU presentation/copy only: no PNG, ImageData or readPixels on interactive paths. */
export function draw(target: HTMLCanvasElement, document: TextureDocument, view: ViewOptions, size: number, retain = true): void {
  const gpu = getRenderer()
  gpu.render(document, view, size)
  canvas!.dataset.compilations = String(gpu.programCompilations)
  if (retain) gpu.retainPresentedProgram()
  const ctx = target.getContext('2d')
  if (!ctx) throw new Error('Canvas display is unavailable')
  if (target.width !== size || target.height !== size) { target.width = size; target.height = size }
  ctx.clearRect(0, 0, size, size)
  ctx.drawImage(canvas!, 0, 0)
  target.dataset.rendered = 'true'
  target.dataset.view = JSON.stringify(view)
  target.dataset.compilations = String(gpu.programCompilations)
  target.dataset.frame = String(Number(target.dataset.frame ?? 0) + 1)
}

/** Explicit export uses the renderer's RGBA target, including uncomposited slice alpha. */
export function exportPng(document: TextureDocument, view: ViewOptions, size: number): Promise<Blob> {
  return new Promise((resolve, reject) => enqueue(() => {
    try {
      const gpu = getRenderer()
      gpu.render(document, view, size)
      const output = window.document.createElement('canvas')
      output.width = size; output.height = size
      output.getContext('2d')!.putImageData(new ImageData(new Uint8ClampedArray(gpu.readPixels(size)), size, size), 0, 0)
      output.toBlob((blob) => { output.width = output.height = 0; if (blob) resolve(blob); else reject(new Error('PNG encoding failed')) }, 'image/png')
    } catch (error) { reject(error) }
  }, -1))
}
function installCleanup(): void {
  window.addEventListener('pagehide', (event) => {
    if (event.persisted) return // Back/forward cache resumes the same page and resources.
    renderer?.dispose(); renderer = null; canvas?.remove(); canvas = null
    jobs.length = 0
    if (frame !== null) cancelAnimationFrame(frame)
    frame = null
  })
}
