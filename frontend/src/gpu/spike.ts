import type { TextureDocument } from '../types'
import { defaultView, orbit, zoom, type ViewOptions, type SliceAxis } from '../view'
import { shapeNames } from './geometry'
import { GpuRenderer } from './renderer'

import { rgbaPng,pngDataUrl } from '../png'
import metadata from '../metadata'
const examples = Object.fromEntries(metadata.examples.map((e) => [e.id, e.document as TextureDocument]))
const canvas = document.querySelector<HTMLCanvasElement>('#preview')!
const status = document.querySelector<HTMLElement>('#status')!
const gl = canvas.getContext('webgl2', { antialias: false, alpha: false, preserveDrawingBuffer: false })
if (!gl) throw new Error('WebGL2 unavailable')
const renderer = new GpuRenderer(gl)
let view = { ...defaultView }
let queued = false
const select = (id: string) => document.querySelector<HTMLSelectElement>(id)!
select('#material').replaceChildren(...Object.keys(examples).sort().map((id) => new Option(id, id)))
select('#shape').replaceChildren(...shapeNames.map((id) => new Option(id, id)))
select('#material').value = 'checker'; select('#shape').value = defaultView.shape
const position = document.querySelector<HTMLInputElement>('#position')!

/** Also used by Puppeteer: no timing includes PNG encoding or browser startup. */
const render = async (name: string, options: ViewOptions, size: number, repeats = 5) => {
  const doc = examples[name]
  if (!doc) throw new Error(`Unknown spike example: ${name}`)
  await renderer.prepare(doc)
  const started = performance.now(), before = renderer.programCompilations
  renderer.render(doc, options, size); renderer.complete()
  const firstMs = performance.now() - started, programCompileMs = renderer.lastProgramCompileMs
  const steady: number[] = []
  for (let i = 0; i < repeats; i++) {
    const start = performance.now()
    renderer.render(doc, options, size); renderer.complete()
    steady.push(performance.now() - start)
  }
  const readStart = performance.now(), pixels = renderer.readPixels(size), readMs = performance.now() - readStart
  const pngStart = performance.now(), png = await pngDataUrl(await rgbaPng(pixels,size)), pngMs = performance.now() - pngStart
  const extension = gl.getExtension('WEBGL_debug_renderer_info')
  return {
    png, firstMs, programCompileMs, steadyMs: steady, readMs, pngMs,
    compilations: renderer.programCompilations - before,
    renderer: extension ? gl.getParameter(extension.UNMASKED_RENDERER_WEBGL) : gl.getParameter(gl.RENDERER),
  }
}

const draw = async () => {
  queued = false
  try {
    const plane = select('#view').value
    view = { ...view, shape: select('#shape').value, mode: plane === 'scene' ? 'scene' : 'slice', axis: plane === 'scene' ? 'xy' : plane as SliceAxis, position: Number(position.value) }
    const document=examples[select('#material').value];await renderer.prepare(document)
    const started = performance.now()
    renderer.render(examples[select('#material').value], view, Number(select('#size').value))
    status.textContent = `Submitted in ${(performance.now() - started).toFixed(2)} ms; ${renderer.programCompilations} program compilations.\nGPU completion is measured separately by the CLI harness.`
  } catch (error) { status.textContent = String(error) }
}
const schedule = () => { if (!queued) { queued = true; requestAnimationFrame(draw) } }
for (const id of ['#material', '#shape', '#view', '#size']) select(id).addEventListener('change', schedule)
position.addEventListener('input', schedule)
let drag: [number, number] | undefined
canvas.addEventListener('pointerdown', (e) => { drag = [e.clientX, e.clientY]; canvas.setPointerCapture(e.pointerId) })
canvas.addEventListener('pointermove', (e) => {
  if (!drag) return
  view = orbit(view, e.clientX - drag[0], e.clientY - drag[1]); drag = [e.clientX, e.clientY]; schedule()
})
canvas.addEventListener('pointerup', () => { drag = undefined })
canvas.addEventListener('pointercancel', () => { drag = undefined })
canvas.addEventListener('wheel', (e) => { e.preventDefault(); view = zoom(view, e.deltaY); schedule() }, { passive: false })
canvas.addEventListener('webglcontextrestored', schedule)
window.addEventListener('pagehide', () => renderer.dispose())
// Explicit small harness API, usable from the console as well as Puppeteer.
Object.assign(window, { gpuSpike: { render, examples: Object.keys(examples), shapes: shapeNames, renderer, defaultView } })
if (!new URLSearchParams(location.search).has('harness')) draw()
