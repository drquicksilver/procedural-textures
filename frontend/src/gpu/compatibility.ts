// Secondary browser harness, served only by gpu-compatibility.mjs.
import materials from '../../../test-vectors/gpu-materials.json'
import geometry from '../../../test-vectors/gpu-geometry.json'
import { compileMaterial } from './compiler'
import type { TextureDocument } from '../types'
import type { DistanceNode } from './geometry'
import type { GpuRenderer } from './renderer'
import type { ViewOptions } from '../view'

interface Measurement { png: string; firstMs: number; programCompileMs: number; steadyMs: number[]; readMs: number; pngMs: number; compilations: number; renderer: string }
const spike = (window as unknown as { gpuSpike: { renderer: GpuRenderer; defaultView: ViewOptions; render: (name: string, view: ViewOptions, size: number, repeats: number) => Promise<Measurement>; shapes: string[]; examples: string[] } }).gpuSpike
const status = document.querySelector('#status')!
const pause = () => new Promise((resolve) => setTimeout(resolve, 0))
async function run() {
  const samples = []
  for (const item of materials.materials) {
    status.textContent = `Checking ${item.name}`
    const doc = { version: 4, name: item.name, description: '', texture: item.texture } as unknown as TextureDocument
    const values = spike.renderer.samples(doc, item.samples.map((s) => s.slice(0, 3)))
    const errors = item.samples.flatMap((s, i) => s.slice(3).map((v, c) => Math.abs(v - values[i * 4 + c])))
    const maxError = Math.max(...errors)
    samples.push({ name: item.name, maxError, tolerance: item.tolerance, pass: Number.isFinite(maxError) && maxError <= item.tolerance })
    await pause()
  }
  const doc: TextureDocument = { version: 4, name: 'Diagnostic', description: '', texture: { type: 'flat', colour: '#ffffffff' } }
  const noise = spike.renderer.samples(doc, materials.noise.map((s) => s.slice(0, 3)), 'noise')
  const noiseError = Math.max(...materials.noise.map((s, i) => Math.abs(s[3] - noise[i * 4])))
  samples.push({ name: 'noise', maxError: noiseError, tolerance: 1e-5, pass: Number.isFinite(noiseError) && noiseError <= 1e-5 })
  for (const item of geometry.cases) {
    const values = spike.renderer.sampleCompiled(compileMaterial(doc, { diagnostic: 'distance', geometry: item.solid as DistanceNode }), item.samples.map((s) => s.slice(0, 3)))
    const maxError = Math.max(...item.samples.map((s, i) => Math.abs(s[3] - values[i * 4])))
    samples.push({ name: `geometry-${item.name}`, maxError, tolerance: 1e-5, pass: Number.isFinite(maxError) && maxError <= 1e-5 })
  }
  const measurements = []
  const cases = spike.shapes.slice(0, 7).flatMap((shape) => ['checker', 'marble', 'cumulus'].map((name) => ({ name, shape, size: 512, distance: 2.1 })))
  cases.push(...['checker', 'marble', 'cumulus'].map((name) => ({ name, shape: 'bitten-cube', size: 96, distance: 2.1 })),
    { name: 'marble', shape: 'bitten-cube', size: 1024, distance: 2.1 },
    { name: 'cumulus', shape: 'bitten-cube', size: 1024, distance: 2.1 },
    ...[96, 512, 1024].map((size) => ({ name: 'cumulus', shape: 'bitten-cube', size, distance: 1.1 })),
    ...['checker', 'marble'].map((name) => ({ name, shape: 'knight', size: 512, distance: 2.1 })))
  for (const c of cases) {
    status.textContent = `Measuring ${c.name} / ${c.shape} / ${c.size}`
    const { png: _, ...timing } = await spike.render(c.name, { ...spike.defaultView, mode: 'scene', shape: c.shape, distance: c.distance }, c.size, 7)
    measurements.push({ ...c, ...timing }); await pause()
  }
  const images = []
  for (const name of spike.examples) {
    status.textContent = `Golden ${name}`
    images.push({ file: `${name}.png`, kind: 'textures', png: (await spike.render(name, { ...spike.defaultView, mode: 'slice' }, 128, 0)).png }); await pause()
  }
  for (const shape of spike.shapes) for (const name of ['checker', 'marble', 'malachite']) {
    images.push({ file: `${shape}-${name}.png`, kind: 'scenes', png: (await spike.render(name, { ...spike.defaultView, mode: 'scene', shape }, 96, 0)).png }); await pause()
  }
  // Count live GL objects across repeated structural edits and renderer disposal.
  const gl = document.querySelector('canvas')!.getContext('webgl2')!
  const live = new Map<string, Set<unknown>>()
  for (const kind of ['Program', 'Shader', 'Texture', 'Framebuffer', 'VertexArray']) {
    const set = new Set<unknown>(); live.set(kind, set)
    const api = gl as unknown as Record<string, (...args: unknown[]) => unknown>
    const create = api[`create${kind}`].bind(gl), remove = api[`delete${kind}`].bind(gl)
    api[`create${kind}`] = (...args) => { const value = create(...args); if (value) set.add(value); return value }
    api[`delete${kind}`] = (value) => { set.delete(value); return remove(value) }
  }
  const { GpuRenderer } = await import('./renderer')
  const stress = new GpuRenderer(gl)
  for (let i = 0; i < 160; i++) {
    let texture = { type: 'flat', colour: '#123456ff' } as TextureDocument['texture']
    for (let j = 0; j < i % 16; j++) texture = { type: 'layer', top: texture, bottom: { type: 'flat', colour: '#abcdef80' } }
    stress.render({ ...doc, texture }, { ...spike.defaultView, mode: 'slice' }, 96)
    if (live.get('Program')!.size > 8) throw new Error('Program cache grew beyond eight entries')
  }
  const resources = Object.fromEntries([...live].map(([k, v]) => [k, v.size]))
  stress.dispose()
  const afterDispose = Object.fromEntries([...live].map(([k, v]) => [k, v.size]))
  if (Object.values(afterDispose).some((n) => n !== 0)) throw new Error('Renderer leaked GL objects after disposal')
  const ext = gl.getExtension('WEBGL_debug_renderer_info')
  return { userAgent: navigator.userAgent, backend: ext ? gl.getParameter(ext.UNMASKED_RENDERER_WEBGL) : gl.getParameter(gl.RENDERER), samples, measurements, images, resources, afterDispose }
}
try {
  const result = await run()
  const failures = result.samples.filter((s) => !s.pass)
  status.textContent = failures.length ? `FAIL: ${JSON.stringify(failures)}` : `PASS: ${result.samples.length} sample cases, ${result.images.length} golden images`
  await fetch(`/__compatibility/${new URLSearchParams(location.search).get('token')}`, { method: 'POST', body: JSON.stringify(result) })
} catch (error) {
  status.textContent = String(error)
  await fetch(`/__compatibility/${new URLSearchParams(location.search).get('token')}`, { method: 'POST', body: JSON.stringify({ error: String(error), userAgent: navigator.userAgent }) })
}
