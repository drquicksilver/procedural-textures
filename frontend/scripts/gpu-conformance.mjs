import assert from 'node:assert/strict'
import { readFileSync, writeFileSync, mkdirSync } from 'node:fs'
import { join } from 'node:path'
import { gpuSession, root } from './gpu-session.mjs'

const args = process.argv.slice(2), filter = args.includes('--case') ? args[args.indexOf('--case') + 1] : undefined
if (args.includes('--case') && (!filter || filter.startsWith('--'))) throw new Error('Missing value for --case')
const session = await gpuSession()
session.page.on('console', (message) => { if (message.text().startsWith('Checking ')) console.log(message.text()) })
let failed = false
async function run() {
  const vectors = JSON.parse(readFileSync(join(root, '../test-vectors/gpu-materials.json'), 'utf8'))
  const geometry = JSON.parse(readFileSync(join(root, '../test-vectors/gpu-geometry.json'), 'utf8'))
  const evaluate = async (vectors, geometry) => await session.page.evaluate(async ({ vectors, geometry, filter, mutate }) => {
    const { compileMaterial } = await import('/src/gpu/compiler.ts')
    const renderer = window.gpuSpike.renderer
    const report = []
    for (const item of vectors.materials.filter((v) => !filter || v.name === filter)) {
      console.log(`Checking material ${item.name}`)
      const document = { version: 4, name: item.name, description: '', texture: item.texture }
      const points = item.samples.map((v) => v.slice(0, 3))
      let values
      if (mutate) {
        const compiled = compileMaterial(document, { diagnostic: 'material' })
        compiled.source = compiled.source.replace('outputColour=material(p);', 'outputColour=vec4(0,1,0,1);')
        values = renderer.sampleCompiled(compiled, points)
      } else values = renderer.samples(document, points)
      let maxError = 0, worst
      for (let i = 0; i < item.samples.length; i++) for (let c = 0; c < 4; c++) {
        const actual = values[i * 4 + c], expected = item.samples[i][c + 3], error = Math.abs(actual - expected)
        if (!Number.isFinite(actual)) throw new Error(`${item.name}: non-finite result`)
        if (error > maxError) { maxError = error; worst = { point: points[i], channel: c, expected, actual } }
      }
      report.push({ name: item.name, maxError, worst, tolerance: item.tolerance, pass: maxError <= item.tolerance })
    }
    if (vectors.noise.length && (!filter || filter === 'noise')) {
      const doc = { version: 4, name: 'Noise', description: '', texture: { type: 'flat', colour: '#ffffffff' } }
      const values = renderer.samples(doc, vectors.noise.map((v) => v.slice(0, 3)), 'noise')
      const maxError = Math.max(...vectors.noise.map((v, i) => Math.abs(v[3] - values[i * 4])))
      report.push({ name: 'noise', maxError, pass: Number.isFinite(maxError) && maxError <= 0.00001 })
    }
    for (const item of geometry.cases.filter((v) => !filter || v.name === filter)) {
      console.log(`Checking geometry ${item.name}`)
      const doc = { version: 4, name: item.name, texture: { type: 'flat', colour: '#ffffffff' } }
      const values = renderer.sampleCompiled(compileMaterial(doc, { diagnostic: 'distance', geometry: item.solid }), item.samples.map((v) => v.slice(0, 3)))
      const maxError = Math.max(...item.samples.map((v, i) => Math.abs(v[3] - values[i * 4])))
      const worstIndex = item.samples.findIndex((v, i) => Math.abs(v[3] - values[i * 4]) === maxError)
      report.push({ name: `geometry-${item.name}`, maxError, worst: { sample: item.samples[worstIndex], actual: values[worstIndex * 4] }, pass: Number.isFinite(maxError) && maxError <= 0.00001 })
    }
    return report
  }, { vectors, geometry, filter, mutate: args.includes('--mutate') })
  // One protocol call per case: software compilers can take seconds per shader,
  // but a whole-suite call exceeds Puppeteer's timeout as the library grows.
  const results = []
  for (const item of vectors.materials.filter((v) => !filter || v.name === filter)) results.push(...await evaluate({ materials: [item], noise: [] }, { cases: [] }))
  if (!filter || filter === 'noise') results.push(...await evaluate({ materials: [], noise: vectors.noise }, { cases: [] }))
  for (const item of geometry.cases.filter((v) => !filter || v.name === filter)) results.push(...await evaluate({ materials: [], noise: [] }, { cases: [item] }))
  if (args.includes('--self-test')) {
    const checks = await session.page.evaluate(async () => {
      const { compileMaterial } = await import('/src/gpu/compiler.ts')
      const doc = { version: 4, name: 'Mutation', description: '', texture: { type: 'flat', colour: '#ffffffff' } }
      const correct = compileMaterial(doc, { diagnostic: 'material' })
      const wrong = { ...correct, source: correct.source.replace('outputColour=material(p);', 'outputColour=vec4(0,1,0,1);') }
      const pixel = window.gpuSpike.renderer.sampleCompiled(wrong, [[0, 0, 0]])
      if (Math.abs(pixel[0] - 1) <= 0.00005) throw new Error('Wrong shader was not detected')
      try { window.gpuSpike.renderer.sampleCompiled({ ...correct, source: correct.source + '\ninvalid GLSL' }, [[0, 0, 0]]) }
      catch (error) {
        if (!String(error).includes('$.texture') || !String(error).includes('invalid GLSL')) throw error
        return true
      }
      throw new Error('Invalid shader compiled successfully')
    })
    await session.page.evaluate(async () => {
      const { compileMaterial } = await import('/src/gpu/compiler.ts')
      const warp = { type: 'turbulence', amount: 0.2, octaves: 3, persistence: 0.5, lacunarity: 2, base: { type: 'flat', colour: '#ffffff80' } }
      const doc = { version: 4, name: 'Sharing', texture: { type: 'layer', top: warp, bottom: structuredClone(warp) } }
      const count = (texture) => {
        const compiled = compileMaterial({ ...doc, texture }, { diagnostic: 'material' })
        compiled.source = compiled.source.replace('vec3 rawWarp(vec3 p,int config) { return', 'int warpEvaluations=0; vec3 rawWarp(vec3 p,int config) { ++warpEvaluations; return')
          .replace('outputColour=material(p);', 'vec4 colour=material(p); outputColour=vec4(float(warpEvaluations),colour.gba);')
        return window.gpuSpike.renderer.sampleCompiled(compiled, [[0.25, 0.5, 0.75]])[0]
      }
      if (count(doc.texture) !== 1) throw new Error('Layer did not reuse its identical warp sample')
      doc.texture.bottom.lacunarity = 2 + 1e-8
      if (count(doc.texture) !== 2) throw new Error('Layer confused distinct configurations rounded to the same FP32 value')
      doc.texture.bottom.lacunarity = 1.7
      if (count(doc.texture) !== 2) throw new Error('Layer reused a different noise configuration')
      const nested = { ...warp, base: structuredClone(warp) }
      if (count(nested) !== 2) throw new Error('Nested warp reused a sample from another coordinate domain')
    })
    console.log('PASS warp sharing and coordinate-domain isolation')
    await session.page.evaluate(async () => {
      const { renderer, defaultView } = window.gpuSpike
      const doc = { version: 4, name: 'Restore', texture: { type: 'flat', colour: '#123456ff' } }
      const view = { ...defaultView, mode: 'slice' }
      renderer.render(doc, view, 16)
      const before = renderer.readPixels(16), gl = document.querySelector('canvas').getContext('webgl2')
      const extension = gl.getExtension('WEBGL_lose_context')
      if (!extension) throw new Error('Missing context-loss test extension')
      const event = (name) => new Promise((resolve, reject) => {
        const timer = setTimeout(() => reject(new Error(`${name} timed out`)), 5000)
        gl.canvas.addEventListener(name, () => { clearTimeout(timer); resolve() }, { once: true })
      })
      const lost = event('webglcontextlost'); extension.loseContext(); await lost
      try { renderer.render(doc, view, 16); throw new Error('Lost context accepted a render') }
      catch (error) { if (!String(error).includes('unavailable')) throw error }
      const restored = event('webglcontextrestored'); await new Promise((resolve) => setTimeout(resolve, 50)); extension.restoreContext(); await restored
      renderer.render(doc, view, 16)
      if (!renderer.readPixels(16).every((v, i) => v === before[i])) throw new Error('Context restoration changed the document render')
      try { renderer.render(doc, view, 2049); throw new Error('Oversized render accepted') }
      catch (error) { if (!String(error).includes('Unsupported render size')) throw error }
    })
    console.log('PASS context loss, restoration and resource limits')
    assert.ok(checks)
    console.log('PASS intentionally wrong shader detection and annotated compile diagnostics')
  }
  assert.ok(results.length, `No case matched ${filter}`)
  for (const r of results) console.log(`${r.pass ? 'PASS' : 'FAIL'} ${r.name}: ${r.maxError.toExponential(3)}${r.pass ? '' : ` ${JSON.stringify(r.worst)}`}`)
  failed = results.some((r) => !r.pass)
  mkdirSync(join(root, '../out/gpu-conformance'), { recursive: true })
  const suffix = `${/SwiftShader/i.test(session.backend) ? 'software' : 'hardware'}${filter ? `-${filter}` : ''}${args.includes('--mutate') ? '-mutation' : ''}`
  writeFileSync(join(root, `../out/gpu-conformance/samples-${suffix}.json`), JSON.stringify({ browser: await session.browser.version(), backend: session.backend, results }, null, 2) + '\n')
  if (failed && !args.includes('--watch')) throw new Error('GPU sample conformance failed')
}
try {
  await run()
  if (args.includes('--watch')) {
    console.log('Watching shaders, fixtures and assets; retaining browser/context. Ctrl-C to stop.')
    let pending, running = false
    // HMR updates the renderer module in the same context; importing with a fresh
    // timestamp below refreshes the instance after a change without relaunching Chrome.
    const rerun = async () => {
      if (running) { pending = true; return }
      running = true
      try {
        const timestamp = Date.now()
        const graph = session.server.environments.client.moduleGraph
        for (const module of graph.idToModuleMap.values()) graph.invalidateModule(module, new Set(), timestamp, true)
        await session.page.evaluate(async (timestamp) => {
          const { GpuRenderer } = await import(`/src/gpu/renderer.ts?t=${timestamp}`)
          const gl = document.querySelector('canvas').getContext('webgl2')
          window.gpuSpike.renderer.dispose(); window.gpuSpike.renderer = new GpuRenderer(gl)
        }, timestamp)
        await run()
      } catch (error) { console.error(error); failed = true }
      finally { running = false; if (pending) { pending = false; void rerun() } }
    }
    let timer
    session.server.watcher.on('change', (path) => {
      if (!/src\/gpu\/|test-vectors\/|examples\/|ramps\//.test(path)) return
      clearTimeout(timer); timer = setTimeout(() => void rerun(), 100)
    })
    await new Promise((resolve) => { process.once('SIGINT', resolve); process.once('SIGTERM', resolve) })
  }
} finally { await session.close() }
if (failed) process.exitCode = 1
