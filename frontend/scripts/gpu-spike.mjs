// A single browser/context for the complete spike run. No Haskell server.
import assert from 'node:assert/strict'
import { execFileSync } from 'node:child_process'
import { existsSync, mkdirSync, writeFileSync } from 'node:fs'
import { join, resolve } from 'node:path'
import { gpuSession, root } from './gpu-session.mjs'

const args = process.argv.slice(2)
const option = (key, fallback) => {
  const index = args.indexOf(key)
  if (index < 0) return fallback
  if (!args[index + 1] || args[index + 1].startsWith('--')) throw new Error(`Missing value for ${key}`)
  return args[index + 1]
}
const example = option('--example', 'all'), size = Number(option('--size', '128'))
const mode = option('--view', 'all'), repeats = Number(option('--repeats', '5'))
const shape = option('--shape', 'bitten-cube')
if (!['all', 'scene', 'xy', 'xz', 'yz'].includes(mode)) throw new Error('Invalid --view')
if (!Number.isInteger(size) || size < 1 || size > 2048) throw new Error('Invalid --size')
if (!Number.isInteger(repeats) || repeats < 0 || repeats > 100) throw new Error('Invalid --repeats')
const maxThreshold = Number(option('--max-threshold', '0.008'))
if (!Number.isFinite(maxThreshold) || maxThreshold < 0) throw new Error('Invalid --max-threshold')
const out = resolve(option('--out', join(root, '..', 'out', 'gpu-spike')))
let session
try {
  session = await gpuSession()
  const { browser, page, server } = session
  const address = server.httpServer.address()
  if (args.includes('--check')) {
    await page.evaluate(() => {
      const { renderer, defaultView } = window.gpuSpike
      const doc = { version: 4, name: 'Cache test', description: '', texture: { type: 'tiled', columns: 8, rows: 8, depth: 1, a: { type: 'flat', colour: '#ffffffff' }, b: { type: 'flat', colour: '#000000ff' } } }
      const view = { ...defaultView, mode: 'slice' }
      renderer.render(doc, view, 32)
      const before = renderer.programCompilations, pixels = renderer.readPixels(32)
      doc.texture.columns = 3
      renderer.render(doc, view, 32)
      if (renderer.programCompilations !== before) throw new Error('Scalar edit recompiled shader')
      const edited = renderer.readPixels(32)
      if (!edited.some((v, i) => v !== pixels[i])) throw new Error('Scalar edit did not update pixels')
      renderer.render(doc, { ...view, yaw: 1, axis: 'xz', position: 0.5 }, 32)
      if (renderer.programCompilations !== before) throw new Error('View edit recompiled shader')
      doc.texture.a = { type: 'layer', top: doc.texture.a, bottom: doc.texture.b }
      renderer.render(doc, view, 32)
      if (renderer.programCompilations !== before + 1) throw new Error('Structural edit did not compile a new program')
    })
    console.log('GPU parameter updates, structural edits and program reuse passed')
  }
  mkdirSync(out, { recursive: true })
  const bin = (args.includes('--compare') || args.includes('--goldens')) ? join(execFileSync('stack', ['path', '--local-install-root'], { cwd: join(root, '..'), encoding: 'utf8' }).trim(), 'bin') : undefined
  const results = []
  let failures = 0
  const cases = []
  if (args.includes('--goldens')) {
    for (const name of await page.evaluate(() => window.gpuSpike.examples)) cases.push({ name, view: 'xy', shape, size: 128 })
    for (const id of await page.evaluate(() => window.gpuSpike.shapes)) for (const name of ['checker', 'marble', 'malachite']) cases.push({ name, view: 'scene', shape: id, size: 96 })
  } else for (const name of example === 'all' ? await page.evaluate(() => window.gpuSpike.examples) : [example]) {
    for (const view of mode === 'all' ? ['scene', 'xy', 'xz', 'yz'] : [mode]) cases.push({ name, view, shape, size })
  }
  for (const { name, view, shape, size } of cases) {
      const result = await page.evaluate(({ name, view, size, repeats, shape }) => {
        const options = { ...window.gpuSpike.defaultView, shape, mode: view === 'scene' ? 'scene' : 'slice', axis: view === 'scene' ? 'xy' : view, position: 0 }
        return window.gpuSpike.render(name, options, size, repeats)
      }, { name, view, size, repeats, shape })
      const { png, ...measurements } = result
      const file = `${view === 'scene' ? shape + '-' : ''}${name}-${view}-${size}.png`
      writeFileSync(join(out, file), Buffer.from(png.split(',')[1], 'base64'))
      let comparison
      if (bin) {
        const repository = join(root, '..')
        let reference = join(out, `reference-${file}`)
        const golden = join(repository, 'golden', view === 'scene' ? 'scenes' : 'textures', view === 'scene' ? `${shape}-${name}.png` : `${name}.png`)
        if (args.includes('--goldens') && !existsSync(golden)) throw new Error(`Missing golden: ${golden}`)
        if ((view === 'xy' && size === 128 || view === 'scene' && size === 96) && existsSync(golden)) reference = golden
        else execFileSync(join(bin, 'procedural-textures'), ['render', join(repository, 'examples', `${name}.json`), reference, '--size', String(size), ...(view === 'scene' ? ['--shape', shape] : ['--axis', view, '--slice', '0'])], { cwd: repository })
        let report, pass = true
        try { report = execFileSync(join(bin, 'png-compare'), ['--threshold', '0.0001', '--max-threshold', String(maxThreshold), reference, join(out, file)], { encoding: 'utf8' }).trim() }
        catch (error) { report = error.stdout.trim(); pass = false; failures++ }
        assert.match(report, /^mean [\d.]+ max [\d.]+$/)
        comparison = { reference, report, pass, meanThreshold: 0.0001, maxThreshold }
        console.log(`  ${report} (Haskell comparison ${pass ? 'passed' : 'FAILED'})`)
      }
      results.push({ example: name, view, size, file, ...measurements, comparison })
      console.log(`${file}: first ${result.firstMs.toFixed(2)} ms, steady ${result.steadyMs.map((v) => v.toFixed(2)).join(', ')} ms; ${result.compilations} compilations; ${result.renderer}`)
  }
  if (args.includes('--check')) {
    const errors = []
    page.on('pageerror', (error) => errors.push(String(error)))
    await page.goto(`http://127.0.0.1:${address.port}/spike.html`)
    await page.waitForFunction(() => document.querySelector('#status').textContent.startsWith('Submitted'))
    await page.select('#material', 'marble')
    await page.waitForFunction(() => document.querySelector('#status').textContent.includes('2 program compilations'))
    await page.select('#view', 'xz')
    await page.$eval('#position', (input) => { input.value = '0.5'; input.dispatchEvent(new Event('input', { bubbles: true })) })
    await page.waitForFunction(() => document.querySelector('#status').textContent.includes('3 program compilations'))
    await page.select('#view', 'scene')
    await page.waitForFunction(() => document.querySelector('#status').textContent.startsWith('Submitted'))
    await page.screenshot({ path: join(out, 'spike-preview.png') })
    assert.deepEqual(errors, [])
    console.log('Standalone preview controls passed')
  }
  writeFileSync(join(out, args.includes('--goldens') ? 'goldens.json' : `measurements-${size}${args.includes('--check') ? '-checks' : ''}.json`), JSON.stringify({ browser: await browser.version(), results }, null, 2) + '\n')
  assert.equal(failures, 0, `${failures} image comparisons failed`)
} finally {
  await session?.close()
}
