// A single browser/context for the complete spike run. No Haskell server.
import assert from 'node:assert/strict'
import { execFileSync } from 'node:child_process'
import { existsSync, mkdirSync, writeFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { join, resolve } from 'node:path'
import { createServer } from 'vite'
import puppeteer from 'puppeteer-core'

const root = fileURLToPath(new URL('..', import.meta.url))
const args = process.argv.slice(2)
const option = (key, fallback) => {
  const index = args.indexOf(key)
  if (index < 0) return fallback
  if (!args[index + 1] || args[index + 1].startsWith('--')) throw new Error(`Missing value for ${key}`)
  return args[index + 1]
}
const example = option('--example', 'all'), size = Number(option('--size', '128'))
const mode = option('--view', 'all'), repeats = Number(option('--repeats', '5'))
if (!['all', 'checker', 'marble', 'cumulus'].includes(example)) throw new Error('Invalid --example')
if (!['all', 'scene', 'xy', 'xz', 'yz'].includes(mode)) throw new Error('Invalid --view')
if (!Number.isInteger(size) || size < 1 || size > 2048) throw new Error('Invalid --size')
if (!Number.isInteger(repeats) || repeats < 0 || repeats > 100) throw new Error('Invalid --repeats')
const maxThreshold = Number(option('--max-threshold', '0.008'))
if (!Number.isFinite(maxThreshold) || maxThreshold < 0) throw new Error('Invalid --max-threshold')
const out = resolve(option('--out', join(root, '..', 'out', 'gpu-spike')))
const chrome = process.env.CHROME ?? ['/Applications/Google Chrome.app/Contents/MacOS/Google Chrome', '/usr/bin/google-chrome', '/usr/bin/chromium'].find(existsSync)
if (!chrome) throw new Error('Set CHROME to a Chrome or Chromium binary')
const server = await createServer({ root, server: { host: '127.0.0.1', port: 0 }, configFile: join(root, 'vite.config.ts') })
let browser
try {
  await server.listen()
  const address = server.httpServer.address()
  browser = await puppeteer.launch({ executablePath: chrome, headless: true, args: process.env.CI ? ['--no-sandbox'] : [] })
  const page = await browser.newPage()
  await page.goto(`http://127.0.0.1:${address.port}/spike.html?harness=1`)
  await page.waitForFunction(() => window.gpuSpike)
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
  const bin = args.includes('--compare') ? join(execFileSync('stack', ['path', '--local-install-root'], { cwd: join(root, '..'), encoding: 'utf8' }).trim(), 'bin') : undefined
  const results = []
  for (const name of example === 'all' ? ['checker', 'marble', 'cumulus'] : [example]) {
    for (const view of mode === 'all' ? ['scene', 'xy', 'xz', 'yz'] : [mode]) {
      const result = await page.evaluate(({ name, view, size, repeats }) => {
        const options = { ...window.gpuSpike.defaultView, mode: view === 'scene' ? 'scene' : 'slice', axis: view === 'scene' ? 'xy' : view, position: 0 }
        return window.gpuSpike.render(name, options, size, repeats)
      }, { name, view, size, repeats })
      const { png, ...measurements } = result
      const file = `${name}-${view}-${size}.png`
      writeFileSync(join(out, file), Buffer.from(png.split(',')[1], 'base64'))
      let comparison
      if (bin) {
        const repository = join(root, '..')
        let reference = join(out, `reference-${file}`)
        const golden = join(repository, 'golden', view === 'scene' ? 'scenes' : 'textures', view === 'scene' ? `bitten-cube-${name}.png` : `${name}.png`)
        if ((view === 'xy' && size === 128 || view === 'scene' && size === 96) && existsSync(golden)) reference = golden
        else execFileSync(join(bin, 'procedural-textures'), ['render', join(repository, 'examples', `${name}.json`), reference, '--size', String(size), ...(view === 'scene' ? ['--shape', 'bitten-cube'] : ['--axis', view, '--slice', '0'])], { cwd: repository })
        const report = execFileSync(join(bin, 'png-compare'), ['--threshold', '0.0001', '--max-threshold', String(maxThreshold), reference, join(out, file)], { encoding: 'utf8' }).trim()
        assert.match(report, /^mean [\d.]+ max [\d.]+$/)
        comparison = { reference, report, meanThreshold: 0.0001, maxThreshold }
        console.log(`  ${report} (Haskell comparison passed)`)
      }
      results.push({ example: name, view, size, file, ...measurements, comparison })
      console.log(`${file}: first ${result.firstMs.toFixed(2)} ms, steady ${result.steadyMs.map((v) => v.toFixed(2)).join(', ')} ms; ${result.compilations} compilations; ${result.renderer}`)
    }
  }
  if (args.includes('--check')) {
    const errors = []
    page.on('pageerror', (error) => errors.push(String(error)))
    await page.goto(`http://127.0.0.1:${address.port}/spike.html`)
    await page.waitForFunction(() => document.querySelector('#status').textContent.startsWith('Submitted'))
    await page.select('#material', 'marble')
    await page.select('#view', 'xz')
    await page.$eval('#position', (input) => { input.value = '0.5'; input.dispatchEvent(new Event('input', { bubbles: true })) })
    await page.waitForFunction(() => document.querySelector('#status').textContent.includes('2 program compilations'))
    await page.select('#view', 'scene')
    await page.waitForFunction(() => document.querySelector('#status').textContent.startsWith('Submitted'))
    await page.screenshot({ path: join(out, 'spike-preview.png') })
    assert.deepEqual(errors, [])
    console.log('Standalone preview controls passed')
  }
  writeFileSync(join(out, `measurements-${size}${args.includes('--check') ? '-checks' : ''}.json`), JSON.stringify({ browser: await browser.version(), results }, null, 2) + '\n')
} finally {
  await browser?.close()
  await server.close()
}
