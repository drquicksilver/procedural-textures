// Bounded real-editor responsiveness study. Run after npm run build.
import { createServer } from 'node:http'
import { readFile } from 'node:fs/promises'
import { mkdirSync, writeFileSync } from 'node:fs'
import { join, resolve, extname } from 'node:path'
import puppeteer, { PUPPETEER_REVISIONS } from 'puppeteer-core'
import { Browser, computeExecutablePath } from '@puppeteer/browsers'
import { root } from './paths.mjs'
const name = process.argv[2] ?? 'chrome', label = process.argv[3] ?? 'current'
if (!['chrome', 'firefox'].includes(name) || !/^[a-z0-9-]+$/.test(label)) throw new Error('Usage: cold-start.mjs chrome|firefox LABEL')
const dist = resolve(root, 'dist'), types = { '.html': 'text/html', '.js': 'text/javascript', '.css': 'text/css', '.json': 'application/json' }
const server = createServer(async (request, response) => {
  try {
    const file = resolve(dist, decodeURIComponent(new URL(request.url, 'http://localhost').pathname).slice(1) || 'index.html')
    if (!file.startsWith(`${dist}/`)) throw new Error('Invalid path')
    response.setHeader('Content-Type', types[extname(file)] ?? 'application/octet-stream')
    response.end(await readFile(file))
  } catch { response.writeHead(404); response.end() }
})
await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve))
const executablePath = computeExecutablePath({ cacheDir: join(root, '../out/gpu-browser'), browser: name === 'chrome' ? Browser.CHROME : Browser.FIREFOX, buildId: PUPPETEER_REVISIONS[name] })
let browser
try {
  browser = await puppeteer.launch({ browser: name, executablePath, headless: true, protocolTimeout: 120000, defaultViewport: { width: 1400, height: 900 } })
  const page = await browser.newPage()
  page.on('pageerror', (error) => console.error('Page error:', error))
  await page.evaluateOnNewDocument(() => {
    const probe = window.coldProbe = { active: false, stages: [], frames: [], longTasks: [] }
    const observeFrame = (time) => {
      if (probe.active) { const indicator = document.querySelector('.preview-busy'); probe.frames.push({ time, busy: indicator?.classList.contains('is-busy') ?? false, opacity: indicator ? Number(getComputedStyle(indicator).opacity) : 0 }) }
      requestAnimationFrame(observeFrame)
    }
    requestAnimationFrame(observeFrame)
    new MutationObserver((changes) => {
      if (!probe.active || probe.firstPresentationMs !== null) return
      if (changes.some((c) => c.attributeName === 'data-frame' && c.target.matches('.preview-image canvas'))) {
        probe.firstPresentationMs = performance.now() - probe.start
        probe.firstViewer = { ...document.querySelector('.preview-image canvas').dataset }
      }
    }).observe(document, { subtree: true, attributes: true, attributeFilter: ['data-frame'] })
    if (PerformanceObserver.supportedEntryTypes.includes('longtask')) new PerformanceObserver((list) => {
      if (probe.active) probe.longTasks.push(...list.getEntries().map((e) => ({ start: e.startTime, duration: e.duration })))
    }).observe({ type: 'longtask', buffered: false })
    const wrap = (prototype, method) => {
      const original = prototype[method]
      prototype[method] = function (...args) {
        const start = performance.now()
        const value = original.apply(this, args)
        if (probe.active) probe.stages.push({ method, start, duration: performance.now() - start })
        return value
      }
    }
    for (const method of ['compileShader', 'linkProgram', 'getShaderParameter', 'getProgramParameter', 'drawArrays']) wrap(WebGL2RenderingContext.prototype, method)
    wrap(CanvasRenderingContext2D.prototype, 'drawImage')
  })
  await page.goto(`http://127.0.0.1:${server.address().port}/`)
  await page.waitForSelector('.preview-image canvas[data-rendered]')
  const backend = await page.evaluate(() => {
    const gl = document.querySelector('canvas[data-renderer]').getContext('webgl2'), extension = gl.getExtension('WEBGL_debug_renderer_info')
    return { renderer: gl.getParameter(extension ? extension.UNMASKED_RENDERER_WEBGL : gl.RENDERER), parallelCompilation: !!gl.getExtension('KHR_parallel_shader_compile'), userAgent: navigator.userAgent }
  })
  const begin = async () => page.evaluate(() => {
    Object.assign(window.coldProbe, { active: true, start: performance.now(), stages: [], frames: [], longTasks: [], firstPresentationMs: null, firstViewer: null })
  })
  const end = async (action) => {
    await new Promise((resolve) => setTimeout(resolve, 80))
    return page.evaluate((action) => {
      const p = window.coldProbe; p.active = false
      return { action, elapsedMs: performance.now() - p.start, longTasksSupported: PerformanceObserver.supportedEntryTypes.includes('longtask'), latestViewer: { ...document.querySelector('.preview-image canvas').dataset }, ...p }
    }, action)
  }
  const actions = []
  console.log('Measuring library'); await begin(); await page.click('.topbar .button'); await page.waitForSelector('.document-card'); actions.push(await end('open-library'))
  // Select before deferred thumbnails warm all the example structures.
  for (const material of ['Cumulus', 'Marble']) {
    if (material === 'Marble') { await page.click('.topbar .button'); await page.waitForSelector('.document-card') }
    console.log('Measuring selection', material)
    const frame = await page.$eval('.preview-image canvas', (c) => Number(c.dataset.frame ?? 0))
    await begin()
    await page.evaluate((name) => [...document.querySelectorAll('.document-card')].find((b) => b.textContent.includes(name)).click(), material)
    await page.waitForFunction((before) => Number(document.querySelector('.preview-image canvas')?.dataset.frame ?? 0) > before, {}, frame)
    actions.push(await end(`select-${material.toLowerCase()}`))
  }
  const frame = await page.$eval('.preview-image canvas', (c) => Number(c.dataset.frame))
  await begin()
  await page.select('select[aria-label="Texture type"]', 'fbm')
  await page.waitForFunction((before) => Number(document.querySelector('.preview-image canvas')?.dataset.frame ?? 0) > before, {}, frame)
  actions.push(await end('structural-edit-to-fbm'))
  const out = join(root, '../out/cold-start'); mkdirSync(out, { recursive: true })
  const report = { browser: await browser.version(), backend, label, actions }
  writeFileSync(join(out, `${name}-${label}.json`), JSON.stringify(report, null, 2) + '\n')
  console.log(JSON.stringify({ browser: report.browser, backend, label, actions: actions.map((a) => ({ action: a.action, elapsedMs: a.elapsedMs, longestObservedCallMs: Math.max(0, ...a.stages.map((s) => s.duration)), longestLongTaskMs: a.longTasksSupported ? Math.max(0, ...a.longTasks.map((t) => t.duration)) : null })) }, null, 2))
} catch (error) { console.error('Cold-start probe failed:', error); throw error } finally { await browser?.close(); server.closeAllConnections(); await new Promise((resolve) => server.close(resolve)) }
