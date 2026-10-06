// Real Chrome/Firefox/Safari: the same self-running page posts evidence back.
import { createServer } from 'vite'
import puppeteer, { PUPPETEER_REVISIONS } from 'puppeteer-core'
import { Browser, computeExecutablePath, install } from '@puppeteer/browsers'
import { execFileSync } from 'node:child_process'
import { mkdirSync, writeFileSync, existsSync } from 'node:fs'
import { join, basename } from 'node:path'
import { randomUUID } from 'node:crypto'
import { root } from './gpu-session.mjs'
const browserName = process.argv[2] ?? 'chrome', token = randomUUID(), uiOnly = process.argv.includes('--ui-only')
if (!['chrome', 'firefox', 'safari'].includes(browserName)) throw new Error('Expected chrome, firefox or safari')
let receive
const report = new Promise((resolve) => { receive = resolve })
const server = await createServer({ root, server: { host: '127.0.0.1', port: 0, hmr: false }, plugins: [{ name: 'compatibility-results', configureServer(server) {
  server.middlewares.use(async (req, res, next) => {
    if (req.url?.startsWith('/compatibility.html')) {
      res.setHeader('Content-Type', 'text/html'); res.end(`<!doctype html><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><style>canvas{width:128px;height:128px}body{font:16px system-ui}</style><canvas id="preview" width="128" height="128"></canvas><pre id="status">Starting…</pre><select id="material"></select><select id="shape"></select><select id="view"><option>scene</option></select><select id="size"><option>128</option></select><input id="position" value="0"><script type="module">${uiOnly ? "import '/src/gpu/editor-compatibility.ts';" : "import '/src/gpu/spike.ts'; import '/src/gpu/compatibility.ts';"}</script>`); return
    }
    if (req.url === `/__compatibility/${token}` && req.method === 'POST') {
      let body = ''; for await (const chunk of req) { body += chunk; if (body.length > 30000000) { res.writeHead(413); res.end(); return } }
      try { receive(JSON.parse(body)); res.end('OK') } catch { res.writeHead(400); res.end() } return
    }
    next()
  })
} }] })
let browser, page, timeout
try {
  await server.listen()
  const url = `http://127.0.0.1:${server.httpServer.address().port}/compatibility.html?harness=1&token=${token}`
  if (browserName === 'safari') execFileSync('open', ['-a', 'Safari', url])
  else {
    const executable = computeExecutablePath({ cacheDir: join(root, '../out/gpu-browser'), browser: browserName === 'firefox' ? Browser.FIREFOX : Browser.CHROME, buildId: PUPPETEER_REVISIONS[browserName] })
    if (!existsSync(executable)) await install({ cacheDir: join(root, '../out/gpu-browser'), browser: browserName === 'firefox' ? Browser.FIREFOX : Browser.CHROME, buildId: PUPPETEER_REVISIONS[browserName] })
    browser = await puppeteer.launch({ browser: browserName, executablePath: executable, headless: true, protocolTimeout: 600000 })
    page = await browser.newPage(); page.on('pageerror', (e) => console.error(e)); await page.goto(url)
  }
  const result = await Promise.race([report, new Promise((_, reject) => { timeout = setTimeout(() => reject(new Error('Compatibility run timed out')), 600000) })])
  if (result.error) throw new Error(result.error)
  const out = join(root, '../out/compatibility', browserName); mkdirSync(out, { recursive: true })
  if (uiOnly) {
    if (page) {
      await page.setViewport({ width: 390, height: 844 }); await page.goto(new URL('/', url).href)
      await page.waitForSelector('.preview canvas[data-rendered]')
      await page.screenshot({ path: join(out, 'editor-narrow.png'), fullPage: true })
    }
    writeFileSync(join(out, 'editor-report.json'), JSON.stringify(result, null, 2) + '\n')
    console.log(JSON.stringify(result, null, 2));
  } else {
  const bin = join(execFileSync('stack', ['path', '--local-install-root'], { cwd: join(root, '..'), encoding: 'utf8' }).trim(), 'bin')
  const comparisons = []
  for (const image of result.images) {
    if (basename(image.file) !== image.file || !['textures', 'scenes'].includes(image.kind)) throw new Error('Invalid image name')
    const path = join(out, image.file); writeFileSync(path, Buffer.from(image.png.split(',')[1], 'base64'))
    try { comparisons.push({ file: image.file, pass: true, report: execFileSync(join(bin, 'png-compare'), ['--threshold', '0.0001', '--max-threshold', '0.008', join(root, '../golden', image.kind, image.file), path], { encoding: 'utf8' }).trim() }) }
    catch (error) { comparisons.push({ file: image.file, pass: false, report: error.stdout?.trim() ?? String(error) }) }
  }
  delete result.images
  const summary = { ...result, comparisons }; writeFileSync(join(out, 'report.json'), JSON.stringify(summary, null, 2) + '\n')
  console.log(JSON.stringify({ browser: browserName, backend: result.backend, samples: result.samples.length, sampleFailures: result.samples.filter((s) => !s.pass), images: comparisons.length, imageFailures: comparisons.filter((c) => !c.pass), resources: result.resources, afterDispose: result.afterDispose }, null, 2))
  if (result.samples.some((s) => !s.pass) || comparisons.some((c) => !c.pass)) process.exitCode = 1
  }
} finally { clearTimeout(timeout); await browser?.close(); await server.close() }
