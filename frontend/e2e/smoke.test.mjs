// End-to-end smoke tests: the real editor in a real browser, against a
// static production build beneath a repository prefix. Slow, so not part of
// `npm test`; run with `make e2e`, which starts only a Node static server.
//
// Environment: E2E_URL (default http://localhost:8095/) and CHROME (path
// to a Chrome or Chromium binary; defaults suit macOS and most Linux).

import assert from 'node:assert/strict'
import { existsSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { after, afterEach, before, beforeEach, describe, it } from 'node:test'
import puppeteer, { PUPPETEER_REVISIONS } from 'puppeteer-core'
import { browserArgs, checkBackend, softwareBackend } from '../scripts/browser-launch.mjs'

const url = process.env.E2E_URL ?? 'http://localhost:8095/'
const chrome =
  process.env.CHROME ??
  ['/Applications/Google Chrome.app/Contents/MacOS/Google Chrome', '/usr/bin/google-chrome', '/usr/bin/chromium'].find(existsSync)

const wait = (ms) => new Promise((resolve) => setTimeout(resolve, ms))

let browser
let page
let context
let errors
let apiRequests

before(async () => {
  assert.ok(chrome, 'Set CHROME to a Chrome or Chromium binary')
  browser = await puppeteer.launch({
    executablePath: chrome,
    headless: true,
    defaultViewport: { width: 1400, height: 900 },
    // GitHub's Ubuntu runners don't let Chrome set up its sandbox (AppArmor
    // restricts unprivileged user namespaces), so CI runs without it.
    args: browserArgs(),
    protocolTimeout: 120000,
  })
  if (process.env.CI) assert.equal(await browser.version(), `Chrome/${PUPPETEER_REVISIONS.chrome}`, 'CI uses pinned Chrome')
})

after(async () => {
  await browser?.close()
})

beforeEach(async () => {
  context = await browser.createBrowserContext()
  page = await context.newPage()
  errors = []
  apiRequests = []
  page.on('request', (r) => { if (new URL(r.url()).pathname.includes('/api/')) apiRequests.push(r.url()) })
  await page.evaluateOnNewDocument(() => {
    window.renderReads = 0; window.pngEncodes = 0; window.simulationWorkers = 0; window.volumeUploads = 0
    const NativeWorker=window.Worker
    window.Worker=class extends NativeWorker { constructor(...args) {super(...args);window.simulationWorkers++} }
    const upload=WebGL2RenderingContext.prototype.texImage3D
    WebGL2RenderingContext.prototype.texImage3D=function(...args) {window.volumeUploads++;return upload.apply(this,args)}
    const read = WebGL2RenderingContext.prototype.readPixels
    WebGL2RenderingContext.prototype.readPixels = function (...args) { window.renderReads++; return read.apply(this, args) }
    const Compression=window.CompressionStream
    window.CompressionStream=class extends Compression { constructor(...args) { super(...args); window.pngEncodes++ } }
    for (const key of ['toBlob', 'toDataURL']) {
      const encode = HTMLCanvasElement.prototype[key]
      HTMLCanvasElement.prototype[key] = function (...args) { window.pngEncodes++; return encode.apply(this, args) }
    }
  })
  page.on('pageerror', (e) => errors.push(String(e)))
  await page.goto(url, { waitUntil: 'domcontentloaded' })
  await page.waitForSelector('.preview-image canvas[data-rendered="true"]', { timeout: 30000 })
  await verifyRendererBackend()
})

// Rendering is queued after examples load; network idle alone is insufficient.
async function verifyRendererBackend() {
  await page.waitForSelector('canvas[data-renderer]', { timeout: 30000 })
  if (!softwareBackend) return
  const backend = await page.evaluate(() => {
    const gl = document.querySelector('canvas[data-renderer]').getContext('webgl2')
    const extension = gl.getExtension('WEBGL_debug_renderer_info')
    return gl.getParameter(extension ? extension.UNMASKED_RENDERER_WEBGL : gl.RENDERER)
  })
  checkBackend(backend)
}

// Close each test's page, so it cannot go on autosaving into the local
// storage the next test starts from.
afterEach(async () => {
  try {
    assert.deepEqual(apiRequests, [], 'the complete editor makes no API requests')
    assert.deepEqual(errors, [], 'no uncaught browser errors')
    const counts = await page.evaluate(() => [window.renderReads, window.pngEncodes, window.expectedExports ?? 0])
    assert.deepEqual(counts.slice(0, 2), [counts[2], counts[2]], 'readback and PNG encoding are reserved for explicit export')
  } finally { await context?.close() }
})

const text = (selector) => page.$eval(selector, (n) => n.textContent.trim())
const value = (selector) => page.$eval(selector, (n) => n.value)
const libraryCount = () =>
  page.evaluate(() => Object.keys(JSON.parse(localStorage.getItem('procedural-textures.library.v1') ?? '{}')).length)

async function openExample(name) {
  await page.click('.topbar .button')
  await page.waitForSelector('.document-card')
  for (const card of await page.$$('.document-card')) {
    if ((await card.evaluate((n) => n.textContent.trim())) === name) {
      await card.click()
      await wait(300)
      return
    }
  }
  throw new Error(`No example called ${name}`)
}

async function clickText(selector, label) {
  for (const element of await page.$$(selector)) {
    if ((await element.evaluate((n) => n.textContent.trim())).startsWith(label)) {
      await element.click()
      return
    }
  }
  throw new Error(`No ${selector} starting "${label}"`)
}

async function drag(handle, dx, dy) {
  const box = await handle.boundingBox()
  const x = box.x + box.width / 2
  const y = box.y + box.height / 2
  await page.mouse.move(x, y)
  await page.mouse.down()
  for (let i = 1; i <= 8; i++) await page.mouse.move(x + (dx * i) / 8, y + (dy * i) / 8)
  await page.mouse.up()
  await wait(300)
}

describe('editor', () => {
  it('opens the first example, read-only until edited', async () => {
    assert.equal(await value('.document-name'), 'Agate')
    assert.equal(await text('.save-status'), 'Example')
    assert.ok(await page.$('.preview-image canvas[data-rendered]'), 'preview rendered')
    assert.deepEqual(errors, [])
  })

  it('precomputes reaction volumes off the UI thread and reuses them for view and concentration changes',async()=>{
    await openExample('Chemical colony')
    await page.waitForFunction(()=>window.volumeUploads>0 && !document.querySelector('.simulation-status'),{timeout:60000})
    const counts=await page.evaluate(()=>({workers:window.simulationWorkers,uploads:window.volumeUploads}))
    await page.select('[aria-label="View"]','slice');await wait(400)
    await clickText('.child-link','Field');await clickText('.child-link','Source')
    await page.select('[aria-label="Concentration"]','u');await wait(400)
    assert.deepEqual(await page.evaluate(()=>({workers:window.simulationWorkers,uploads:window.volumeUploads})),counts)
    await page.$eval('input[aria-label="Seed"]',(n)=>{n.value='99';n.dispatchEvent(new Event('input',{bubbles:true}))})
    await page.waitForFunction((count)=>window.volumeUploads>count && !document.querySelector('.simulation-status'),{timeout:60000},counts.uploads)
    assert.ok(await page.evaluate((count)=>window.simulationWorkers>count,counts.workers))
    const workers=await page.evaluate(()=>window.simulationWorkers)
    await clickText('.topbar button','Undo');await wait(400)
    assert.equal(await page.evaluate(()=>window.simulationWorkers),workers,'undo reuses the previous CPU volume')
    const uploads=await page.evaluate(()=>window.volumeUploads)
    await page.$eval('canvas[data-renderer]',(c)=>{window.lostContext=c.getContext('webgl2').getExtension('WEBGL_lose_context');window.lostContext.loseContext()})
    await page.waitForSelector('.preview-error')
    await page.evaluate(()=>window.lostContext.restoreContext())
    await page.waitForFunction((old)=>window.volumeUploads>old && !document.querySelector('.preview-error'),{},uploads)
    assert.equal(await page.evaluate(()=>window.simulationWorkers),workers,'restoration uploads the retained CPU volume')
    assert.deepEqual(errors,[])
  })

  it('edits and inspects a scalar field with undo and field PNG export', async () => {
    await openExample('Gated alpine ridges')
    await clickText('.child-link','Field'); await clickText('.child-link','B'); await clickText('.child-link','A')
    assert.equal(await value('[aria-label="Texture type"]'),'constant')
    const choices=await page.$$eval('[aria-label="Texture type"] option',(nodes)=>nodes.map((n)=>n.value))
    assert.ok(choices.includes('noise')); assert.ok(!choices.includes('flat')); assert.ok(!choices.includes('translate'))
    await page.select('[aria-label="View"]','slice'); await page.click('[aria-label="Inspect selected field"]'); await wait(350)
    const before=await page.$eval('.preview-image canvas',(c)=>Number(c.dataset.frame))
    await page.$eval('input[aria-label="Value"]',(input)=>{ input.value='0.12'; input.dispatchEvent(new Event('input',{bubbles:true})) })
    await page.waitForFunction((before)=>Number(document.querySelector('.preview-image canvas').dataset.frame)>before,{},before)
    assert.equal(await page.$eval('.preview-image canvas',(c)=>Number(c.dataset.programCompileMs)),0)
    await clickText('.topbar button','Undo'); assert.equal(await value('input[aria-label="Value"]'),'0.18')
    await clickText('.topbar button','Redo'); assert.equal(await value('input[aria-label="Value"]'),'0.12')
    await page.evaluate(()=>{
      window.expectedExports=1
      const click=HTMLAnchorElement.prototype.click
      HTMLAnchorElement.prototype.click=function(){
        if(!this.download.endsWith('.png')) return click.call(this)
        const name=this.download
        fetch(this.href).then((r)=>r.blob()).then(createImageBitmap).then((image)=>{
          const c=document.createElement('canvas'); c.width=image.width; c.height=image.height
          const ctx=c.getContext('2d'); ctx.drawImage(image,0,0)
          window.fieldExport={name,width:image.width,corner:[...ctx.getImageData(0,0,1,1).data],centre:[...ctx.getImageData(128,128,1,1).data]}; image.close()
        })
      }
    })
    await page.select('[aria-label="PNG resolution"]','256'); await clickText('.viewer-export button','Download PNG')
    await page.waitForFunction(()=>window.fieldExport)
    const exported=await page.evaluate(()=>window.fieldExport)
    assert.match(exported.name,/-field-/); assert.equal(exported.width,256); assert.deepEqual(exported.corner,exported.centre)
    assert.equal(exported.corner[0],exported.corner[1]); assert.equal(exported.corner[1],exported.corner[2]); assert.equal(exported.corner[3],255)
    await page.waitForFunction(()=>document.querySelector('.save-status')?.textContent==='Saved')
    const stored=await page.evaluate(()=>Object.values(JSON.parse(localStorage.getItem('procedural-textures.library.v1')))[0].document)
    assert.equal(stored.version,5); assert.equal(stored.texture.field.b.a.value,0.12)
    await page.click('[aria-label="Inspect selected field"]')
    await openExample('Cloud silhouette — scalar union')
    await clickText('.structure-actions button','Swap')
    await page.waitForFunction(()=>document.querySelector('.save-status')?.textContent==='Saved')
    const mixed=await page.evaluate(()=>Object.values(JSON.parse(localStorage.getItem('procedural-textures.library.v1'))).find((e)=>e.document.name.startsWith('Cloud silhouette — scalar union')).document.texture)
    assert.equal(mixed.a.type,'flat'); assert.equal(mixed.b.type,'colourise'); assert.equal(mixed.mask.type,'threshold')
    assert.deepEqual(errors,[])
  })

  it('offers typed domain composition and generic fractal source editing', async () => {
    await openExample('Diagonal inlay'); await clickText('.child-link','Coordinates')
    assert.equal(await value('[aria-label="Texture type"]'),'compose'); await clickText('.child-link','First')
    const choices=await page.$$eval('[aria-label="Texture type"] option',(nodes)=>nodes.map((n)=>n.value))
    assert.ok(choices.includes('warp')); assert.ok(choices.includes('compose')); assert.ok(!choices.includes('noise')); assert.ok(!choices.includes('flat'))
    await page.select('[aria-label="Wrap in…"]','compose.first'); assert.equal(await value('[aria-label="Texture type"]'),'compose')
    await clickText('.topbar button','Undo'); assert.equal(await value('[aria-label="Texture type"]'),'translate')
    await openExample('Nested fractal frost')
    await clickText('.child-link','Field'); await clickText('.child-link','Source'); await clickText('.child-link','Noise source')
    assert.equal(await value('[aria-label="Texture type"]'),'fractal')
    await page.select('[aria-label="Style"]','smooth'); await page.waitForFunction(()=>document.querySelector('.save-status')?.textContent==='Saved')
    await page.click('[aria-label="Inspect selected field"]'); await wait(350)
    assert.ok(await page.$('.preview-image canvas[data-rendered]')); assert.deepEqual(errors,[])
  })

  it('edits SDF joins and cellular modes through the typed inspector', async () => {
    await openExample('SDF union — sharp')
    await clickText('.child-link','Field'); await clickText('.child-link','Source')
    assert.equal(await value('[aria-label="Texture type"]'),'sdf-union')
    await page.$eval('input[aria-label="Smoothing radius"]',(input)=>{ input.value='0.15'; input.dispatchEvent(new Event('input',{bubbles:true})) })
    await page.click('[aria-label="Inspect selected field"]')
    await page.waitForFunction(()=>document.querySelector('.save-status')?.textContent==='Saved')
    await openExample('Euclidean cells — pebbled jade')
    await clickText('.child-link','Field'); await clickText('.child-link','Source')
    assert.equal(await value('[aria-label="Texture type"]'),'worley')
    await page.select('[aria-label="View"]','slice'); await wait(350)
    const before=await page.$eval('.preview-image canvas',(c)=>Number(c.dataset.frame))
    await page.select('[aria-label="Distance metric"]','manhattan')
    await page.select('[aria-label="Output"]','f2')
    await page.waitForFunction((before)=>Number(document.querySelector('.preview-image canvas').dataset.frame)>before,{},before)
    assert.equal(await page.$eval('.preview-image canvas',(c)=>Number(c.dataset.programCompileMs)),0)
    await page.waitForFunction(()=>document.querySelector('.save-status')?.textContent==='Saved')
    const stored=await page.evaluate(()=>Object.values(JSON.parse(localStorage.getItem('procedural-textures.library.v1'))).find((e)=>e.document.name.startsWith('Euclidean cells — pebbled jade')).document.texture.field.source)
    assert.equal(stored.metric,'manhattan'); assert.equal(stored.output,'f2')
    await openExample('Volumetric cell mosaic')
    await clickText('.child-link','Base'); await clickText('.child-link','Vector')
    assert.equal(await value('[aria-label="Texture type"]'),'cell-colour')
    const choices=await page.$$eval('[aria-label="Texture type"] option',(nodes)=>nodes.map((n)=>n.value))
    assert.ok(choices.includes('cell-id')); assert.ok(!choices.includes('cell-value'))
  })

  it('warms nearby library thumbnails and renders distant cards when scrolled into view', async () => {
    await page.click('.topbar .button')
    await page.waitForSelector('.document-card .thumbnail canvas[data-rendered="true"]')
    // Use the last card in the final group, rather than each group's last card.
    const cards = await page.$$('.document-card'), distant = cards.at(-1)
    assert.equal(await distant.$eval('canvas', (c) => c.dataset.rendered), undefined)
    await distant.evaluate((card) => card.scrollIntoView({ block: 'center' }))
    await page.waitForFunction((card) => card.querySelector('canvas').dataset.rendered === 'true', {}, distant)
    assert.equal(await distant.$eval('canvas', (c) => c.title), '')
  })

  it('paints immediate preparation feedback before compiling a structural edit', async () => {
    await openExample('Checkerboard')
    await page.evaluate(() => {
      window.preparationFrames = []
      window.preparationCompile = null
      window.preparationArmed = false
      const sources = new Map()
      const setSource = WebGL2RenderingContext.prototype.shaderSource
      WebGL2RenderingContext.prototype.shaderSource = function (shader, source) {
        sources.set(shader, source)
        return setSource.call(this, shader, source)
      }
      document.querySelector('.inspector select[aria-label="Texture type"]').addEventListener('change', () => {
        window.preparationFrames = []
        window.preparationArmed = true
      }, { once: true })
      const observe = (time) => {
        const indicator = document.querySelector('.preview-busy')
        if (indicator?.classList.contains('is-preparing') && Number(getComputedStyle(indicator).opacity) > 0) window.preparationFrames.push(time)
        requestAnimationFrame(observe)
      }
      requestAnimationFrame(observe)
      const compile = WebGL2RenderingContext.prototype.compileShader
      WebGL2RenderingContext.prototype.compileShader = function (...args) {
        if (window.preparationArmed && !window.preparationCompile && sources.get(args[0])?.includes('if (false)')) window.preparationCompile = { now: performance.now(), frames: [...window.preparationFrames] }
        return compile.apply(this, args)
      }
    })
    await page.select('.inspector select[aria-label="Texture type"]', 'fbm')
    await page.waitForFunction(() => window.preparationCompile)
    const result = await page.evaluate(() => window.preparationCompile)
    assert.ok(result.frames.some((time) => result.now - time >= 8), 'visible feedback had a previous rendering opportunity before compile')
    await page.waitForSelector('.preview-image canvas[data-rendered="true"]')
  })

  it('waits for queued renderer creation after a reload with delayed frames', async () => {
    await page.evaluateOnNewDocument(() => {
      const frame = window.requestAnimationFrame.bind(window)
      window.requestAnimationFrame = (callback) => frame((time) => setTimeout(() => callback(time), 350))
    })
    await page.reload({ waitUntil: 'networkidle0' })
    await verifyRendererBackend()
    await page.waitForSelector('.preview-image canvas[data-rendered="true"]')
  })

  it('saves an edited example as a copy, undoes, and restores after a reload', async () => {
    await openExample('Checkerboard')
    await page.click('button[aria-label="Increase Columns"]')
    await wait(700)
    assert.equal(await value('.inspector .number-input'), '9')
    assert.equal(await text('.save-status'), 'Saved')
    assert.equal(await libraryCount(), 1)

    // Edits to one field less than a second apart merge into one undo step,
    // so leave a gap to make the next click a step of its own.
    await wait(1100)
    await page.click('button[aria-label="Increase Columns"]')
    await page.keyboard.down(process.platform === 'darwin' ? 'Meta' : 'Control')
    await page.keyboard.press('z')
    await page.keyboard.up(process.platform === 'darwin' ? 'Meta' : 'Control')
    await wait(700)
    assert.equal(await value('.inspector .number-input'), '9')

    await page.reload({ waitUntil: 'networkidle0' })
    await verifyRendererBackend()
    assert.equal(await value('.inspector .number-input'), '9')
    assert.equal(await text('.save-status'), 'Saved')
  })

  it('keeps edits that could not be saved, and saves them once storage works again', async () => {
    await openExample('Checkerboard')
    await page.click('button[aria-label="Increase Columns"]')
    await wait(700)
    assert.equal(await text('.save-status'), 'Saved')

    // Make local storage fail, as it does when full.
    await page.evaluate(() => {
      window.__setItem = Storage.prototype.setItem
      Storage.prototype.setItem = () => {
        throw new DOMException('Storage is full', 'QuotaExceededError')
      }
    })
    await wait(1100)
    await page.click('button[aria-label="Increase Columns"]')
    await wait(700)
    assert.equal(await text('.save-status'), 'Not saved')

    // Switching documents asks before discarding the unsaved edit; decline.
    let asked = false
    page.once('dialog', (dialog) => {
      asked = true
      void dialog.dismiss()
    })
    await openExample('Linear gradient').catch(() => {})
    await page.keyboard.press('Escape')
    assert.ok(asked, 'asked before discarding')
    assert.equal(await value('.document-name'), 'Checkerboard')
    assert.equal(await value('.inspector .number-input'), '10')

    // Once storage works again, the retry saves the edit.
    await page.evaluate(() => {
      Storage.prototype.setItem = window.__setItem
    })
    await page.waitForFunction(() => document.querySelector('.save-status')?.textContent === 'Saved', { timeout: 10000 })
    const columns = await page.evaluate(
      () => Object.values(JSON.parse(localStorage.getItem('procedural-textures.library.v1')))[0].document.texture.columns,
    )
    assert.equal(columns, 10)
  })

  it('reopens the current card without losing pending edits or undo history', async () => {
    await openExample('Checkerboard')
    await page.click('button[aria-label="Increase Columns"]')
    await page.waitForFunction(() => document.querySelector('.save-status')?.textContent === 'Saved')
    await wait(1100) // Make the next edit a separate undo step.
    // Hold autosave so this always exercises the stale card, even on slow CI.
    await page.evaluate(() => {
      window.__setTimeout = window.setTimeout
      window.setTimeout = (handler, ms, ...args) => window.__setTimeout(handler, ms === 400 ? 60000 : ms, ...args)
    })
    await page.click('button[aria-label="Increase Columns"]')
    await page.click('.topbar .button')
    await page.waitForSelector('.library-card.is-open .document-card')
    const storedColumns = await page.evaluate(
      () => Object.values(JSON.parse(localStorage.getItem('procedural-textures.library.v1')))[0].document.texture.columns,
    )
    assert.equal(storedColumns, 9)
    await page.click('.library-card.is-open .document-card')
    await page.waitForSelector('.library-card', { hidden: true })
    assert.equal(await value('.inspector .number-input'), '10')
    await page.keyboard.down(process.platform === 'darwin' ? 'Meta' : 'Control')
    await page.keyboard.press('z')
    await page.keyboard.up(process.platform === 'darwin' ? 'Meta' : 'Control')
    await page.waitForFunction(() => document.querySelector('.inspector .number-input')?.value === '9')
    assert.equal(await value('.inspector .number-input'), '9')
    await page.keyboard.down(process.platform === 'darwin' ? 'Meta' : 'Control')
    await page.keyboard.down('Shift')
    await page.keyboard.press('z')
    await page.keyboard.up('Shift')
    await page.keyboard.up(process.platform === 'darwin' ? 'Meta' : 'Control')
    await page.waitForFunction(() => document.querySelector('.inspector .number-input')?.value === '10')
    assert.equal(await value('.inspector .number-input'), '10')
    await page.evaluate(() => {
      window.setTimeout = window.__setTimeout
      window.dispatchEvent(new Event('pagehide'))
    })
    await page.waitForFunction(() => document.querySelector('.save-status')?.textContent === 'Saved')
    await page.reload({ waitUntil: 'networkidle0' })
    await verifyRendererBackend()
    assert.equal(await value('.inspector .number-input'), '10')
    assert.equal(await libraryCount(), 1)
  })

  it('fetches a stored document again instead of opening the card snapshot', async () => {
    await openExample('Checkerboard')
    await page.click('button[aria-label="Increase Columns"]')
    await page.waitForFunction(() => document.querySelector('.save-status')?.textContent === 'Saved')
    await openExample('Linear gradient')
    await page.click('.topbar .button')
    await page.waitForSelector('.library-card .document-card')
    // Another save can update storage while this card holds the older snapshot.
    await page.evaluate(() => {
      const key = 'procedural-textures.library.v1'
      const entries = JSON.parse(localStorage.getItem(key))
      Object.values(entries)[0].document.texture.columns = 11
      localStorage.setItem(key, JSON.stringify(entries))
    })
    await page.click('.library-card .document-card')
    await page.waitForSelector('.library-card', { hidden: true })
    assert.equal(await value('.inspector .number-input'), '11')
  })

  it('keeps one example copy when only the working-state write fails repeatedly', async () => {
    await openExample('Checkerboard')
    await page.evaluate(() => {
      window.__setItem = Storage.prototype.setItem
      window.__workingFailures = 0
      Storage.prototype.setItem = function (key, value) {
        if (key === 'procedural-textures.working.v1') {
          window.__workingFailures++
          throw new DOMException('Storage is full', 'QuotaExceededError')
        }
        return window.__setItem.call(this, key, value)
      }
    })
    await page.click('button[aria-label="Increase Columns"]')
    await page.waitForFunction(() => document.querySelector('.save-status')?.textContent === 'Not saved')
    const firstId = await page.evaluate(() => Object.keys(JSON.parse(localStorage.getItem('procedural-textures.library.v1')))[0])
    // Flush retries through the same lifecycle path, without waiting for timers.
    await page.evaluate(() => {
      window.dispatchEvent(new Event('pagehide'))
      window.dispatchEvent(new Event('pagehide'))
    })
    assert.ok(await page.evaluate(() => window.__workingFailures >= 3))
    assert.equal(await libraryCount(), 1)
    await page.click('button[aria-label="Increase Columns"]')
    await page.evaluate(() => window.dispatchEvent(new Event('pagehide')))
    assert.equal(await text('.save-status'), 'Not saved')
    // Undo back to the original object must still settle the partially written
    // library record, whether the two edits coalesced or CI separated them.
    for (let i = 0; i < 2 && (await value('.inspector .number-input')) !== '8'; i++) {
      const before = await value('.inspector .number-input')
      await page.keyboard.down(process.platform === 'darwin' ? 'Meta' : 'Control')
      await page.keyboard.press('z')
      await page.keyboard.up(process.platform === 'darwin' ? 'Meta' : 'Control')
      await page.waitForFunction((previous) => document.querySelector('.inspector .number-input')?.value !== previous, {}, before)
    }
    assert.equal(await value('.inspector .number-input'), '8')
    await page.evaluate(() => window.dispatchEvent(new Event('pagehide')))
    const revertedColumns = await page.evaluate(
      () => Object.values(JSON.parse(localStorage.getItem('procedural-textures.library.v1')))[0].document.texture.columns,
    )
    assert.equal(revertedColumns, 8)
    assert.equal(await text('.save-status'), 'Not saved')
    for (let i = 0; i < 2 && (await value('.inspector .number-input')) !== '10'; i++) {
      const before = await value('.inspector .number-input')
      await page.keyboard.down(process.platform === 'darwin' ? 'Meta' : 'Control')
      await page.keyboard.down('Shift')
      await page.keyboard.press('z')
      await page.keyboard.up('Shift')
      await page.keyboard.up(process.platform === 'darwin' ? 'Meta' : 'Control')
      await page.waitForFunction((previous) => document.querySelector('.inspector .number-input')?.value !== previous, {}, before)
    }
    assert.equal(await value('.inspector .number-input'), '10')
    await page.evaluate(() => {
      Storage.prototype.setItem = window.__setItem
      window.dispatchEvent(new Event('pagehide'))
    })
    await page.waitForFunction(() => document.querySelector('.save-status')?.textContent === 'Saved')
    assert.equal(await libraryCount(), 1)
    const working = await page.evaluate(() => JSON.parse(localStorage.getItem('procedural-textures.working.v1')))
    assert.equal(working.source.id, firstId)
    assert.equal(working.document.texture.columns, 10)
    await page.reload({ waitUntil: 'networkidle0' })
    await verifyRendererBackend()
    assert.equal(await value('.inspector .number-input'), '10')
    assert.equal(await libraryCount(), 1)
  })

  it('shows the undone value in a field that still has focus', async () => {
    await openExample('Checkerboard')
    const columns = await page.$('.inspector .number-input')
    await columns.click()
    await page.keyboard.press('End')
    await page.keyboard.press('Backspace')
    await page.keyboard.type('9')
    await wait(300)
    await page.keyboard.down(process.platform === 'darwin' ? 'Meta' : 'Control')
    await page.keyboard.press('z')
    await page.keyboard.up(process.platform === 'darwin' ? 'Meta' : 'Control')
    await wait(300)
    assert.equal(await columns.evaluate((n) => n === document.activeElement), true)
    assert.equal(await value('.inspector .number-input'), '8')
  })

  it('keeps arrow keys within an integer field\'s minimum', async () => {
    await openExample('Checkerboard')
    const columns = await page.$('.inspector .number-input')
    await columns.click()
    await page.keyboard.press('ArrowDown', { delay: 0 })
    await page.keyboard.down('Shift')
    await page.keyboard.press('ArrowDown')
    await page.keyboard.up('Shift')
    await wait(300)
    assert.equal(await value('.inspector .number-input'), '1')
  })

  it('wraps a node and shows it in the tree', async () => {
    await openExample('Checkerboard')
    const before = (await page.$$('.tree-row')).length
    await page.select('.inspector select[aria-label="Wrap in…"]', 'layer.bottom')
    await wait(200)
    assert.equal((await page.$$('.tree-row')).length, before + 2)
    assert.equal(await value('.inspector select[aria-label="Texture type"]'), 'layer')
  })

  it('moves points by dragging handles on the preview', async () => {
    await openExample('Linear gradient')
    await page.select('[aria-label="View"]', 'slice')
    const [, to] = await page.$$('.handle')
    await drag(to, -200, 100)
    const inputs = await page.$$eval('.inspector .number-input', (ns) => ns.map((n) => Number(n.value)))
    assert.ok(inputs[3] < 1, `to.x moved left (${inputs[3]})`)
    assert.ok(inputs[4] > 0.5, `to.y moved down (${inputs[4]})`)
  })

  it('orbits and zooms every supported solid without editing the material', async () => {
    await openExample('Checkerboard')
    await page.waitForFunction(() => document.querySelector('[aria-label="Shape"]').options.length === 13)
    for (const shape of ['sphere', 'cube', 'cylinder', 'torus', 'bitten-cube', 'cut-sphere', 'cut-cube', 'pawn', 'rook', 'knight', 'bishop', 'queen', 'king']) {
      await page.select('[aria-label="Shape"]', shape)
      await wait(250)
      assert.equal(await page.$eval('[aria-label="Shape"]', (n) => n.value), shape)
    }
    await wait(400)
    const before = await page.$eval('.preview canvas', (n) => n.dataset.frame)
    const compilations = await page.$eval('canvas[data-renderer]', (n) => n.dataset.compilations)
    const camera = await page.$('[aria-label="3D camera controls"]')
    await camera.focus()
    await page.keyboard.press('ArrowRight')
    await page.keyboard.press('+')
    await page.waitForFunction((old) => document.querySelector('.preview canvas')?.dataset.frame !== old, {}, before)
    await wait(400)
    assert.equal(await page.$eval('canvas[data-renderer]', (n) => n.dataset.compilations), compilations, 'camera edits reuse the shader')
    assert.equal(await text('.save-status'), 'Example')
    assert.deepEqual(errors, [])
  })

  it('uses budget-selected resolution during an orbit and refines after release', async () => {
    await openExample('Linear gradient')
    await wait(700)
    const fullSize = await page.$eval('.preview canvas', (n) => n.width)
    await page.waitForFunction(() => document.querySelector('.preview canvas').dataset.budgetMs === '15')
    const before = await page.$eval('.preview canvas', (n) => Number(n.dataset.frame))
    await page.evaluate(() => {
      window.previewSizes = []
      new MutationObserver(() => window.previewSizes.push(document.querySelector('.preview canvas').width))
        .observe(document.querySelector('.preview canvas'), { attributes: true, attributeFilter: ['data-frame'] })
    })
    const box = await (await page.$('.viewer-surface')).boundingBox()
    await page.mouse.move(box.x + box.width * 0.5, box.y + box.height * 0.5)
    await page.mouse.down()
    await page.mouse.move(box.x + box.width * 0.65, box.y + box.height * 0.55, { steps: 8 })
    await wait(450)
    assert.ok(await page.$eval('.preview canvas', (n) => Number(n.dataset.frame)) > before, 'interactive renders started')
    assert.ok((await page.evaluate(() => window.previewSizes)).every((size) => size >= 64 && size <= fullSize), 'adaptive sizes stay within viewer bounds')
    await page.mouse.up()
    await page.waitForFunction(() => !document.querySelector('.preview-busy').classList.contains('is-busy'))
    assert.equal(await page.$eval('.preview canvas', (n) => n.width), fullSize, 'release renders full resolution')
  })

  it('changes slice depth/orientation and preserves depth when dragging XY points', async () => {
    await openExample('Linear gradient')
    await page.select('[aria-label="View"]', 'slice')
    const z = await page.$('.number-input[aria-label="To z"]')
    await z.click({ clickCount: 3 }); await z.type('0.6')
    const [, to] = await page.$$('.handle')
    await drag(to, -100, 40)
    assert.equal(Number(await page.$eval('.number-input[aria-label="To z"]', (n) => n.value)), 0.6)
    await page.select('[aria-label="Slice plane"]', 'xz')
    await page.$eval('[aria-label="Slice position"]', (n) => { n.value = '0.4'; n.dispatchEvent(new Event('input', { bubbles: true })) })
    await wait(300)
    assert.equal(await page.$eval('.slice-position output', (n) => n.textContent), '0.400')
    assert.deepEqual(errors, [])
  })

  it('moves ramp stops by dragging markers', async () => {
    await openExample('Linear gradient')
    const [first] = await page.$$('.ramp-marker')
    await drag(first, 60, 0)
    const position = Number(await value('.stops-list .number-input'))
    assert.ok(position > 0.1, `stop moved right (${position})`)
  })

  it('uses library ramps, and carries saved ramps between textures', async () => {
    await openExample('Linear gradient')
    await clickText('.ramp-actions .button', 'Choose')
    await page.waitForSelector('.ramp-choice')
    await clickText('.ramp-choice', 'Sunset')
    await wait(300)
    assert.match(await text('.ramp-origin'), /Library ramp Sunset/)

    await clickText('.ramp-actions .button', 'Save to My ramps')
    await page.type('.name-form input', ' saved')
    await clickText('.name-form .button', 'Save')
    await clickText('.ramp-actions .button', 'Customise')
    await wait(200)
    assert.equal(await text('.ramp-origin'), 'Custom ramp')

    await openExample('Spherical rings')
    await clickText('.ramp-actions .button', 'Choose')
    await page.waitForSelector('.ramp-choice')
    await clickText('.ramp-choice', 'Sunset saved')
    await wait(800)
    assert.match(await text('.ramp-origin'), /Shared ramp Sunset saved/)
    assert.ok(await page.$('.preview-image canvas[data-rendered]'))
    assert.deepEqual(errors, [])
  })

  it('exports the chosen slice and solid at the chosen resolution, preserving alpha', async () => {
    const path = join(tmpdir(), 'procedural-textures-transparent.json')
    writeFileSync(path, JSON.stringify({ version: 3, name: 'Transparent', texture: { type: 'flat', colour: '#ff000080' } }))
    await (await page.$('input[type=file]')).uploadFile(path)
    await page.waitForFunction(() => document.querySelector('.preview canvas')?.getAttribute('aria-label') === 'Transparent')
    await page.evaluate(() => {
      window.expectedExports = 2
      const click = HTMLAnchorElement.prototype.click
      HTMLAnchorElement.prototype.click = function () {
        if (!this.download.endsWith('.png')) return click.call(this)
        const name = this.download
        window.exported = null
        fetch(this.href).then((r) => r.blob()).then(createImageBitmap).then((image) => {
          const c = document.createElement('canvas'); c.width = image.width; c.height = image.height
          const ctx = c.getContext('2d'); ctx.drawImage(image, 0, 0)
          window.exported = { name, width: image.width, height: image.height, corner: [...ctx.getImageData(0, 0, 1, 1).data] }
          image.close()
        })
      }
    })
    await page.select('[aria-label="View"]', 'slice')
    await page.select('[aria-label="Slice plane"]', 'xz')
    await page.select('[aria-label="PNG resolution"]', '256')
    await clickText('.viewer-export button', 'Download PNG')
    await page.waitForFunction(() => window.exported)
    const slice = await page.evaluate(() => window.exported)
    assert.equal(slice.width, 256); assert.equal(slice.height, 256)
    assert.deepEqual(slice.corner, [255, 0, 0, 128])
    assert.match(slice.name, /Transparent-xz-0.000-256.png/)
    await page.select('[aria-label="View"]', 'scene')
    await page.select('[aria-label="Shape"]', 'knight')
    await page.select('[aria-label="PNG resolution"]', '512')
    await clickText('.viewer-export button', 'Download PNG')
    await page.waitForFunction(() => window.exported?.width === 512)
    const solid = await page.evaluate(() => window.exported)
    assert.equal(solid.height, 512); assert.equal(solid.corner[3], 255)
    assert.match(solid.name, /Transparent-knight-512.png/)
  })

  it('keeps shaders resident during numeric edits, including thumbnail updates', async () => {
    await openExample('Linear gradient'); await wait(700)
    const before = await page.$eval('canvas[data-renderer]', (n) => n.dataset.compilations)
    await page.$eval('.number-input[aria-label="To x"]', (n) => { n.value = '0.75'; n.dispatchEvent(new Event('input', { bubbles: true })) })
    await wait(700)
    assert.equal(await page.$eval('canvas[data-renderer]', (n) => n.dataset.compilations), before)
  })

  it('restores a lost WebGL context and renders the latest material state', async () => {
    await openExample('Linear gradient')
    await page.select('[aria-label="View"]', 'slice')
    await wait(400)
    await page.waitForFunction(() => document.querySelector('.preview canvas')?.dataset.rendered)
    await page.evaluate(() => {
      window.lostContext = document.querySelector('canvas[data-renderer]').getContext('webgl2').getExtension('WEBGL_lose_context')
      window.lostContext.loseContext()
    })
    await page.waitForSelector('.preview-error')
    await page.$eval('.number-input[aria-label="To x"]', (n) => { n.value = '0.75'; n.dispatchEvent(new Event('input', { bubbles: true })) })
    await page.waitForFunction(() => document.querySelector('.number-input[aria-label="To x"]').value === '0.75')
    await page.evaluate(() => window.lostContext.restoreContext())
    await page.waitForFunction(() => !document.querySelector('.preview-error') && !document.querySelector('.preview-busy').classList.contains('is-busy'))
    assert.equal(Number(await value('.number-input[aria-label="To x"]')), 0.75)
    const pixel = await page.$eval('.preview canvas', (c) => [...c.getContext('2d').getImageData(c.width - 2, c.height / 2, 1, 1).data])
    assert.ok(pixel[0] < 5 && pixel[2] > 250, 'restored canvas reflects the new ramp endpoint')
  })

  it('keeps the complete editor usable at a phone-sized viewport', async () => {
    await page.setViewport({ width: 390, height: 844, deviceScaleFactor: 2, isMobile: true, hasTouch: true })
    await page.reload({ waitUntil: 'networkidle0' })
    await verifyRendererBackend()
    await page.waitForSelector('.preview canvas[data-rendered]')
    assert.ok(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), 'no horizontal overflow')
    assert.ok(await page.$eval('.preview', (n) => n.getBoundingClientRect().width) > 300, 'viewer has useful width')
    await openExample('Checkerboard')
    await page.click('button[aria-label="Increase Columns"]')
    await wait(700)
    assert.equal(await value('.inspector .number-input'), '9')
    assert.equal(await text('.save-status'), 'Saved')
  })

  it('explains unavailable WebGL2 while leaving documents editable and exportable', async () => {
    await page.evaluateOnNewDocument(() => {
      const getContext = HTMLCanvasElement.prototype.getContext
      HTMLCanvasElement.prototype.getContext = function (kind, ...args) { return kind === 'webgl2' ? null : getContext.call(this, kind, ...args) }
    })
    await page.reload({ waitUntil: 'networkidle0' })
    await page.waitForSelector('.preview-error')
    assert.match(await text('.preview-error'), /WebGL2 is unavailable/)
    await page.type('.document-name', ' edited')
    await wait(700)
    assert.equal(await text('.save-status'), 'Saved')
    assert.ok(await page.$('.topbar button'), 'JSON/library controls remain available')
  })

  it('links the editor and gallery beneath the repository prefix', { skip: !process.env.E2E_PAGES }, async () => {
    await page.click('a[href="./gallery/index.html"]')
    await page.waitForSelector('a[href="../index.html"]')
    assert.ok(page.url().includes('/procedural-textures/gallery/index.html'))
    await page.click('a[href="../index.html"]')
    await verifyRendererBackend()
    await page.waitForSelector('.preview canvas[data-rendered]')
    await page.reload({ waitUntil: 'networkidle0' })
    await verifyRendererBackend()
    assert.ok(await page.$('.document-name'))
    await page.goto(new URL('gallery.html', url).href)
    await page.waitForFunction(() => location.pathname.endsWith('/gallery/gallery.html'))
    await page.click('a[href="../index.html"]')
    await verifyRendererBackend()
    await page.waitForSelector('.preview canvas[data-rendered]')
  })

  it('explains why an import was rejected', async () => {
    const path = join(tmpdir(), 'procedural-textures-bad.json')
    writeFileSync(path, JSON.stringify({ version: 1, name: 'bad', texture: { type: 'wobble' } }))
    const input = await page.$('input[type=file]')
    await input.uploadFile(path)
    await page.waitForSelector('.toast')
    assert.match(await text('.toast'), /Unknown texture type "wobble"/)
    assert.equal(await libraryCount(), 0)
  })
})
