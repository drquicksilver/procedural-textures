// End-to-end smoke tests: the real editor in a real browser, against a
// running texture-server. Slow, so not part of `npm test`; run with
// `make e2e`, which builds everything and starts a server.
//
// Environment: E2E_URL (default http://localhost:8095/) and CHROME (path
// to a Chrome or Chromium binary; defaults suit macOS and most Linux).

import assert from 'node:assert/strict'
import { existsSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { after, before, beforeEach, describe, it } from 'node:test'
import puppeteer from 'puppeteer-core'

const url = process.env.E2E_URL ?? 'http://localhost:8095/'
const chrome =
  process.env.CHROME ??
  ['/Applications/Google Chrome.app/Contents/MacOS/Google Chrome', '/usr/bin/google-chrome', '/usr/bin/chromium'].find(existsSync)

const wait = (ms) => new Promise((resolve) => setTimeout(resolve, ms))

let browser
let page
let errors

before(async () => {
  assert.ok(chrome, 'Set CHROME to a Chrome or Chromium binary')
  browser = await puppeteer.launch({ executablePath: chrome, headless: true, defaultViewport: { width: 1400, height: 900 } })
})

after(async () => {
  await browser?.close()
})

beforeEach(async () => {
  page = await browser.newPage()
  errors = []
  page.on('pageerror', (e) => errors.push(String(e)))
  await page.goto(url, { waitUntil: 'networkidle0' })
  await page.evaluate(() => localStorage.clear())
  await page.reload({ waitUntil: 'networkidle0' })
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
    assert.equal(await value('.document-name'), 'Checker')
    assert.equal(await text('.save-status'), 'Example')
    assert.ok(await page.$('.preview-image img'), 'preview rendered')
    assert.deepEqual(errors, [])
  })

  it('saves an edited example as a copy, undoes, and restores after a reload', async () => {
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
    assert.equal(await value('.inspector .number-input'), '9')
    assert.equal(await text('.save-status'), 'Saved')
  })

  it('keeps edits that could not be saved, and saves them once storage works again', async () => {
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
    await openExample('Gradient').catch(() => {})
    await page.keyboard.press('Escape')
    assert.ok(asked, 'asked before discarding')
    assert.equal(await value('.document-name'), 'Checker')
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

  it('wraps a node and shows it in the tree', async () => {
    const before = (await page.$$('.tree-row')).length
    await page.select('.inspector select[aria-label="Wrap in…"]', 'layer.bottom')
    await wait(200)
    assert.equal((await page.$$('.tree-row')).length, before + 2)
    assert.equal(await value('.inspector select[aria-label="Texture type"]'), 'layer')
  })

  it('moves points by dragging handles on the preview', async () => {
    await openExample('Gradient')
    const [, to] = await page.$$('.handle')
    await drag(to, -200, 100)
    const inputs = await page.$$eval('.inspector .number-input', (ns) => ns.map((n) => Number(n.value)))
    assert.ok(inputs[2] < 1, `to.x moved left (${inputs[2]})`)
    assert.ok(inputs[3] > 0.5, `to.y moved down (${inputs[3]})`)
  })

  it('moves ramp stops by dragging markers', async () => {
    await openExample('Gradient')
    const [first] = await page.$$('.ramp-marker')
    await drag(first, 60, 0)
    const position = Number(await value('.stops-list .number-input'))
    assert.ok(position > 0.1, `stop moved right (${position})`)
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
