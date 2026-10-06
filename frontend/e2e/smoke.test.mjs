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
import { after, afterEach, before, beforeEach, describe, it } from 'node:test'
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
  browser = await puppeteer.launch({
    executablePath: chrome,
    headless: true,
    defaultViewport: { width: 1400, height: 900 },
    // GitHub's Ubuntu runners don't let Chrome set up its sandbox (AppArmor
    // restricts unprivileged user namespaces), so CI runs without it.
    args: process.env.CI ? ['--no-sandbox'] : [],
  })
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

// Close each test's page, so it cannot go on autosaving into the local
// storage the next test starts from.
afterEach(async () => {
  await page?.close()
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
    assert.ok(await page.$('.preview-image img'), 'preview rendered')
    assert.deepEqual(errors, [])
  })

  it('saves an edited example as a copy, undoes, and restores after a reload', async () => {
    await openExample('Checker')
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
    await openExample('Checker')
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

  it('reopens the current card without losing pending edits or undo history', async () => {
    await openExample('Checker')
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
    assert.equal(await value('.inspector .number-input'), '10')
    assert.equal(await libraryCount(), 1)
  })

  it('fetches a stored document again instead of opening the card snapshot', async () => {
    await openExample('Checker')
    await page.click('button[aria-label="Increase Columns"]')
    await page.waitForFunction(() => document.querySelector('.save-status')?.textContent === 'Saved')
    await openExample('Gradient')
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
    await openExample('Checker')
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
    assert.equal(await value('.inspector .number-input'), '10')
    assert.equal(await libraryCount(), 1)
  })

  it('shows the undone value in a field that still has focus', async () => {
    await openExample('Checker')
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
    await openExample('Checker')
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
    await openExample('Checker')
    const before = (await page.$$('.tree-row')).length
    await page.select('.inspector select[aria-label="Wrap in…"]', 'layer.bottom')
    await wait(200)
    assert.equal((await page.$$('.tree-row')).length, before + 2)
    assert.equal(await value('.inspector select[aria-label="Texture type"]'), 'layer')
  })

  it('moves points by dragging handles on the preview', async () => {
    await openExample('Gradient')
    await page.select('[aria-label="View"]', 'slice')
    const [, to] = await page.$$('.handle')
    await drag(to, -200, 100)
    const inputs = await page.$$eval('.inspector .number-input', (ns) => ns.map((n) => Number(n.value)))
    assert.ok(inputs[3] < 1, `to.x moved left (${inputs[3]})`)
    assert.ok(inputs[4] > 0.5, `to.y moved down (${inputs[4]})`)
  })

  it('orbits and zooms every supported solid without editing the material', async () => {
    await openExample('Checker')
    await page.waitForFunction(() => document.querySelector('[aria-label="Shape"]').options.length === 13)
    for (const shape of ['sphere', 'cube', 'cylinder', 'torus', 'bitten-cube', 'cut-sphere', 'cut-cube', 'pawn', 'rook', 'knight', 'bishop', 'queen', 'king']) {
      await page.select('[aria-label="Shape"]', shape)
      await wait(250)
      assert.equal(await page.$eval('[aria-label="Shape"]', (n) => n.value), shape)
    }
    const before = await page.$eval('.preview img', (n) => n.src)
    const camera = await page.$('[aria-label="3D camera controls"]')
    await camera.focus()
    await page.keyboard.press('ArrowRight')
    await page.keyboard.press('+')
    await page.waitForFunction((old) => document.querySelector('.preview img')?.src !== old, {}, before)
    assert.equal(await text('.save-status'), 'Example')
    assert.deepEqual(errors, [])
  })

  it('renders only low resolution during an orbit and refines after release', async () => {
    await openExample('Gradient')
    await wait(700)
    const requests = []
    page.on('request', (r) => { if (r.url().includes('/api/render?')) requests.push(new URL(r.url())) })
    const box = await (await page.$('.viewer-surface')).boundingBox()
    await page.mouse.move(box.x + box.width * 0.5, box.y + box.height * 0.5)
    await page.mouse.down()
    await page.mouse.move(box.x + box.width * 0.65, box.y + box.height * 0.55, { steps: 8 })
    await wait(450)
    assert.ok(requests.length > 0, 'interactive renders started')
    assert.ok(requests.every((q) => Number(q.searchParams.get('size')) <= 96), 'no full renders while dragging')
    await page.mouse.up()
    await page.waitForFunction(() => !document.querySelector('.preview-busy').classList.contains('is-busy'))
    assert.ok(requests.some((q) => Number(q.searchParams.get('size')) > 96), 'release refines')
  })

  it('changes slice depth/orientation and preserves depth when dragging XY points', async () => {
    await openExample('Gradient')
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
    await openExample('Gradient')
    const [first] = await page.$$('.ramp-marker')
    await drag(first, 60, 0)
    const position = Number(await value('.stops-list .number-input'))
    assert.ok(position > 0.1, `stop moved right (${position})`)
  })

  it('uses library ramps, and carries saved ramps between textures', async () => {
    await openExample('Gradient')
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

    await openExample('Rings')
    await clickText('.ramp-actions .button', 'Choose')
    await page.waitForSelector('.ramp-choice')
    await clickText('.ramp-choice', 'Sunset saved')
    await wait(800)
    assert.match(await text('.ramp-origin'), /Shared ramp Sunset saved/)
    assert.ok(await page.$('.preview-image img'))
    assert.deepEqual(errors, [])
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
