// Shared headless session for image, sample and watch commands.
import { existsSync } from 'node:fs'
import { join } from 'node:path'
import { createServer } from 'vite'
import puppeteer, { PUPPETEER_REVISIONS } from 'puppeteer-core'
import { Browser, computeExecutablePath } from '@puppeteer/browsers'
import { browserArgs, checkBackend } from './browser-launch.mjs'
import { root } from './paths.mjs'
export { root } from './paths.mjs'
export async function gpuSession() {
  const pinned = computeExecutablePath({ cacheDir: join(root, '../out/gpu-browser'), browser: Browser.CHROME, buildId: PUPPETEER_REVISIONS.chrome })
  const chrome = process.env.CHROME ?? [pinned, '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome', '/usr/bin/google-chrome', '/usr/bin/chromium'].find(existsSync)
  if (!chrome) throw new Error('Run npm run gpu:browser or set CHROME')
  const args = browserArgs()
  const server = await createServer({ root, configFile: join(root, 'vite.config.ts'), server: { host: '127.0.0.1', port: 0, hmr: false } })
  let browser
  try {
    server.watcher.add([join(root, '../test-vectors'), join(root, '../examples'), join(root, '../ramps')])
    await server.listen()
    browser = await puppeteer.launch({ executablePath: chrome, headless: true, protocolTimeout: 600000, args })
    const page = await browser.newPage()
    const url = `http://127.0.0.1:${server.httpServer.address().port}/spike.html?harness=1`
    const reload = async () => { await page.goto(url); await page.waitForFunction(() => window.gpuSpike) }
    await reload()
    const backend = await page.evaluate(() => {
      const gl = document.querySelector('canvas').getContext('webgl2'), ext = gl.getExtension('WEBGL_debug_renderer_info')
      return ext ? gl.getParameter(ext.UNMASKED_RENDERER_WEBGL) : gl.getParameter(gl.RENDERER)
    })
    checkBackend(backend)
    if (process.env.CI && await browser.version() !== `Chrome/${PUPPETEER_REVISIONS.chrome}`) throw new Error('CI must use the pinned Chrome revision')
    return { server, browser, page, backend, reload, close: async () => { await browser.close(); await server.close() } }
  } catch (error) { await browser?.close(); await server.close(); throw error }
}
