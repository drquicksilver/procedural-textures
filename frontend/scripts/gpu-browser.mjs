import { install, Browser } from '@puppeteer/browsers'
import { PUPPETEER_REVISIONS } from 'puppeteer-core'
import { join } from 'node:path'
import { root } from './gpu-session.mjs'
const browser = await install({ cacheDir: join(root, '../out/gpu-browser'), browser: Browser.CHROME, buildId: PUPPETEER_REVISIONS.chrome })
console.log(browser.executablePath)
