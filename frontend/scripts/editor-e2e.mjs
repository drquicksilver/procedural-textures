// Serve the production build beneath a repository-style prefix. No Haskell,
// SPA fallback or API proxy: a mistaken asset/API path really returns 404.
import { createServer } from 'node:http'
import { readFile } from 'node:fs/promises'
import { existsSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { join, resolve, extname } from 'node:path'
import { spawn } from 'node:child_process'
import { Browser, computeExecutablePath } from '@puppeteer/browsers'
import { PUPPETEER_REVISIONS } from 'puppeteer-core'

const root = fileURLToPath(new URL('..', import.meta.url)), dist = join(root, 'dist')
const prefix = '/procedural-textures/'
const types = { '.html': 'text/html', '.js': 'text/javascript', '.css': 'text/css', '.json': 'application/json', '.svg': 'image/svg+xml', '.png': 'image/png' }
const server = createServer(async (request, response) => {
  try {
    const path = decodeURIComponent(new URL(request.url, 'http://localhost').pathname)
    if (!path.startsWith(prefix) || path.includes('/api/')) throw new Error('Not a static asset')
    const file = resolve(dist, path.slice(prefix.length) || 'index.html')
    if (!file.startsWith(`${dist}/`)) throw new Error('Invalid path')
    const data = await readFile(file)
    response.writeHead(200, { 'Content-Type': types[extname(file)] ?? 'application/octet-stream' }); response.end(data)
  } catch { response.writeHead(404); response.end('Not found') }
})
const pinned = computeExecutablePath({ cacheDir: join(root, '../out/gpu-browser'), browser: Browser.CHROME, buildId: PUPPETEER_REVISIONS.chrome })
const chrome = process.env.CHROME ?? [pinned, '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome', '/usr/bin/google-chrome', '/usr/bin/chromium'].find(existsSync)
await new Promise((resolve, reject) => { server.once('error', reject); server.listen(Number(process.env.E2E_PORT ?? 0), '127.0.0.1', resolve) })
try {
  const child = spawn(process.execPath, ['--test', ...process.argv.slice(2), 'e2e/*.test.mjs'], { cwd: root, stdio: 'inherit', env: { ...process.env, ...(chrome ? { CHROME: chrome } : {}), E2E_URL: `http://127.0.0.1:${server.address().port}${prefix}` } })
  process.exitCode = await new Promise((resolve) => { child.on('error', () => resolve(1)); child.on('exit', (code) => resolve(code ?? 1)) })
} finally { server.closeAllConnections(); await new Promise((resolve) => server.close(resolve)) }
