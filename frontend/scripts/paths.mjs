import { fileURLToPath } from 'node:url'

// Build tools can use repository paths without importing Vite or Puppeteer.
export const root = fileURLToPath(new URL('..', import.meta.url))
