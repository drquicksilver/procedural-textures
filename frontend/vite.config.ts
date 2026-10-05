import preact from '@preact/preset-vite'
import { defineConfig } from 'vitest/config'

// In development, Vite serves the frontend and forwards API calls to the
// Haskell texture-server (`stack run texture-server`, port 8080).
export default defineConfig({
  plugins: [preact()],
  server: {
    proxy: {
      '/api': 'http://localhost:8080',
    },
  },
  test: {
    include: ['src/**/*.test.ts'],
  },
})
