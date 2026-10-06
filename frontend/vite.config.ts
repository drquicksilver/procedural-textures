import preact from '@preact/preset-vite'
import { defineConfig } from 'vitest/config'

// In development, Vite serves the frontend and forwards API calls to the
// Haskell texture-server, on port 8080 unless API_PORT says otherwise
// (`make dev PORT=…` sets it).
const apiPort = process.env.API_PORT ?? '8080'

export default defineConfig({
  plugins: [preact()],
  build: {
    rolldownOptions: { input: { editor: 'index.html', spike: 'spike.html' } },
  },
  server: {
    proxy: {
      '/api': `http://localhost:${apiPort}`,
    },
  },
  test: {
    include: ['src/**/*.test.ts', 'src/**/*.test.tsx'],
  },
})
