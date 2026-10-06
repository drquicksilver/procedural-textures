import preact from '@preact/preset-vite'
import { defineConfig } from 'vitest/config'

export default defineConfig({
  base: './',
  plugins: [preact()],
  build: {
    rolldownOptions: { input: { editor: 'index.html', spike: 'spike.html' } },
  },
  test: {
    include: ['src/**/*.test.ts', 'src/**/*.test.tsx'],
  },
})
