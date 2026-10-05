// Fixtures shared with the Haskell test suite, which keeps them up to date
// (see test-vectors/ at the repository root).
import { readFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import type { Schema } from '../types'

function readVector<T>(name: string): T {
  const url = new URL(`../../../test-vectors/${name}`, import.meta.url)
  return JSON.parse(readFileSync(fileURLToPath(url), 'utf8')) as T
}

export const schema: Schema = readVector<Schema>('schema.json')

export { readVector }
