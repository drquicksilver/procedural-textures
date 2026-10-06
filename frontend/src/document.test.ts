import { describe, expect, it } from 'vitest'
import vectors from '../../test-vectors/documents.json'
import metadata from './generated/metadata.json'
import { processDocument } from './document'

describe('Haskell document conformance', () => {
  for (const c of vectors.cases) it(c.name, () => {
    if ('output' in c) expect(processDocument(c.input)).toEqual(c.output)
    else {
      expect(() => processDocument(c.input)).toThrow()
      // Aeson and the browser have different prose; both must locate bad texture data.
      if (c.error?.includes('$.texture')) expect(() => processDocument(c.input)).toThrow(/\$\.texture/)
    }
  })
  it('validates current autosaves while retaining identity for canonical documents', () => {
    const doc = metadata.examples[0].document
    expect(processDocument(doc)).toBe(doc)
    expect(() => processDocument({ ...doc, texture: { type: 'flat', colour: null } })).toThrow('$.texture.colour')
  })
  it('rejects cycles, unsafe integers and non-finite coordinates', () => {
    const cycle: Record<string, unknown> = {}; cycle.self = cycle
    expect(() => processDocument(cycle)).toThrow('JSON tree')
    expect(() => processDocument({ version: 4, name: 'bad', texture: { type: 'tiled', columns: 2 ** 60 } })).toThrow('$.texture.columns')
    expect(() => processDocument({ version: 4, name: 'bad', texture: { type: 'perlin', scale: [Infinity, 0, 0] } })).toThrow('$.texture.scale')
  })
  it('does not mutate old autosaves or interpret inherited reference names as definitions', () => {
    const old = { version: 2, name: 'old', texture: { type: 'perlin', scale: [2, 8], ramp: { type: 'sinusoidal', from: '#ffffff', to: '#000000' } } }
    const before = structuredClone(old)
    expect(processDocument(old).texture.scale).toEqual([2, 8, 4])
    expect(processDocument(old).texture.mode).toBe('mirror')
    expect(old).toEqual(before)
    expect(() => processDocument({ version: 4, name: 'bad', texture: { type: 'perlin', scale: [1, 1, 1], ramp: { type: 'named', name: '__proto__' } } })).toThrow('Unknown named ramp')
  })
})
