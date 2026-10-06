import { describe, expect, it } from 'vitest'
import type { TextureDocument } from '../types'
import { compileMaterial } from './compiler'

const document = (texture: TextureDocument['texture']): TextureDocument => ({ version: 4, name: 'Test', description: '', texture })
const ramp = { type: 'stops', stops: [{ position: 0, colour: '#000000ff' }, { position: 1, colour: '#ffffffff' }] }

describe('GPU material compiler', () => {
  it('keeps shader source stable through scalar, vector, colour, mode and octave edits', () => {
    const a = document({ type: 'fbm', scale: [1, 2, 3], octaves: 3, persistence: 0.5, lacunarity: 2, style: 'smooth', mode: 'clamp', ramp })
    const b = structuredClone(a)
    Object.assign(b.texture, { scale: [4, 5, 6], octaves: 7, persistence: 0.4, lacunarity: 1.9, style: 'ridged', mode: 'mirror', ramp: { ...ramp, stops: [{ position: -1, colour: '#123456ff' }, { position: 3, colour: '#abcdef80' }] } })
    const before = JSON.stringify(a), ca = compileMaterial(a), cb = compileMaterial(b)
    expect(ca.source).toBe(cb.source)
    expect(ca.parameters).not.toEqual(cb.parameters)
    expect(JSON.stringify(a)).toBe(before)
  })

  it('changes program structure for new branches and ramp stop counts', () => {
    const a = document({ type: 'flat', colour: '#ffffffff' })
    const b = document({ type: 'layer', top: a.texture, bottom: a.texture })
    expect(compileMaterial(a).source).not.toBe(compileMaterial(b).source)
    const linear = document({ type: 'linear', from: [0, 0, 0], to: [1, 0, 0], mode: 'clamp', ramp })
    const extended = structuredClone(linear)
    extended.texture.ramp = { ...ramp, stops: [...ramp.stops, { position: 0.5, colour: '#ff0000ff' }] }
    expect(compileMaterial(linear).source).not.toBe(compileMaterial(extended).source)
  })

  it('resolves document ramps without changing the input or shader structure', () => {
    const inline = document({ type: 'linear', from: [0, 0, 0], to: [1, 0, 0], mode: 'clamp', ramp })
    const shared = structuredClone(inline)
    shared.ramps = { test: ramp }
    shared.texture.ramp = { type: 'named', name: 'test' }
    expect(compileMaterial(shared)).toEqual(compileMaterial(inline))
    shared.texture.ramp = { type: 'named', name: 'missing' }
    expect(() => compileMaterial(shared)).toThrow('Missing named ramp missing')
  })

  it('fails clearly for unsupported nodes and unsafe numerical workloads', () => {
    expect(() => compileMaterial(document({ type: 'radial' }))).toThrow('Unsupported spike texture')
    const n = { type: 'fbm', scale: [1, 1, 1], octaves: 33, persistence: 0.5, lacunarity: 2, ramp }
    expect(() => compileMaterial(document(n))).toThrow('1–32 octaves')
    expect(() => compileMaterial(document({ ...n, octaves: 4, lacunarity: 1e40 }))).toThrow('Non-finite GPU parameter')
    expect(() => compileMaterial(document({ ...n, scale: [NaN, 1, 1] }))).toThrow('Expected finite scalar')
  })
})
