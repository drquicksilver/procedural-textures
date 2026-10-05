import { describe, expect, it } from 'vitest'
import {
  detachRamp,
  importTexture,
  removeNamedRamp,
  renameNamedRamp,
  resolveRamp,
  shareRamp,
  sourcesOf,
  useCopiedRamp,
  usageCount,
} from './rampRefs'
import { schema } from './test/fixtures'
import type { LibraryRamp, Node, TextureDocument } from './types'

const red: Node = { type: 'stops', mode: 'clamp', stops: [{ position: 0, colour: '#ff0000ff' }] }
const blue: Node = { type: 'stops', mode: 'clamp', stops: [{ position: 0, colour: '#0000ffff' }] }
const builtins: LibraryRamp[] = [{ id: 'viridis', name: 'Viridis', description: '', category: 'scientific', ramp: blue }]

function doc(texture: Node, ramps?: Record<string, Node>): TextureDocument {
  return { version: 2, name: 'd', description: '', texture, ...(ramps ? { ramps } : {}) }
}

const twoPerlins = (top: Node, bottom: Node): Node => ({
  type: 'layer',
  top: { type: 'perlin', scale: [1, 1], ramp: top },
  bottom: { type: 'perlin', scale: [1, 1], ramp: bottom },
})

describe('ramp references', () => {
  it('resolves named and library references', () => {
    const sources = sourcesOf(doc(red, { eye: red }), builtins)
    expect(resolveRamp({ type: 'named', name: 'eye' }, sources)).toEqual(red)
    expect(resolveRamp({ type: 'builtin', name: 'viridis' }, sources)).toEqual(blue)
    expect(resolveRamp({ type: 'named', name: 'nope' }, sources)).toBeUndefined()
  })

  it('shares an inline ramp under a unique name, and counts its uses', () => {
    let d = doc(twoPerlins(red, blue), { mine: blue })
    d = shareRamp(d, ['top'], 'ramp', 'mine')
    expect(d.ramps).toEqual({ mine: blue, 'mine 2': red })
    expect(d.texture.top).toMatchObject({ ramp: { type: 'named', name: 'mine 2' } })
    expect(usageCount(schema, d, 'mine 2')).toBe(1)
  })

  it('detaches a reference into a local copy', () => {
    const d = detachRamp(doc(twoPerlins({ type: 'builtin', name: 'viridis' }, red)), ['top'], 'ramp', builtins)
    expect(d.texture.top).toMatchObject({ ramp: blue })
  })

  it('copies an outside ramp in, reusing an identical copy', () => {
    let d = doc(twoPerlins(red, red))
    d = useCopiedRamp(d, ['top'], 'ramp', 'Saved', blue)
    d = useCopiedRamp(d, ['bottom'], 'ramp', 'Saved', blue)
    expect(Object.keys(d.ramps!)).toEqual(['Saved'])
    expect(usageCount(schema, d, 'Saved')).toBe(2)
  })

  it('renames a named ramp with every reference, refusing taken names', () => {
    const named = { type: 'named', name: 'a' }
    let d = doc(twoPerlins(named, named), { a: red, b: blue })
    expect(renameNamedRamp(schema, d, 'a', 'b')).toBe(d)
    d = renameNamedRamp(schema, d, 'a', 'c')
    expect(Object.keys(d.ramps!).sort()).toEqual(['b', 'c'])
    expect(usageCount(schema, d, 'c')).toBe(2)
  })

  it('removes only unused named ramps', () => {
    const d = doc(twoPerlins({ type: 'named', name: 'a' }, red), { a: red, b: blue })
    expect(removeNamedRamp(schema, d, 'a').ramps).toEqual({ a: red, b: blue })
    expect(removeNamedRamp(schema, d, 'b').ramps).toEqual({ a: red })
  })

  it('imports a texture with its named ramps, renaming clashes', () => {
    const target = doc(red, { eye: blue })
    const source = doc(twoPerlins({ type: 'named', name: 'eye' }, { type: 'named', name: 'eye' }), { eye: red })
    const { ramps, texture } = importTexture(schema, target, source)
    expect(ramps).toEqual({ eye: blue, 'eye 2': red })
    expect(texture.top).toMatchObject({ ramp: { type: 'named', name: 'eye 2' } })
    expect(texture.bottom).toMatchObject({ ramp: { type: 'named', name: 'eye 2' } })
  })
})
