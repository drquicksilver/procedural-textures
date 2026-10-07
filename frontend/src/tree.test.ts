import { describe, expect, it } from 'vitest'
import { schema } from './test/fixtures'
import {
  changeType,
  flatten,
  getAt,
  setAt,
  setField,
  swapChildren,
  wrap,
  wrapOptions,
} from './tree'
import type { Node } from './types'

const red: Node = { type: 'flat', colour: '#ff0000ff' }
const blue: Node = { type: 'flat', colour: '#0000ffff' }
const ramp = { type: 'stops', mode: 'clamp', stops: [{ position: 0, colour: '#000000ff' }] }

const tree: Node = {
  type: 'layer',
  top: { type: 'turbulence', amount: 0.1, octaves: 3, persistence: 0.5, lacunarity: 2, base: red },
  bottom: blue,
}

describe('getAt / setAt / setField', () => {
  it('finds nodes by path', () => {
    expect(getAt(tree, ['top', 'base'])).toEqual(red)
    expect(getAt(tree, ['bottom', 'nope'])).toBeUndefined()
  })

  it('replaces nodes without mutating the original', () => {
    const updated = setAt(tree, ['top', 'base'], blue)
    expect(getAt(updated, ['top', 'base'])).toEqual(blue)
    expect(getAt(tree, ['top', 'base'])).toEqual(red)
    expect(updated.bottom).toBe(tree.bottom)
  })

  it('sets fields', () => {
    const updated = setField(tree, ['top'], 'amount', 0.5)
    expect(getAt(updated, ['top'])?.amount).toBe(0.5)
  })
})

describe('changeType', () => {
  it('keeps fields shared by both variants and defaults the rest', () => {
    const linear: Node = { type: 'linear', from: [0.1, 0.2], to: [0.3, 0.4], ramp }
    const circular = changeType(schema, 'texture', linear, 'circular')
    expect(circular.type).toBe('circular')
    expect(circular.ramp).toEqual(ramp)
    expect(circular.radius).toBe(0.5)
    expect(circular.centre).toEqual([0.5, 0.5, 0])
  })

  it('switches ramp kinds', () => {
    const sinusoidal = changeType(schema, 'ramp', ramp as Node, 'sinusoidal')
    expect(sinusoidal.type).toBe('sinusoidal')
    expect(sinusoidal.from).toBeDefined()
  })
})

describe('wrapping and swapping', () => {
  it('offers each texture slot of each variant', () => {
    const labels = wrapOptions(schema).map((o) => `${o.type}.${o.key}`)
    expect(labels).toEqual(['domain.base', 'mix.a', 'mix.b', 'turbulence.base', 'tiled.a', 'tiled.b', 'layer.top', 'layer.bottom'])
  })

  it('wraps a node into the chosen slot', () => {
    const wrapped = wrap(schema, red, 'layer', 'bottom')
    expect(wrapped.type).toBe('layer')
    expect(wrapped.bottom).toEqual(red)
    expect(wrapped.top).toBeDefined()
  })

  it('swaps two children', () => {
    const swapped = swapChildren(schema, tree)!
    expect(swapped.top).toEqual(tree.bottom)
    expect(swapped.bottom).toEqual(tree.top)
    expect(swapChildren(schema, red)).toBeUndefined()
  })
})

describe('flatten', () => {
  it('lists nodes depth-first with their field labels', () => {
    const entries = flatten(schema, tree, new Set())
    expect(entries.map((e) => [e.path.join('.'), e.depth, e.fieldLabel])).toEqual([
      ['', 0, undefined],
      ['top', 1, 'Top'],
      ['top.base', 2, 'Base'],
      ['bottom', 1, 'Bottom'],
    ])
  })

  it('skips collapsed subtrees', () => {
    const entries = flatten(schema, tree, new Set(['top']))
    expect(entries.map((e) => e.path.join('.'))).toEqual(['', 'top', 'bottom'])
  })
})
