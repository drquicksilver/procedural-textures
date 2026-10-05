import { describe, expect, it } from 'vitest'
import { normaliseNumber, parseNumber, textShows } from './numbers'

describe('number rules', () => {
  it('rounds integers and applies the minimum', () => {
    expect(normaliseNumber(2.6, { integer: true })).toBe(3)
    expect(normaliseNumber(-3, { integer: true, min: 1 })).toBe(1)
    expect(normaliseNumber(-0.5, {})).toBe(-0.5)
  })

  it('parses only complete numbers', () => {
    expect(parseNumber('0.')).toBe(0)
    expect(parseNumber('-')).toBeNull()
    expect(parseNumber(' ')).toBeNull()
    expect(parseNumber('1e')).toBeNull()
  })

  it('accepts text that already means the value', () => {
    expect(textShows('0.', 0, {})).toBe(true)
    expect(textShows('2.6', 3, { integer: true })).toBe(true)
    expect(textShows('9', 8, {})).toBe(false)
    expect(textShows('-', 0, {})).toBe(false)
  })
})
