import { describe, expect, it } from 'vitest'
import { formatColour, parseColour, toHex8 } from './colour'

describe('colours', () => {
  it('parses hex with and without alpha', () => {
    expect(parseColour('#ff000080')).toEqual({ r: 1, g: 0, b: 0, a: 128 / 255 })
    expect(parseColour('#00ff00')).toEqual({ r: 0, g: 1, b: 0, a: 1 })
  })

  it('parses arrays', () => {
    expect(parseColour([0.15, 0.2, 0.6, 1])).toEqual({ r: 0.15, g: 0.2, b: 0.6, a: 1 })
    expect(parseColour([0.1, 0.2, 0.3])).toEqual({ r: 0.1, g: 0.2, b: 0.3, a: 1 })
  })

  it('formats exact 8-bit colours as hex and others as arrays, like the server', () => {
    expect(formatColour({ r: 1, g: 128 / 255, b: 0, a: 0 })).toBe('#ff800000')
    expect(formatColour({ r: 0.15, g: 0.2, b: 0.6, a: 1 })).toEqual([0.15, 0.2, 0.6, 1])
  })

  it('round-trips hex through parse and format', () => {
    for (const hex of ['#12345678', '#ffffffff', '#00000000']) {
      expect(formatColour(parseColour(hex))).toBe(hex)
    }
  })

  it('rounds to hex8', () => {
    expect(toHex8({ r: 0.15, g: 0.2, b: 0.6, a: 1 })).toBe('#263399ff')
  })
})
