// Colours in documents are "#rrggbb", "#rrggbbaa" or [r, g, b(, a)] with
// finite channels; array values may lie outside [0, 1], as in TextureJson.

import type { Json } from './types'

export interface Rgba {
  r: number
  g: number
  b: number
  a: number
}

export const BLACK: Rgba = { r: 0, g: 0, b: 0, a: 1 }

export function parseColour(value: Json | undefined): Rgba {
  if (typeof value === 'string') {
    const match = /^#([0-9a-f]{6})([0-9a-f]{2})?$/i.exec(value)
    if (!match) return BLACK
    const byte = (i: number) => parseInt(value.slice(1 + 2 * i, 3 + 2 * i), 16) / 255
    return { r: byte(0), g: byte(1), b: byte(2), a: match[2] ? byte(3) : 1 }
  }
  if (Array.isArray(value) && (value.length === 3 || value.length === 4) && value.every((v) => typeof v === 'number')) {
    const [r, g, b, a = 1] = value as number[]
    return { r, g, b, a }
  }
  return BLACK
}

function exactByte(channel: number): number | null {
  const byte = Math.round(channel * 255)
  return channel >= 0 && channel <= 1 && byte / 255 === channel ? byte : null
}

/** Hex when every channel is an exact 8-bit value, as the server writes it; an array otherwise. */
export function formatColour(colour: Rgba): Json {
  const bytes = [colour.r, colour.g, colour.b, colour.a].map(exactByte)
  if (bytes.every((b) => b !== null)) {
    return '#' + bytes.map((b) => b!.toString(16).padStart(2, '0')).join('')
  }
  return [colour.r, colour.g, colour.b, colour.a]
}

function toByte(channel: number): number {
  return Math.round(Math.min(1, Math.max(0, channel)) * 255)
}

/** "#rrggbb", ignoring alpha, for <input type="color">. */
export function toHex6(colour: Rgba): string {
  return '#' + [colour.r, colour.g, colour.b].map((c) => toByte(c).toString(16).padStart(2, '0')).join('')
}

/** "#rrggbbaa", rounding to 8-bit channels. */
export function toHex8(colour: Rgba): string {
  return toHex6(colour) + toByte(colour.a).toString(16).padStart(2, '0')
}

export function fromHex6(hex: string, alpha: number): Rgba {
  const rgb = parseColour(hex)
  return { ...rgb, a: alpha }
}

export function toCss(colour: Rgba): string {
  return `rgba(${toByte(colour.r)}, ${toByte(colour.g)}, ${toByte(colour.b)}, ${Math.min(1, Math.max(0, colour.a))})`
}

export function lerp(t: number, from: Rgba, to: Rgba): Rgba {
  return {
    r: from.r + (to.r - from.r) * t,
    g: from.g + (to.g - from.g) * t,
    b: from.b + (to.b - from.b) * t,
    a: from.a + (to.a - from.a) * t,
  }
}
