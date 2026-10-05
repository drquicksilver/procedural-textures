// A port of OkLab.hs: blending colours in OKLab, premultiplied by alpha, as
// CSS Color 4 interpolates "in oklab". Kept formula for formula in step with
// the Haskell, which the shared ramp vectors check.

import type { Rgba } from './colour'

/** A colour in OKLab with straight (not premultiplied) alpha. */
export interface Lab {
  l: number
  a: number
  b: number
  alpha: number
}

function toLinear(c: number): number {
  return c <= 0.04045 ? c / 12.92 : ((c + 0.055) / 1.055) ** 2.4
}

function toSrgb(l: number): number {
  return l <= 0.0031308 ? 12.92 * l : 1.055 * l ** (1 / 2.4) - 0.055
}

function cbrt(x: number): number {
  return Math.sign(x) * Math.abs(x) ** (1 / 3)
}

function unit(x: number): number {
  return Math.max(0, Math.min(1, x))
}

export function toLab({ r, g, b, a }: Rgba): Lab {
  const lr = toLinear(r)
  const lg = toLinear(g)
  const lb = toLinear(b)
  const l = cbrt(0.4122214708 * lr + 0.5363325363 * lg + 0.0514459929 * lb)
  const m = cbrt(0.2119034982 * lr + 0.6806995451 * lg + 0.1073969566 * lb)
  const s = cbrt(0.0883024619 * lr + 0.2817188376 * lg + 0.6299787005 * lb)
  return {
    l: 0.2104542553 * l + 0.793617785 * m - 0.0040720468 * s,
    a: 1.9779984951 * l - 2.428592205 * m + 0.4505937099 * s,
    b: 0.0259040371 * l + 0.7827717662 * m - 0.808675766 * s,
    alpha: a,
  }
}

export function fromLab({ l: bigL, a: aa, b: bb, alpha }: Lab): Rgba {
  const l = (bigL + 0.3963377774 * aa + 0.2158037573 * bb) ** 3
  const m = (bigL - 0.1055613458 * aa - 0.0638541728 * bb) ** 3
  const s = (bigL - 0.0894841775 * aa - 1.291485548 * bb) ** 3
  const r = 4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s
  const g = -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s
  const b = -0.0041960863 * l - 0.7034186147 * m + 1.707614701 * s
  return { r: unit(toSrgb(r)), g: unit(toSrgb(g)), b: unit(toSrgb(b)), a: unit(alpha) }
}

/** Blend a fraction `t` of the way from one colour to another, premultiplied by alpha. */
export function mixLab(t: number, c1: Lab, c2: Lab): Lab {
  const lerp = (x: number, y: number) => x + (y - x) * t
  const alpha = lerp(c1.alpha, c2.alpha)
  if (alpha <= 0) return { l: lerp(c1.l, c2.l), a: lerp(c1.a, c2.a), b: lerp(c1.b, c2.b), alpha: 0 }
  const premultiplied = (x: number, y: number) => lerp(x * c1.alpha, y * c2.alpha) / alpha
  return { l: premultiplied(c1.l, c2.l), a: premultiplied(c1.a, c2.a), b: premultiplied(c1.b, c2.b), alpha }
}
