// The rules every way of entering a number (typing, buttons, arrow keys)
// shares, so they cannot disagree.

export interface NumberRules {
  integer?: boolean
  /** A hard lower bound, e.g. 1 for a tile count. */
  min?: number
}

export function normaliseNumber(value: number, rules: NumberRules): number {
  let result = rules.integer ? Math.round(value) : value
  if (rules.min !== undefined) result = Math.max(rules.min, result)
  return result
}

/** The number some text means, or null if it is empty or not a number (yet). */
export function parseNumber(text: string): number | null {
  if (text.trim() === '') return null
  const parsed = Number(text)
  return Number.isFinite(parsed) ? parsed : null
}

/**
 * Whether a field's text already shows `value`. Half-typed text such as
 * "0." means 0, so it is left alone while it still means the value.
 */
export function textShows(text: string, value: number, rules: NumberRules): boolean {
  const parsed = parseNumber(text)
  return parsed !== null && normaliseNumber(parsed, rules) === value
}
