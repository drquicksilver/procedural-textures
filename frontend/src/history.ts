// Undo/redo history. Consecutive edits with the same coalescing key made
// close together in time (a slider being dragged, a number being typed) merge
// into a single undo step.

export interface History<T> {
  past: T[]
  present: T
  future: T[]
  lastKey: string | null
  lastTime: number
}

export const COALESCE_MS = 1000
export const MAX_UNDO = 200

export function createHistory<T>(present: T): History<T> {
  return { past: [], present, future: [], lastKey: null, lastTime: 0 }
}

/**
 * Record a new present. With a `key` matching the previous edit's key, within
 * COALESCE_MS of it, the edit replaces the present instead of adding a step.
 */
export function record<T>(history: History<T>, next: T, key: string | null = null, now = Date.now()): History<T> {
  if (next === history.present) return history
  const coalesce = key !== null && key === history.lastKey && now - history.lastTime < COALESCE_MS
  return {
    past: coalesce ? history.past : [...history.past, history.present].slice(-MAX_UNDO),
    present: next,
    future: [],
    lastKey: key,
    lastTime: now,
  }
}

export function undo<T>(history: History<T>): History<T> {
  const previous = history.past[history.past.length - 1]
  if (previous === undefined) return history
  return {
    past: history.past.slice(0, -1),
    present: previous,
    future: [history.present, ...history.future],
    lastKey: null,
    lastTime: 0,
  }
}

export function redo<T>(history: History<T>): History<T> {
  const [next, ...rest] = history.future
  if (next === undefined) return history
  return {
    past: [...history.past, history.present],
    present: next,
    future: rest,
    lastKey: null,
    lastTime: 0,
  }
}

export function canUndo<T>(history: History<T>): boolean {
  return history.past.length > 0
}

export function canRedo<T>(history: History<T>): boolean {
  return history.future.length > 0
}
