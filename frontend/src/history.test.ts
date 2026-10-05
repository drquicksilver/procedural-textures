import { describe, expect, it } from 'vitest'
import { COALESCE_MS, canRedo, canUndo, createHistory, record, redo, undo } from './history'

describe('history', () => {
  it('undoes and redoes', () => {
    let h = createHistory(1)
    h = record(h, 2)
    h = record(h, 3)
    h = undo(h)
    expect(h.present).toBe(2)
    h = undo(h)
    expect(h.present).toBe(1)
    expect(canUndo(h)).toBe(false)
    h = redo(h)
    expect(h.present).toBe(2)
    expect(canRedo(h)).toBe(true)
  })

  it('drops the redo future on a new edit', () => {
    let h = record(record(createHistory(1), 2), 3)
    h = record(undo(h), 4)
    expect(canRedo(h)).toBe(false)
    expect(undo(h).present).toBe(2)
  })

  it('coalesces quick edits with the same key', () => {
    let h = createHistory(0)
    h = record(h, 1, 'slider', 1000)
    h = record(h, 2, 'slider', 1100)
    h = record(h, 3, 'slider', 1200)
    expect(h.past).toEqual([0])
    expect(undo(h).present).toBe(0)
  })

  it('does not coalesce different keys or slow edits', () => {
    let h = createHistory(0)
    h = record(h, 1, 'a', 1000)
    h = record(h, 2, 'b', 1100)
    h = record(h, 3, 'b', 1100 + COALESCE_MS + 1)
    expect(h.past).toEqual([0, 1, 2])
  })

  it('never coalesces across an undo', () => {
    let h = createHistory(0)
    h = record(h, 1, 'a', 1000)
    h = undo(h)
    h = record(h, 2, 'a', 1001)
    expect(h.past).toEqual([0])
  })
})
