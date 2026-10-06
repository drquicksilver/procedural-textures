import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { PreviewScheduler, type PreviewResult } from './preview'
import type { TextureDocument } from './types'

interface Call {
  document: TextureDocument
  size: number
  signal: AbortSignal
  resolve: (blob: Blob) => void
  reject: (error: unknown) => void
}

function doc(name: string): TextureDocument {
  return { version: 1, name, description: '', texture: { type: 'flat', colour: '#000000ff' } }
}

function setup() {
  const calls: Call[] = []
  const results: PreviewResult[] = []
  const errors: unknown[] = []
  const scheduler = new PreviewScheduler(
    (document, size, signal) =>
      new Promise<Blob>((resolve, reject) => calls.push({ document, size, signal, resolve, reject })),
    (result) => results.push(result),
    (error) => errors.push(error),
    () => {},
    { lowSize: 64, fullSize: 512, settleMs: 200 },
  )
  return { scheduler, calls, results, errors }
}

const blob = new Blob()

async function flush() {
  await Promise.resolve()
  await Promise.resolve()
  await Promise.resolve()
}

describe('PreviewScheduler', () => {
  beforeEach(() => {
    vi.useFakeTimers()
  })
  afterEach(() => {
    vi.useRealTimers()
  })

  it('defers all full-resolution work until an interaction ends', async () => {
    const { scheduler, calls } = setup()
    scheduler.setOptions({ lowSize: 64, fullSize: 512, settleMs: 200, interactive: true })
    scheduler.update(doc('a'))
    calls[0].resolve(blob)
    await flush()
    vi.advanceTimersByTime(1000)
    expect(calls.map((c) => c.size)).toEqual([64])
    scheduler.setOptions({ lowSize: 64, fullSize: 512, settleMs: 200, interactive: false })
    vi.advanceTimersByTime(200)
    expect(calls.some((c) => c.size === 512)).toBe(true)
  })

  it('aborts an in-flight preview and ignores its result on disposal', async () => {
    const { scheduler, calls, results } = setup()
    scheduler.update(doc('a'))
    scheduler.dispose()
    expect(calls[0].signal.aborted).toBe(true)
    calls[0].resolve(blob)
    await flush()
    expect(results).toEqual([])
  })

  it('renders low resolution at once, then full resolution after the pause', async () => {
    const { scheduler, calls, results } = setup()
    scheduler.update(doc('a'))
    expect(calls.map((c) => c.size)).toEqual([64])
    calls[0].resolve(blob)
    await flush()
    expect(results.map((r) => r.size)).toEqual([64])
    vi.advanceTimersByTime(200)
    expect(calls.map((c) => c.size)).toEqual([64, 512])
    calls[1].resolve(blob)
    await flush()
    expect(results.map((r) => r.size)).toEqual([64, 512])
  })

  it('collapses changes during a low-resolution render into one follow-up of the latest document', async () => {
    const { scheduler, calls } = setup()
    scheduler.update(doc('a'))
    scheduler.update(doc('b'))
    scheduler.update(doc('c'))
    expect(calls.length).toBe(1)
    calls[0].resolve(blob)
    await flush()
    expect(calls.map((c) => c.document.name)).toEqual(['a', 'c'])
  })

  it('aborts a full-resolution render when the document changes', async () => {
    const { scheduler, calls, results } = setup()
    scheduler.update(doc('a'))
    calls[0].resolve(blob)
    await flush()
    vi.advanceTimersByTime(200)
    const full = calls[1]
    expect(full.size).toBe(512)
    scheduler.update(doc('b'))
    expect(full.signal.aborted).toBe(true)
    full.resolve(blob)
    await flush()
    expect(results.every((r) => r.size === 64)).toBe(true)
  })

  it('does not replace a full-resolution image with a late low-resolution one', async () => {
    const { scheduler, calls, results } = setup()
    scheduler.update(doc('a'))
    vi.advanceTimersByTime(200)
    calls[1].resolve(blob)
    await flush()
    calls[0].resolve(blob)
    await flush()
    expect(results.map((r) => r.size)).toEqual([512])
  })

  it('reports errors only for the current document', async () => {
    const { scheduler, calls, errors } = setup()
    scheduler.update(doc('a'))
    scheduler.update(doc('b'))
    calls[0].reject(new Error('old'))
    await flush()
    expect(errors).toEqual([])
    calls[1].reject(new Error('bad'))
    await flush()
    expect(errors).toHaveLength(1)
  })
})
