// @vitest-environment jsdom
import { act, cleanup, render } from '@testing-library/preact'
import { afterEach, beforeEach, expect, it, vi } from 'vitest'
import { Thumbnail } from './Thumbnail'

const gpu = vi.hoisted(() => ({ draw: vi.fn(), jobs: [] as (() => void)[] }))
vi.mock('../gpu/editor', () => ({
  draw: gpu.draw,
  enqueue: (work: () => void) => {
    gpu.jobs.push(work)
    return () => { const i = gpu.jobs.indexOf(work); if (i >= 0) gpu.jobs.splice(i, 1) }
  },
  onContextChange: () => () => {},
}))
let visibility: (visible: boolean) => void
beforeEach(() => {
  vi.useFakeTimers(); gpu.draw.mockClear(); gpu.jobs.length = 0
  vi.stubGlobal('IntersectionObserver', class {
    constructor(callback: IntersectionObserverCallback) {
      visibility = (visible) => callback([{ isIntersecting: visible } as IntersectionObserverEntry], this as unknown as IntersectionObserver)
    }
    observe() {}
    disconnect() {}
  })
  vi.spyOn(HTMLCanvasElement.prototype, 'getContext').mockReturnValue({ drawImage: vi.fn() } as unknown as CanvasRenderingContext2D)
})
afterEach(() => { cleanup(); vi.clearAllTimers(); vi.restoreAllMocks(); vi.unstubAllGlobals(); vi.useRealTimers() })

it('does no GPU work for offscreen cards, then debounces and renders a visible card', () => {
  render(<Thumbnail texture={{ type: 'flat', colour: '#123457ff' }} />)
  act(() => { vi.advanceTimersByTime(10000) })
  expect(gpu.jobs).toHaveLength(0); expect(gpu.draw).not.toHaveBeenCalled()
  act(() => visibility(true))
  act(() => { vi.advanceTimersByTime(249) }); expect(gpu.jobs).toHaveLength(0)
  act(() => { vi.advanceTimersByTime(1) }); expect(gpu.jobs).toHaveLength(1)
  act(() => gpu.jobs.shift()!())
  expect(gpu.draw).toHaveBeenCalledOnce()
})

it('cancels both debounced and queued work when a card moves offscreen', () => {
  render(<Thumbnail texture={{ type: 'flat', colour: '#654321ff' }} />)
  act(() => { visibility(true); vi.advanceTimersByTime(100); visibility(false); vi.advanceTimersByTime(1000) })
  expect(gpu.jobs).toHaveLength(0)
  act(() => { visibility(true); vi.advanceTimersByTime(250) })
  expect(gpu.jobs).toHaveLength(1)
  act(() => visibility(false))
  expect(gpu.jobs).toHaveLength(0); expect(gpu.draw).not.toHaveBeenCalled()
})

it('renders and caches representative slices separately from the default plane', () => {
  const texture = { type: 'flat', colour: '#123459ff' }
  const component = render(<Thumbnail texture={texture} preview={{ axis: 'xz', position: 0.5 }} />)
  act(() => { visibility(true); vi.advanceTimersByTime(250) })
  act(() => gpu.jobs.shift()!())
  expect(gpu.draw.mock.calls.at(-1)![2]).toMatchObject({ mode: 'slice', axis: 'xz', position: 0.5 })
  component.rerender(<Thumbnail texture={texture} preview={{ axis: 'xy', position: 0 }} />)
  act(() => { visibility(true); vi.advanceTimersByTime(250) })
  act(() => gpu.jobs.shift()!())
  expect(gpu.draw).toHaveBeenCalledTimes(2)
  expect(gpu.draw.mock.calls.at(-1)![2]).toMatchObject({ axis: 'xy', position: 0 })
})
