import { afterEach, beforeEach, expect, it, vi } from 'vitest'
import { enqueue } from './editor'

beforeEach(() => {
  vi.useFakeTimers()
  vi.stubGlobal('requestAnimationFrame', (callback: FrameRequestCallback) => setTimeout(() => callback(0), 16))
  vi.stubGlobal('cancelAnimationFrame', (id: ReturnType<typeof setTimeout>) => clearTimeout(id))
})
afterEach(() => { vi.runAllTimers(); vi.unstubAllGlobals(); vi.useRealTimers() })

it('submits one job per frame and prioritises the viewer and export over library thumbnails', () => {
  const shown: string[] = []
  enqueue(() => shown.push('thumbnail'), 1)
  enqueue(() => shown.push('viewer'))
  enqueue(() => shown.push('export'), -1)
  vi.advanceTimersByTime(16); expect(shown).toEqual(['export'])
  vi.advanceTimersByTime(16); expect(shown).toEqual(['export', 'viewer'])
  vi.advanceTimersByTime(16); expect(shown).toEqual(['export', 'viewer', 'thumbnail'])
})

it('removes cancelled work and restarts after the whole queue was cancelled', () => {
  const stale = vi.fn(), latest = vi.fn()
  const cancel = enqueue(stale)
  cancel(); cancel()
  vi.advanceTimersByTime(32); expect(stale).not.toHaveBeenCalled()
  enqueue(latest)
  vi.advanceTimersByTime(16); expect(latest).toHaveBeenCalledOnce()
})
