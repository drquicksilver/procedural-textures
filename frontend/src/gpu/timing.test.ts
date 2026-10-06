import { afterEach, beforeEach, expect, it, vi } from 'vitest'
import { GpuTimer } from './timing'

beforeEach(() => {
  vi.useFakeTimers()
  vi.stubGlobal('requestAnimationFrame', (callback: FrameRequestCallback) => setTimeout(() => callback(0), 16))
  vi.stubGlobal('cancelAnimationFrame', (id: ReturnType<typeof setTimeout>) => clearTimeout(id))
})
afterEach(() => { vi.unstubAllGlobals(); vi.useRealTimers() })
function setup(supported = true) {
  let ready = false, disjoint = false
  const extension = { TIME_ELAPSED_EXT: 1, GPU_DISJOINT_EXT: 2 }
  const gl = {
    QUERY_RESULT_AVAILABLE: 3, QUERY_RESULT: 4,
    getExtension: () => supported ? extension : null,
    createQuery: () => ({}), beginQuery: vi.fn(), endQuery: vi.fn(), deleteQuery: vi.fn(),
    isContextLost: () => false, getParameter: () => disjoint,
    getQueryParameter: (_query: unknown, parameter: number) => parameter === 3 ? ready : 12000000,
  }
  const timer = new GpuTimer(gl as unknown as WebGL2RenderingContext)
  return { timer, gl, ready: () => { ready = true }, disjoint: () => { disjoint = true } }
}
it('polls asynchronously until available, converts nanoseconds and releases the query', () => {
  const s = setup(), receive = vi.fn()
  s.timer.end(s.timer.begin()!, receive)
  vi.advanceTimersByTime(16); expect(receive).not.toHaveBeenCalled()
  s.ready(); vi.advanceTimersByTime(16)
  expect(receive).toHaveBeenCalledWith(12); expect(s.gl.deleteQuery).toHaveBeenCalledOnce()
  s.timer.dispose()
})
it('bounds outstanding measurements and never substitutes invalid disjoint results', () => {
  const s = setup(), receive = vi.fn()
  for (let i = 0; i < 4; i++) s.timer.end(s.timer.begin()!, receive)
  expect(s.timer.begin()).toBeNull()
  s.disjoint(); vi.advanceTimersByTime(16)
  expect(receive).not.toHaveBeenCalled(); expect(s.gl.deleteQuery).toHaveBeenCalledTimes(4)
  s.timer.dispose()
})
it('releases cold-frame/disposed measurements without delivering stale feedback', () => {
  const s = setup(), receive = vi.fn()
  s.timer.end(s.timer.begin()!)
  expect(s.gl.deleteQuery).toHaveBeenCalledOnce()
  s.timer.end(s.timer.begin()!, receive)
  s.timer.dispose(); s.ready(); vi.advanceTimersByTime(100)
  expect(receive).not.toHaveBeenCalled(); expect(s.gl.deleteQuery).toHaveBeenCalledTimes(2)
})
it('supports browsers without timer queries without issuing GL commands', () => {
  const s = setup(false)
  expect(s.timer.supported).toBe(false); expect(s.timer.begin()).toBeNull()
  expect(s.gl.beginQuery).not.toHaveBeenCalled(); s.timer.dispose()
})
