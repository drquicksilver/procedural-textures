import { afterEach, beforeEach, expect, it, vi } from 'vitest'
import { CanvasPreview } from './canvas-preview'

beforeEach(() => vi.useFakeTimers())
afterEach(() => vi.useRealTimers())
function setup() {
  const jobs = new Set<() => void>(), render = vi.fn(), error = vi.fn(), busy = vi.fn()
  const scheduler = new CanvasPreview<string>(render, (work) => { jobs.add(work); return () => { jobs.delete(work) } }, error, busy,
    { previewSize: () => 96, fullSize: 512, settleMs: 180, interactive: false })
  const frame = () => { for (const job of [...jobs]) { jobs.delete(job); job() } }
  return { scheduler, jobs, render, error, busy, frame }
}
it('coalesces changes to the latest state before GPU submission', () => {
  const s = setup()
  for (let i = 0; i < 100; i++) s.scheduler.update(String(i))
  expect(s.jobs.size).toBe(1)
  s.frame(); expect(s.render.mock.calls).toEqual([['99', 96]])
  vi.advanceTimersByTime(180); s.frame()
  expect(s.render.mock.calls).toEqual([['99', 96], ['99', 512]])
  expect(s.busy).toHaveBeenLastCalledWith(false)
})
it('cancels obsolete queued refinement when editing resumes', () => {
  const s = setup(); s.scheduler.update('old'); s.frame()
  vi.advanceTimersByTime(180)
  s.scheduler.update('new'); s.frame()
  expect(s.render.mock.calls).toEqual([['old', 96], ['new', 96]])
})
it('does not refine until the interaction ends, and picks up resize changes', () => {
  const s = setup()
  s.scheduler.setOptions({ previewSize: () => 96, fullSize: 512, settleMs: 180, interactive: true })
  s.scheduler.update('drag'); s.frame(); vi.advanceTimersByTime(1000); s.frame()
  expect(s.render.mock.calls).toEqual([['drag', 96]])
  s.scheduler.setOptions({ previewSize: () => 96, fullSize: 768, settleMs: 180, interactive: false })
  s.frame(); vi.advanceTimersByTime(180); s.frame()
  expect(s.render).toHaveBeenLastCalledWith('drag', 768)
})
it('disposes queued work and timers', () => {
  const s = setup(); s.scheduler.update('old'); s.scheduler.dispose()
  vi.advanceTimersByTime(1000); s.frame(); expect(s.render).not.toHaveBeenCalled()
  expect(s.busy).toHaveBeenLastCalledWith(false)
})
it('reports rendering errors and recovers on later updates', () => {
  const s = setup(); s.render.mockImplementationOnce(() => { throw new Error('shader') })
  s.scheduler.update('bad'); s.frame(); expect(s.error).toHaveBeenCalledOnce()
  s.scheduler.update('good'); s.frame(); expect(s.render).toHaveBeenLastCalledWith('good', 96)
})
it('chooses each interactive frame from the latest budget estimate and still settles at full size', () => {
  const s = setup()
  let size = 512
  s.scheduler.setOptions({ previewSize: () => size, fullSize: 512, settleMs: 180, interactive: true })
  s.scheduler.update('first'); s.frame(); expect(s.render).toHaveBeenLastCalledWith('first', 512)
  size = 320
  s.scheduler.update('second'); s.frame(); expect(s.render).toHaveBeenLastCalledWith('second', 320)
  s.scheduler.setOptions({ previewSize: () => size, fullSize: 512, settleMs: 180, interactive: false })
  s.frame(); vi.advanceTimersByTime(180); s.frame()
  expect(s.render).toHaveBeenLastCalledWith('second', 512)
})

it('waits for preparation, cancels stale results and keeps the latest frame',async()=>{
  const jobs=new Set<()=>void>(),render=vi.fn(),error=vi.fn(),busy=vi.fn()
  const ready=new Map<string,()=>void>(),cancel=vi.fn()
  const scheduler=new CanvasPreview<string>(render,work=>{jobs.add(work);return()=>{jobs.delete(work)}},error,busy,{previewSize:()=>96,fullSize:512,settleMs:180,interactive:true},state=>({ready:new Promise<void>(resolve=>ready.set(state,resolve)),cancel}))
  const frame=()=>{for(const work of [...jobs]){jobs.delete(work);work()}}
  scheduler.update('old');frame();scheduler.update('new');frame()
  ready.get('old')!();await Promise.resolve();expect(render).not.toHaveBeenCalled()
  ready.get('new')!();await Promise.resolve();expect(render).toHaveBeenCalledWith('new',96);expect(cancel).toHaveBeenCalled()
  scheduler.dispose()
})
