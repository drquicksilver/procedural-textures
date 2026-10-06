import { expect, it } from 'vitest'
import { AdaptiveResolution } from './resolution'

it('starts at full size and keeps cheap rendering at full size', () => {
  const r = new AdaptiveResolution(15)
  expect(r.size(704)).toBe(704)
  r.sample(704, 4, 1); expect(r.size(704)).toBe(704)
})
it('reduces pixel count according to measured cost with time-budget headroom', () => {
  const r = new AdaptiveResolution(15)
  r.sample(512, 60, 1)
  const size = r.size(512)
  expect(size).toBeGreaterThanOrEqual(192); expect(size).toBeLessThanOrEqual(256)
  expect(60 * (size / 512) ** 2).toBeLessThan(15)
})
it('reacts immediately to expensive views but recovers gradually as they get cheaper', () => {
  const r = new AdaptiveResolution()
  r.sample(512, 4, 1); expect(r.size(512)).toBe(512)
  r.sample(512, 60, 2)
  const low = r.size(512)
  r.sample(low, 1, 3)
  expect(r.size(512)).toBeGreaterThanOrEqual(low)
  expect(r.size(512)).toBeLessThan(512)
  for (let i = 4; i < 30; i++) r.sample(r.size(512), 1, i)
  expect(r.size(512)).toBe(512)
})
it('rejects obsolete asynchronous results and invalid timing samples', () => {
  const r = new AdaptiveResolution()
  r.sample(512, 2, 5)
  r.sample(512, 100, 4); r.sample(512, NaN, 6); r.sample(512, 0, 7)
  expect(r.size(512)).toBe(512)
})
it('bounds resolution to the viewer, including small frames, and resets after context loss', () => {
  const r = new AdaptiveResolution()
  r.sample(512, 10000, 1)
  expect(r.size(1024)).toBe(64); expect(r.size(32)).toBe(32)
  r.reset(); expect(r.size(1024)).toBe(1024)
  r.sample(1024, 1, 1); expect(r.size(1024)).toBe(1024)
})
