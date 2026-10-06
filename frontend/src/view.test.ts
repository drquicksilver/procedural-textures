import { describe, it, expect } from 'vitest'
import { defaultView, orbit, zoom, viewQuery, projectPoint, movePoint } from './view'

describe('3D viewer state', () => {
  it('bounds pitch and zoom without changing the material or shape', () => {
    expect(orbit(defaultView, 30, 10000).pitch).toBe(1.45)
    expect(orbit(defaultView, 30, -10000).pitch).toBe(-1.45)
    expect(zoom({ ...defaultView, distance: 6 }, 200).distance).toBe(6)
    expect(zoom({ ...defaultView, distance: 1.1 }, -200).distance).toBe(1.1)
    expect(orbit(defaultView, 20, 30).shape).toBe(defaultView.shape)
  })
  it('sends only the active view parameters', () => {
    expect(viewQuery(defaultView).get('view')).toBe('scene')
    const query = viewQuery({ ...defaultView, mode: 'slice', axis: 'yz', position: 0.3 })
    expect(query.get('shape')).toBeNull()
    expect(query.get('axis')).toBe('yz')
    expect(query.get('position')).toBe('0.3')
  })
  it.each(['xy', 'xz', 'yz'] as const)('moves projected handles in %s while preserving the hidden coordinate', (axis) => {
    const point = [0.2, 0.3, 0.4]
    const moved = movePoint(point, axis, [0.7, 0.8])
    expect(projectPoint(moved, axis)).toEqual([0.7, 0.8])
    expect(point).toEqual([0.2, 0.3, 0.4])
    expect(moved.filter((n) => n === 0.2 || n === 0.3 || n === 0.4)).toHaveLength(1)
  })
})
