/** Viewer state is separate from the material document and its undo history. */
export type SliceAxis = 'xy' | 'xz' | 'yz'
export interface ViewOptions {
  mode: 'scene' | 'slice'
  shape: string
  yaw: number
  pitch: number
  distance: number
  axis: SliceAxis
  position: number
}
export interface ShapeOption { id: string; label: string }
export const defaultView: ViewOptions = {
  mode: 'scene', shape: 'bitten-cube', yaw: 0.55, pitch: 0.35, distance: 2.1,
  axis: 'xy', position: 0,
}
export const clamp = (n: number, lo: number, hi: number) => Math.max(lo, Math.min(hi, n))
export function orbit(view: ViewOptions, dx: number, dy: number): ViewOptions {
  return { ...view, yaw: ((view.yaw - dx * 0.008) % (2 * Math.PI)), pitch: clamp(view.pitch + dy * 0.008, -1.45, 1.45) }
}
export function zoom(view: ViewOptions, delta: number): ViewOptions {
  return { ...view, distance: clamp(view.distance * Math.exp(clamp(delta, -200, 200) * 0.002), 1.1, 6) }
}
export function viewQuery(view: ViewOptions): URLSearchParams {
  return view.mode === 'scene'
    ? new URLSearchParams({ view: 'scene', shape: view.shape, yaw: String(view.yaw), pitch: String(view.pitch), distance: String(view.distance) })
    : new URLSearchParams({ view: 'slice', axis: view.axis, position: String(view.position) })
}
export function planeAxes(axis: SliceAxis): [number, number, number] {
  return axis === 'xy' ? [0, 1, 2] : axis === 'xz' ? [0, 2, 1] : [1, 2, 0]
}
export function projectPoint(point: number[], axis: SliceAxis): [number, number] {
  const [a, b] = planeAxes(axis)
  return [point[a], point[b]]
}
export function movePoint(point: number[], axis: SliceAxis, position: [number, number]): number[] {
  const [a, b] = planeAxes(axis)
  const result = [...point]
  result[a] = position[0]
  result[b] = position[1]
  return result
}
