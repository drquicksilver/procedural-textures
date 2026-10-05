import { useEffect, useRef } from 'preact/hooks'
import { toCss, type Rgba } from '../colour'
import { compileRamp } from '../ramp'
import type { Node } from '../types'

/** Fill a canvas with `f` evaluated from `from` (left edge) to `to` (right edge). */
export function paintRamp(canvas: HTMLCanvasElement | null, f: (t: number) => Rgba, from: number, to: number): void {
  if (!canvas) return
  const width = Math.max(1, Math.round(canvas.clientWidth * window.devicePixelRatio))
  if (canvas.width !== width) canvas.width = width
  canvas.height = 1
  const context = canvas.getContext('2d')
  if (!context) return
  context.clearRect(0, 0, width, 1)
  for (let x = 0; x < width; x++) {
    context.fillStyle = toCss(f(from + ((x + 0.5) / width) * (to - from)))
    context.fillRect(x, 0, 1, 1)
  }
}

/** A concrete ramp drawn over [0, 1] on a checkerboard. */
export function RampSwatch({ ramp }: { ramp: Node }) {
  const canvas = useRef<HTMLCanvasElement>(null)
  useEffect(() => paintRamp(canvas.current, compileRamp(ramp), 0, 1), [ramp])
  return (
    <div class="ramp-swatch checkerboard">
      <canvas ref={canvas} />
    </div>
  )
}
