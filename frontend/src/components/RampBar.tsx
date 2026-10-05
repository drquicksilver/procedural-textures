import { useEffect, useRef } from 'preact/hooks'
import { formatColour, parseColour, toCss } from '../colour'
import { compileRamp } from '../ramp'
import type { Json, Node } from '../types'
import { paintRamp } from './RampSwatch'

export interface StopJson {
  position: number
  colour: Json
}

interface Props {
  ramp: Node
  stops: StopJson[]
  /** Show the ramp without markers or editing (library ramps). */
  readOnly?: boolean
  selected: number
  onSelect: (index: number) => void
  /** `key` identifies the edit for undo coalescing. */
  onChange: (stops: StopJson[], key: string) => void
}

const HARD_EDGE_SPREAD = 13

/** The visible range: [0, 1], widened to include any stops outside it. */
function domainOf(stops: StopJson[]): [number, number] {
  const positions = stops.map((s) => s.position)
  return [Math.min(0, ...positions), Math.max(1, ...positions)]
}

function round(position: number): number {
  return Math.round(position * 1000) / 1000
}

/**
 * The ramp drawn as a gradient, with a marker for each stop. Drag markers to
 * move stops, click the bar to add one, and use the arrow keys or Delete on a
 * focused marker. Stops sharing a position (hard edges) are drawn side by
 * side so each can be grabbed.
 */
export function RampBar({ ramp, stops, readOnly, selected, onSelect, onChange }: Props) {
  const barRef = useRef<HTMLDivElement>(null)
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const extendedRef = useRef<HTMLCanvasElement>(null)
  const dragDomain = useRef<[number, number] | null>(null)
  const [lo, hi] = dragDomain.current ?? domainOf(stops)
  const isStops = ramp.type === 'stops' && !readOnly

  useEffect(() => {
    const f = compileRamp(ramp)
    paintRamp(canvasRef.current, f, lo, hi)
    paintRamp(extendedRef.current, f, lo - (hi - lo), hi + (hi - lo))
  })

  const positionAt = (clientX: number): number => {
    const rect = barRef.current!.getBoundingClientRect()
    const fraction = Math.min(1, Math.max(0, (clientX - rect.left) / rect.width))
    return round(lo + fraction * (hi - lo))
  }

  const moveStop = (index: number, position: number) =>
    onChange(
      stops.map((s, i) => (i === index ? { ...s, position } : s)),
      `stop-${index}-position`,
    )

  const removeStop = (index: number) => {
    if (stops.length <= 1) return
    onChange(
      stops.filter((_, i) => i !== index),
      'remove-stop',
    )
    onSelect(Math.max(0, Math.min(index, stops.length - 2)))
  }

  // Offsets for markers that share a position.
  const offsets = stops.map((stop, index) => {
    const group = stops.map((s, i) => (s.position === stop.position ? i : -1)).filter((i) => i >= 0)
    const rank = group.indexOf(index)
    return (rank - (group.length - 1) / 2) * HARD_EDGE_SPREAD
  })

  return (
    <div class="ramp-bar">
      <div
        class="ramp-gradient checkerboard"
        ref={barRef}
        title={isStops ? 'Click to add a stop' : undefined}
        onPointerDown={(e) => {
          if (!isStops || e.target !== canvasRef.current) return
          const position = positionAt(e.clientX)
          const colour = formatColour(compileRamp(ramp)(position))
          onChange([...stops, { position, colour }], 'add-stop')
          onSelect(stops.length)
        }}
      >
        <canvas ref={canvasRef} />
        {lo < 0 && <span class="ramp-tick" style={{ left: `${((0 - lo) / (hi - lo)) * 100}%` }} />}
        {hi > 1 && <span class="ramp-tick" style={{ left: `${((1 - lo) / (hi - lo)) * 100}%` }} />}
      </div>
      {isStops && (
        <div class="ramp-markers">
          {stops.map((stop, index) => (
            <button
              key={index}
              class={`ramp-marker ${index === selected ? 'is-selected' : ''} ${offsets[index] !== 0 ? 'is-hard-edge' : ''}`}
              style={{
                left: `calc(${((stop.position - lo) / (hi - lo)) * 100}% + ${offsets[index]}px)`,
                '--marker-colour': toCss(parseColour(stop.colour)),
              }}
              aria-label={`Stop ${index + 1} at ${stop.position}`}
              title={`${stop.position}${offsets[index] !== 0 ? ' (hard edge)' : ''}`}
              onPointerDown={(e) => {
                e.preventDefault()
                e.currentTarget.focus()
                onSelect(index)
                dragDomain.current = [lo, hi]
                e.currentTarget.setPointerCapture(e.pointerId)
              }}
              onPointerMove={(e) => {
                if (!e.currentTarget.hasPointerCapture(e.pointerId)) return
                moveStop(index, positionAt(e.clientX))
              }}
              onPointerUp={(e) => {
                e.currentTarget.releasePointerCapture(e.pointerId)
                dragDomain.current = null
              }}
              onKeyDown={(e) => {
                if (e.key === 'Delete' || e.key === 'Backspace') {
                  e.preventDefault()
                  removeStop(index)
                } else if (e.key === 'ArrowLeft' || e.key === 'ArrowRight') {
                  e.preventDefault()
                  const step = (e.shiftKey ? 0.1 : 0.01) * (e.key === 'ArrowLeft' ? -1 : 1)
                  moveStop(index, round(stop.position + step))
                }
              }}
            />
          ))}
        </div>
      )}
      <div class="ramp-extended checkerboard" title="How the ramp continues beyond its ends">
        <canvas ref={extendedRef} />
        <span class="ramp-tick" style={{ left: '33.333%' }} />
        <span class="ramp-tick" style={{ left: '66.667%' }} />
      </div>
    </div>
  )
}
