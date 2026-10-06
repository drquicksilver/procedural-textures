import { useRef, useState } from 'preact/hooks'
import { variantOf } from '../tree'
import type { Json, Node, Schema } from '../types'

interface Props {
  schema: Schema
  node: Node
  /** Set one field of the node. */
  onChange: (field: string, value: Json) => void
}

type Point = [number, number]

function asPoint(value: Json | undefined): Point | null {
  return Array.isArray(value) && typeof value[0] === 'number' && typeof value[1] === 'number' ? [value[0], value[1]] : null
}

const SNAP = 0.05

function snap(value: number, enabled: boolean): number {
  return enabled ? Math.round(value / SNAP) * SNAP : Math.round(value * 1000) / 1000
}

function percent(value: number): string {
  return `${value * 100}%`
}

/**
 * Handles for the selected node, drawn over the preview in texture
 * coordinates (the square is [0, 1]², y downwards). Points and radii are
 * dragged directly; hold Shift to snap to a 0.05 grid. Hovering shows the
 * coordinate under the pointer.
 */
export function Handles({ schema, node, onChange }: Props) {
  const surface = useRef<HTMLDivElement>(null)
  const [hover, setHover] = useState<Point | null>(null)
  const [dragging, setDragging] = useState<string | null>(null)
  const variant = variantOf(schema, 'texture', node.type)

  const toUnit = (e: PointerEvent): Point => {
    const rect = surface.current!.getBoundingClientRect()
    return [(e.clientX - rect.left) / rect.width, (e.clientY - rect.top) / rect.height]
  }

  const handles: { key: string; at: Point; kind: 'point' | 'radius'; drag: (p: Point, snapping: boolean) => void }[] = []
  const circles: { centre: Point; radius: number }[] = []
  for (const field of variant?.fields ?? []) {
    if (field.handle?.kind === 'point') {
      const at = asPoint(node[field.key])
      if (at) handles.push({ key: field.key, at, kind: 'point', drag: (p, s) => onChange(field.key, [snap(p[0], s), snap(p[1], s), (node[field.key] as number[])[2] ?? 0]) })
    } else if (field.handle?.kind === 'radius') {
      const centre = asPoint(node[field.handle.centre])
      const radius = node[field.key]
      if (centre && typeof radius === 'number') {
        circles.push({ centre, radius })
        handles.push({
          key: field.key,
          at: [centre[0] + radius, centre[1]],
          kind: 'radius',
          drag: (p, s) => onChange(field.key, Math.max(0, snap(Math.hypot(p[0] - centre[0], p[1] - centre[1]), s))),
        })
      }
    }
  }
  const lines = (variant?.guides ?? []).flatMap((guide) => {
    const from = asPoint(node[guide.from])
    const to = asPoint(node[guide.to])
    return from && to ? [{ from, to }] : []
  })
  const labelOf = (key: string) => variant?.fields.find((f) => f.key === key)?.label ?? key

  return (
    <div
      class="handles"
      ref={surface}
      onPointerMove={(e) => setHover(toUnit(e))}
      onPointerLeave={() => setHover(null)}
    >
      <svg class="guides" viewBox="0 0 1 1" preserveAspectRatio="none" aria-hidden="true">
        {lines.map(({ from, to }, i) => (
          <line key={i} x1={from[0]} y1={from[1]} x2={to[0]} y2={to[1]} />
        ))}
        {circles.map(({ centre, radius }, i) => (
          <ellipse key={i} cx={centre[0]} cy={centre[1]} rx={radius} ry={radius} />
        ))}
      </svg>
      {handles.map((handle) => (
        <button
          key={handle.key}
          class={`handle is-${handle.kind} ${dragging === handle.key ? 'is-dragging' : ''}`}
          style={{ left: percent(handle.at[0]), top: percent(handle.at[1]) }}
          aria-label={`${labelOf(handle.key)} handle`}
          title={labelOf(handle.key)}
          onPointerDown={(e) => {
            e.preventDefault()
            e.stopPropagation()
            e.currentTarget.setPointerCapture(e.pointerId)
            setDragging(handle.key)
          }}
          onPointerMove={(e) => {
            if (!e.currentTarget.hasPointerCapture(e.pointerId)) return
            handle.drag(toUnit(e), e.shiftKey)
          }}
          onPointerUp={(e) => {
            e.currentTarget.releasePointerCapture(e.pointerId)
            setDragging(null)
          }}
        >
          <span class="handle-label">{labelOf(handle.key)}</span>
        </button>
      ))}
      {hover && (
        <div class="coordinates" aria-live="off">
          x {hover[0].toFixed(3)} y {hover[1].toFixed(3)}
        </div>
      )}
    </div>
  )
}
