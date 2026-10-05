import { useState } from 'preact/hooks'
import { formatColour, parseColour } from '../colour'
import { compileRamp, type RampMode } from '../ramp'
import { changeType } from '../tree'
import type { Json, Node, Schema } from '../types'
import { ColourInput, EnumSelect, NumberInput } from './fields'
import { RampBar, type StopJson } from './RampBar'

interface Props {
  schema: Schema
  /** A concrete ramp (stops or sinusoidal), never a reference. */
  ramp: Node
  /** How the texture node uses the ramp beyond its span (for the preview strip). */
  mode: RampMode
  /** Show the ramp without editing controls (library ramps). */
  readOnly?: boolean
  /** `key` identifies the edit for undo coalescing. */
  onChange: (ramp: Node, key: string) => void
}

function stopsOf(ramp: Node): StopJson[] {
  return Array.isArray(ramp.stops) ? (ramp.stops as unknown as StopJson[]) : []
}

export function RampEditor({ schema, ramp, mode, readOnly, onChange }: Props) {
  const [selected, setSelected] = useState(0)
  // References are chosen through the ramp picker, not switched to here.
  const kindOptions = schema.ramp.filter((v) => v.type === 'stops' || v.type === 'sinusoidal').map((v) => ({ value: v.type, label: v.label }))
  if (readOnly) {
    return (
      <div class="ramp-editor">
        <RampBar ramp={ramp} mode={mode} stops={stopsOf(ramp)} readOnly selected={-1} onSelect={() => {}} onChange={() => {}} />
      </div>
    )
  }
  const setStops = (stops: StopJson[], key: string) => onChange({ ...ramp, stops: stops as unknown as Json }, key)

  return (
    <div class="ramp-editor">
      <div class="ramp-header">
        <EnumSelect
          ariaLabel="Ramp kind"
          value={ramp.type}
          options={kindOptions}
          onChange={(type) => onChange(changeType(schema, 'ramp', ramp, type), 'ramp-kind')}
        />
      </div>
      <RampBar ramp={ramp} mode={mode} stops={stopsOf(ramp)} selected={selected} onSelect={setSelected} onChange={setStops} />
      {ramp.type === 'stops' ? (
        <StopsTable stops={stopsOf(ramp)} ramp={ramp} selected={selected} onSelect={setSelected} onChange={setStops} />
      ) : (
        <SinusoidalFields ramp={ramp} onChange={onChange} />
      )}
    </div>
  )
}

interface StopsTableProps {
  ramp: Node
  stops: StopJson[]
  selected: number
  onSelect: (index: number) => void
  onChange: (stops: StopJson[], key: string) => void
}

/**
 * Every stop as a row, in document order. Two stops at the same position (a
 * hard edge) are marked, so they are easy to tell apart and edit.
 */
export function StopsTable({ ramp, stops, selected, onSelect, onChange }: StopsTableProps) {
  const update = (index: number, stop: StopJson, key: string) =>
    onChange(
      stops.map((s, i) => (i === index ? stop : s)),
      key,
    )

  const addStop = () => {
    // Put the new stop in the middle of the widest gap, coloured as the ramp is there.
    const positions = stops.map((s) => s.position).sort((a, b) => a - b)
    let at = positions.length === 0 ? 0 : Math.min(1, positions[positions.length - 1] + 0.1)
    let widest = -1
    for (let i = 1; i < positions.length; i++) {
      const gap = positions[i] - positions[i - 1]
      if (gap > widest) {
        widest = gap
        at = (positions[i] + positions[i - 1]) / 2
      }
    }
    at = Math.round(at * 1000) / 1000
    const colour = formatColour(compileRamp(ramp)(at))
    onChange([...stops, { position: at, colour }], 'add-stop')
    onSelect(stops.length)
  }

  const hardEdge = (index: number) =>
    stops.some((s, i) => i !== index && s.position === stops[index].position)

  return (
    <div class="stops">
      <ul class="stops-list">
        {stops.map((stop, index) => (
          <li
            key={index}
            class={`stop-row ${index === selected ? 'is-selected' : ''}`}
            onFocusIn={() => onSelect(index)}
            onClick={() => onSelect(index)}
          >
            <NumberInput
              ariaLabel={`Stop ${index + 1} position`}
              value={stop.position}
              step={0.01}
              onChange={(position) => update(index, { ...stop, position }, `stop-${index}-position`)}
            />
            <ColourInput
              ariaLabel={`Stop ${index + 1}`}
              value={parseColour(stop.colour)}
              onChange={(colour) => update(index, { ...stop, colour: formatColour(colour) }, `stop-${index}-colour`)}
            />
            <span class={`hard-edge ${hardEdge(index) ? 'is-shown' : ''}`} title="Hard edge: another stop shares this position">
              ⫼
            </span>
            <button
              class="icon-button"
              title="Remove this stop"
              aria-label={`Remove stop ${index + 1}`}
              disabled={stops.length <= 1}
              onClick={(e) => {
                e.stopPropagation()
                onChange(
                  stops.filter((_, i) => i !== index),
                  'remove-stop',
                )
                onSelect(Math.max(0, Math.min(selected, stops.length - 2)))
              }}
            >
              ×
            </button>
          </li>
        ))}
      </ul>
      <div class="stops-actions">
        <button class="button" onClick={addStop}>
          Add stop
        </button>
        <button
          class="button"
          title="Duplicate the selected stop at the same position, making a hard edge"
          disabled={!stops[selected]}
          onClick={() => {
            const stop = stops[selected]
            onChange([...stops.slice(0, selected + 1), { ...stop }, ...stops.slice(selected + 1)], 'hard-edge')
            onSelect(selected + 1)
          }}
        >
          Split into hard edge
        </button>
      </div>
    </div>
  )
}

function SinusoidalFields({ ramp, onChange }: { ramp: Node; onChange: (ramp: Node, key: string) => void }) {
  return (
    <div class="stops-list">
      {(['from', 'to'] as const).map((key) => (
        <div class="stop-row" key={key}>
          <span class="field-label">{key === 'from' ? 'From' : 'To'}</span>
          <ColourInput
            ariaLabel={key}
            value={parseColour(ramp[key])}
            onChange={(colour) => onChange({ ...ramp, [key]: formatColour(colour) }, `sinusoidal-${key}`)}
          />
        </div>
      ))}
    </div>
  )
}
