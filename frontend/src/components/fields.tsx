import type { ComponentChildren } from 'preact'
import { useEffect, useState } from 'preact/hooks'
import { fromHex6, parseColour, toCss, toHex6, toHex8, type Rgba } from '../colour'
import { normaliseNumber, parseNumber, textShows } from '../numbers'

/** Compact numeric display; small-step controls request more precision. */
export function formatNumber(value: number, precision = 4): string {
  if (!Number.isFinite(value)) return ''
  const factor=10**precision
  return String(Math.round(value * factor) / factor)
}

interface RowProps {
  label: string
  help?: string
  children: ComponentChildren
}

export function FieldRow({ label, help, children }: RowProps) {
  return (
    <div class="field-row" title={help || undefined}>
      <span class="field-label">{label}</span>
      <div class="field-control">{children}</div>
    </div>
  )
}

interface NumberInputProps {
  value: number
  onChange: (value: number) => void
  integer?: boolean
  min?: number
  step?: number
  ariaLabel?: string
}

/**
 * A text box for a number that tolerates half-typed input ("0.", "-"). The
 * text follows the value whenever it stops meaning it, for example after an
 * undo, even while the box has focus.
 */
export function NumberInput({ value, onChange, integer, min, step, ariaLabel }: NumberInputProps) {
  const rules = { integer, min }
  const precision=step&&step>0?Math.min(12,Math.max(4,Math.ceil(-Math.log10(step)))):4
  const display=(value:number)=>formatNumber(value,precision)
  const [text, setText] = useState(display(value))

  useEffect(() => {
    setText((current) => (textShows(current, value, rules) ? current : display(value)))
    // Only a new value should resync the text; rules come from the field's schema.
  }, [value])

  const propose = (candidate: number) => {
    const next = normaliseNumber(candidate, rules)
    if (next !== value) onChange(next)
    return next
  }

  return (
    <input
      class="number-input"
      type="text"
      inputMode="decimal"
      aria-label={ariaLabel}
      value={text}
      onBlur={() => setText(display(value))}
      onInput={(e) => {
        const raw = e.currentTarget.value
        setText(raw)
        const parsed = parseNumber(raw)
        if (parsed !== null) propose(parsed)
      }}
      onKeyDown={(e) => {
        // Arrow keys nudge by one step (ten with Shift).
        if (e.key !== 'ArrowUp' && e.key !== 'ArrowDown') return
        e.preventDefault()
        const delta = (step ?? 1) * (e.shiftKey ? 10 : 1) * (e.key === 'ArrowUp' ? 1 : -1)
        setText(display(propose(Number(display(value + delta)))))
      }}
    />
  )
}

interface SliderProps {
  value: number
  min: number
  max: number
  step: number
  onChange: (value: number) => void
  ariaLabel?: string
}

/** A range slider. Values outside the range are shown pinned to its ends. */
export function Slider({ value, min, max, step, onChange, ariaLabel }: SliderProps) {
  return (
    <input
      class="slider"
      type="range"
      aria-label={ariaLabel}
      min={min}
      max={max}
      step={step}
      value={Math.min(max, Math.max(min, value))}
      onInput={(e) => onChange(Number(e.currentTarget.value))}
    />
  )
}

interface ScalarProps {
  label: string
  help?: string
  value: number
  min: number
  max: number
  step: number
  integer?: boolean
  onChange: (value: number) => void
}

/** Slider for the usual range plus a text box that can go beyond it. */
export function ScalarField({ label, help, value, min, max, step, integer, onChange }: ScalarProps) {
  return (
    <FieldRow label={label} help={help}>
      <Slider value={value} min={min} max={max} step={step} onChange={onChange} ariaLabel={label} />
      {integer ? (
        <Stepper value={value} min={min} onChange={onChange} ariaLabel={label} />
      ) : (
        <NumberInput value={value} onChange={onChange} step={step} ariaLabel={label} />
      )}
    </FieldRow>
  )
}

/** A whole-number entry with − and + buttons, never going below `min`. */
function Stepper({ value, min, onChange, ariaLabel }: { value: number; min: number; onChange: (value: number) => void; ariaLabel: string }) {
  return (
    <div class="stepper">
      <button class="icon-button" aria-label={`Decrease ${ariaLabel}`} disabled={value <= min} onClick={() => onChange(Math.max(min, value - 1))}>
        −
      </button>
      <NumberInput value={value} onChange={onChange} integer min={min} step={1} ariaLabel={ariaLabel} />
      <button class="icon-button" aria-label={`Increase ${ariaLabel}`} onClick={() => onChange(value + 1)}>
        +
      </button>
    </div>
  )
}

interface PairProps {
  label: string
  help?: string
  value: number[]
  min: number
  max: number
  step: number
  onChange: (value: number[]) => void
}

export function PairField({ label, help, value, min, max, step, onChange }: PairProps) {
  return (
    <div class="field-group" title={help || undefined}>
      <span class="field-label">{label}</span>
      {value.map((component, index) => {
        const axis = ['x', 'y', 'z'][index]
        const change = (v: number) => onChange(value.map((old, i) => i === index ? v : old))
        return <div class="field-row nested" key={axis}>
          <span class="field-label axis">{axis}</span>
          <div class="field-control">
            <Slider value={component} min={min} max={max} step={step} onChange={change} ariaLabel={`${label} ${axis}`} />
            <NumberInput value={component} onChange={change} step={step} ariaLabel={`${label} ${axis}`} />
          </div>
        </div>
      })}
    </div>
  )
}

interface ColourInputProps {
  value: Rgba
  onChange: (value: Rgba) => void
  ariaLabel?: string
}

/** Swatch (over a checkerboard, so transparency shows), native picker, alpha slider and hex entry. */
export function ColourInput({ value, onChange, ariaLabel }: ColourInputProps) {
  const [hex, setHex] = useState(toHex8(value))

  // Follow the value whenever the text stops meaning it (after an undo, say),
  // even while focused; text that already means it, like "FF0000", stays.
  useEffect(() => {
    setHex((current) => {
      const typed = parseHexText(current)
      return typed && toHex8(typed) === toHex8(value) ? current : toHex8(value)
    })
  }, [value])

  return (
    <div class="colour-input">
      <label class="swatch checkerboard" title="Pick a colour">
        <span class="swatch-fill" style={{ background: toCss(value) }} />
        <input
          type="color"
          aria-label={ariaLabel ? `${ariaLabel} colour` : 'Colour'}
          value={toHex6(value)}
          onInput={(e) => onChange(fromHex6(e.currentTarget.value, value.a))}
        />
      </label>
      <input
        class="alpha-slider"
        type="range"
        min={0}
        max={1}
        step={1 / 255}
        aria-label={ariaLabel ? `${ariaLabel} opacity` : 'Opacity'}
        title="Opacity"
        value={value.a}
        style={{ '--alpha-colour': toHex6(value) }}
        onInput={(e) => onChange({ ...value, a: Number(e.currentTarget.value) })}
      />
      <input
        class="hex-input"
        type="text"
        spellcheck={false}
        aria-label={ariaLabel ? `${ariaLabel} hex` : 'Hex'}
        value={hex}
        onBlur={() => setHex(toHex8(value))}
        onInput={(e) => {
          const raw = e.currentTarget.value.trim()
          setHex(raw)
          const colour = parseHexText(raw)
          if (colour) onChange(colour)
        }}
      />
    </div>
  )
}

/** A colour from typed hex ("ff0000", "#FF000080"), or null if incomplete. */
function parseHexText(text: string): Rgba | null {
  const normalised = text.trim().startsWith('#') ? text.trim() : `#${text.trim()}`
  return /^#([0-9a-f]{6}|[0-9a-f]{8})$/i.test(normalised) ? parseColour(normalised.toLowerCase()) : null
}

interface EnumProps {
  value: string
  options: { value: string; label: string }[]
  onChange: (value: string) => void
  ariaLabel?: string
}

export function EnumSelect({ value, options, onChange, ariaLabel }: EnumProps) {
  return (
    <select class="select" aria-label={ariaLabel} value={value} onChange={(e) => onChange(e.currentTarget.value)}>
      {options.map((o) => (
        <option key={o.value} value={o.value}>
          {o.label}
        </option>
      ))}
    </select>
  )
}
