// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen } from '@testing-library/preact'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { ColourInput, NumberInput, ScalarField } from './fields'

afterEach(cleanup)

const box = (label: string) => screen.getByLabelText(label) as HTMLInputElement

describe('NumberInput', () => {
  it('shows a new value from outside (undo) even while focused', () => {
    const onChange = vi.fn()
    const { rerender } = render(<NumberInput ariaLabel="n" value={8} onChange={onChange} />)
    const input = box('n')
    input.focus()
    fireEvent.input(input, { target: { value: '9' } })
    expect(onChange).toHaveBeenLastCalledWith(9)
    rerender(<NumberInput ariaLabel="n" value={9} onChange={onChange} />)
    expect(input.value).toBe('9')
    // Undo puts the document back to 8 while the field still has focus.
    rerender(<NumberInput ariaLabel="n" value={8} onChange={onChange} />)
    expect(input.value).toBe('8')
  })

  it('keeps half-typed text that still means the current value', () => {
    const onChange = vi.fn()
    const { rerender } = render(<NumberInput ariaLabel="n" value={1} onChange={onChange} />)
    const input = box('n')
    input.focus()
    fireEvent.input(input, { target: { value: '0.' } })
    expect(onChange).toHaveBeenLastCalledWith(0)
    rerender(<NumberInput ariaLabel="n" value={0} onChange={onChange} />)
    expect(input.value).toBe('0.')
  })

  it('applies the minimum to typing and to arrow keys', () => {
    const onChange = vi.fn()
    render(<NumberInput ariaLabel="n" value={1} integer min={1} onChange={onChange} />)
    const input = box('n')
    fireEvent.keyDown(input, { key: 'ArrowDown' })
    expect(onChange).not.toHaveBeenCalled()
    expect(input.value).toBe('1')
    fireEvent.input(input, { target: { value: '-4' } })
    expect(onChange).not.toHaveBeenCalled()
  })

  it('nudges with arrow keys, ten steps with Shift', () => {
    const onChange = vi.fn()
    render(<NumberInput ariaLabel="n" value={0.5} step={0.01} onChange={onChange} />)
    fireEvent.keyDown(box('n'), { key: 'ArrowUp' })
    expect(onChange).toHaveBeenLastCalledWith(0.51)
    fireEvent.keyDown(box('n'), { key: 'ArrowDown', shiftKey: true })
    expect(onChange).toHaveBeenLastCalledWith(0.4)
  })
})

describe('ScalarField stepper', () => {
  it('cannot go below the minimum by button or keyboard', () => {
    const onChange = vi.fn()
    render(<ScalarField label="Columns" value={1} min={1} max={64} step={1} integer onChange={onChange} />)
    expect((screen.getByLabelText('Decrease Columns') as HTMLButtonElement).disabled).toBe(true)
    fireEvent.keyDown(screen.getAllByLabelText('Columns')[1], { key: 'ArrowDown' })
    expect(onChange).not.toHaveBeenCalled()
  })
})

describe('ColourInput hex field', () => {
  it('shows a new colour from outside even while focused', () => {
    const onChange = vi.fn()
    const red = { r: 1, g: 0, b: 0, a: 1 }
    const blue = { r: 0, g: 0, b: 1, a: 1 }
    const { rerender } = render(<ColourInput ariaLabel="c" value={red} onChange={onChange} />)
    const hex = box('c hex')
    hex.focus()
    fireEvent.input(hex, { target: { value: '#00ff00ff' } })
    expect(onChange).toHaveBeenCalled()
    rerender(<ColourInput ariaLabel="c" value={blue} onChange={onChange} />)
    expect(hex.value).toBe('#0000ffff')
  })

  it('keeps text that means the current colour', () => {
    const onChange = vi.fn()
    const red = { r: 1, g: 0, b: 0, a: 1 }
    const { rerender } = render(<ColourInput ariaLabel="c" value={red} onChange={onChange} />)
    const hex = box('c hex')
    hex.focus()
    fireEvent.input(hex, { target: { value: 'FF0000' } })
    rerender(<ColourInput ariaLabel="c" value={red} onChange={onChange} />)
    expect(hex.value).toBe('FF0000')
  })
})
