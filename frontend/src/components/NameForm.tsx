import { useState } from 'preact/hooks'

interface Props {
  label: string
  initial: string
  submitLabel: string
  onSubmit: (name: string) => void
  onCancel: () => void
}

/** A one-line form for naming something, shown in place of the action that asked. */
export function NameForm({ label, initial, submitLabel, onSubmit, onCancel }: Props) {
  const [name, setName] = useState(initial)
  return (
    <form
      class="name-form"
      onSubmit={(e) => {
        e.preventDefault()
        if (name.trim()) onSubmit(name.trim())
      }}
    >
      <input
        type="text"
        aria-label={label}
        placeholder={label}
        value={name}
        ref={(el) => el?.focus()}
        onInput={(e) => setName(e.currentTarget.value)}
        onKeyDown={(e) => {
          if (e.key === 'Escape') {
            e.stopPropagation()
            onCancel()
          }
        }}
      />
      <button class="button primary" type="submit" disabled={!name.trim()}>
        {submitLabel}
      </button>
      <button class="button" type="button" onClick={onCancel}>
        Cancel
      </button>
    </form>
  )
}
