import { useState } from 'preact/hooks'
import type { StoredRamp } from '../library'
import type { LibraryRamp, Node } from '../types'
import { Dialog } from './Dialog'
import { RampSwatch } from './RampSwatch'

export type RampChoice =
  | { kind: 'named'; name: string }
  | { kind: 'builtin'; id: string }
  | { kind: 'saved'; ramp: StoredRamp }

interface Props {
  documentRamps: Record<string, Node>
  builtins: LibraryRamp[]
  savedRamps: StoredRamp[]
  onPick: (choice: RampChoice) => void
  onDeleteSaved: (id: string) => void
  onClose: () => void
}

const CATEGORY_LABELS: Record<string, string> = {
  natural: 'Natural',
  sky: 'Sky',
  fire: 'Fire',
  water: 'Water',
  scientific: 'Scientific',
  decorative: 'Decorative',
  utility: 'Utility',
}

/** Choose a ramp: this texture's shared ramps, your saved ramps, or the built-in library. */
export function RampPicker({ documentRamps, builtins, savedRamps, onPick, onDeleteSaved, onClose }: Props) {
  const [filter, setFilter] = useState('')
  const matches = (text: string) => text.toLowerCase().includes(filter.trim().toLowerCase())
  const shared = Object.entries(documentRamps).filter(([name]) => matches(name))
  const saved = savedRamps.filter((r) => matches(r.name))
  const categories = Object.keys(CATEGORY_LABELS).concat(builtins.map((b) => b.category).filter((c) => !(c in CATEGORY_LABELS)))
  const grouped = [...new Set(categories)]
    .map((category) => ({
      category,
      ramps: builtins.filter((b) => b.category === category && (matches(b.name) || matches(b.description) || matches(category))),
    }))
    .filter((g) => g.ramps.length > 0)

  return (
    <Dialog title="Choose a ramp" onClose={onClose}>
      <input
        type="text"
        class="picker-filter"
        aria-label="Filter ramps"
        placeholder="Filter by name or category…"
        value={filter}
        ref={(el) => el?.focus()}
        onInput={(e) => setFilter(e.currentTarget.value)}
      />
      {shared.length > 0 && (
        <>
          <h2>In this texture</h2>
          <ul class="ramp-grid">
            {shared.map(([name, ramp]) => (
              <li key={name}>
                <button class="ramp-choice" onClick={() => onPick({ kind: 'named', name })}>
                  <RampSwatch ramp={ramp} />
                  <span>{name}</span>
                </button>
              </li>
            ))}
          </ul>
        </>
      )}
      <h2>My ramps</h2>
      {saved.length === 0 ? (
        <p class="hint">{savedRamps.length === 0 ? 'Save a ramp from the ramp editor to keep it here for any texture.' : 'No matches.'}</p>
      ) : (
        <ul class="ramp-grid">
          {saved.map((r) => (
            <li key={r.id} class="ramp-choice-wrap">
              <button class="ramp-choice" title="Copies this ramp into the texture" onClick={() => onPick({ kind: 'saved', ramp: r })}>
                <RampSwatch ramp={r.ramp} />
                <span>{r.name}</span>
              </button>
              <button class="link-button danger" aria-label={`Delete saved ramp ${r.name}`} onClick={() => onDeleteSaved(r.id)}>
                Delete
              </button>
            </li>
          ))}
        </ul>
      )}
      {grouped.map(({ category, ramps }) => (
        <section key={category}>
          <h2>{CATEGORY_LABELS[category] ?? category}</h2>
          <ul class="ramp-grid">
            {ramps.map((b) => (
              <li key={b.id}>
                <button class="ramp-choice" title={b.description} onClick={() => onPick({ kind: 'builtin', id: b.id })}>
                  <RampSwatch ramp={b.ramp} />
                  <span>{b.name}</span>
                </button>
              </li>
            ))}
          </ul>
        </section>
      ))}
    </Dialog>
  )
}
