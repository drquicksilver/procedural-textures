import { useState } from 'preact/hooks'
import type { StoredDocument } from '../library'
import type { Example, TextureDocument } from '../types'
import { Dialog } from './Dialog'
import { Thumbnail } from './Thumbnail'
import metadata from '../metadata'

interface Props {
  documents: StoredDocument[]
  examples: Example[]
  openId: string | null
  onOpenDocument: (entry: StoredDocument) => void
  onOpenExample: (example: Example) => void
  onNew: () => void
  onImport: () => void
  onRename: (id: string, name: string) => void
  onDuplicate: (id: string) => void
  onDelete: (id: string) => void
  onClose: () => void
}

const KNOWN_CATEGORIES = metadata.exampleCategories.map((category) => category.id)

/** Examples grouped by category: known ones in order, then others, then uncategorised. */
export function groupByCategory(examples: Example[]): { heading: string; members: Example[] }[] {
  const category = (e: Example) => e.document.category ?? ''
  const others = [...new Set(examples.map(category))].filter((c) => c && !KNOWN_CATEGORIES.includes(c)).sort()
  return [...KNOWN_CATEGORIES, ...others, '']
    .map((c) => ({
      heading: metadata.exampleCategories.find((category) => category.id === c)?.label ?? (c ? c[0].toUpperCase() + c.slice(1) : 'Other'),
      members: sortExamples(examples.filter((e) => category(e) === c)),
    }))
    .filter((g) => g.members.length > 0)
}

export function sortExamples(examples: Example[]): Example[] {
  const rank = (e: Example) => e.document.guide?.order ?? 100
  const family = (e: Example) => e.document.guide?.family ?? ''
  const first = (e: Example) => family(e) ? Math.min(...examples.filter((other) => family(other) === family(e)).map(rank)) : rank(e)
  return [...examples].sort((a, b) => first(a) - first(b) || family(a).localeCompare(family(b)) || rank(a) - rank(b))
}

export function groupByFamily(examples: Example[]): { family: string; members: Example[] }[] {
  const groups: { family: string; members: Example[] }[] = []
  for (const example of examples) {
    const family = example.document.guide?.family ?? ''
    if (groups.at(-1)?.family === family) groups.at(-1)!.members.push(example)
    else groups.push({ family, members: [example] })
  }
  return groups
}

export function matchesExample(example: Example, query: string): boolean {
  const { name, description, guide } = example.document
  const haystack = [name, description, guide?.family, guide?.hint, ...(guide?.tags ?? [])].join(' ').toLocaleLowerCase()
  return query.toLocaleLowerCase().trim().split(/\s+/).every((word) => haystack.includes(word))
}

const ROLE_LABELS = { preset: 'Material preset', study: 'Minimal study', comparison: 'Controlled comparison', composition: 'Composition study' }

function when(time: number): string {
  const minutes = Math.round((Date.now() - time) / 60000)
  if (minutes < 1) return 'just now'
  if (minutes < 60) return `${minutes} min ago`
  const hours = Math.round(minutes / 60)
  if (hours < 24) return `${hours} h ago`
  return new Date(time).toLocaleDateString()
}

export function LibraryDialog(props: Props) {
  const { documents, examples, onClose } = props
  const [query, setQuery] = useState('')
  const filtered = examples.filter((example) => matchesExample(example, query))
  const cards = (members: Example[]) => <ul class="document-grid">{members.map((example) => (
    <li key={example.id}><DocumentButton document={example.document} onClick={() => props.onOpenExample(example)} /></li>
  ))}</ul>
  return (
    <Dialog title="Library" onClose={onClose}>
      <div class="dialog-actions">
        <button class="button primary" onClick={props.onNew}>
          New blank texture
        </button>
        <button class="button" onClick={props.onImport}>
          Import JSON…
        </button>
      </div>
      <h2>Your textures</h2>
      {documents.length === 0 ? (
        <p class="empty">Nothing here yet. Edit an example or start a new texture and it is saved here automatically.</p>
      ) : (
        <ul class="document-grid">
          {documents.map((entry) => (
            <LibraryCard key={entry.id} entry={entry} isOpen={entry.id === props.openId} {...props} />
          ))}
        </ul>
      )}
      <h2>Examples</h2>
      <p class="hint">Examples are read-only: editing one saves a copy to your textures.</p>
      <label class="example-search">Search examples <input type="search" aria-label="Search examples" placeholder="Name, subject or capability…" value={query} onInput={(event) => setQuery(event.currentTarget.value)} /></label>
      {filtered.length === 0 && <p class="empty">No examples match this search.</p>}
      {groupByCategory(filtered).map(({ heading, members }) => (
        <section key={heading} class="example-group">
          <h3>{heading}</h3>
          {groupByFamily(members).map(({ family, members }) => family ? (
            <details key={family} class="example-family" open={query.trim() !== ''}>
              <summary>{family} · {members.length} examples</summary>{cards(members)}
            </details>
          ) : cards(members))}
        </section>
      ))}
    </Dialog>
  )
}

function DocumentButton({ document, detail, onClick }: { document: TextureDocument; detail?: string; onClick: () => void }) {
  return (
    <button class="document-card" title={[document.description, document.guide?.hint].filter(Boolean).join('\n') || undefined} onClick={onClick}>
      <Thumbnail texture={document.texture} ramps={document.ramps} preview={document.guide?.preview} />
      <span class="document-title">{document.name || 'Untitled'}</span>
      {document.guide && <span class="document-detail">{ROLE_LABELS[document.guide.role]}</span>}
      {document.guide?.tags.length ? <span class="document-tags">{document.guide.tags.join(' · ')}</span> : null}
      {detail && <span class="document-detail">{detail}</span>}
    </button>
  )
}

function LibraryCard({ entry, isOpen, onOpenDocument, onRename, onDuplicate, onDelete }: Props & { entry: StoredDocument; isOpen: boolean }) {
  const [mode, setMode] = useState<'idle' | 'renaming' | 'confirming'>('idle')
  const [name, setName] = useState(entry.document.name)

  return (
    <li class={`library-card ${isOpen ? 'is-open' : ''}`}>
      {mode === 'renaming' ? (
        <form
          class="rename-form"
          onSubmit={(e) => {
            e.preventDefault()
            onRename(entry.id, name.trim() || entry.document.name)
            setMode('idle')
          }}
        >
          <Thumbnail texture={entry.document.texture} ramps={entry.document.ramps} />
          <input
            type="text"
            aria-label="New name"
            value={name}
            ref={(el) => el?.focus()}
            onInput={(e) => setName(e.currentTarget.value)}
            onKeyDown={(e) => {
              if (e.key === 'Escape') {
                e.stopPropagation()
                setMode('idle')
              }
            }}
            onBlur={() => setMode('idle')}
          />
        </form>
      ) : (
        <DocumentButton
          document={entry.document}
          detail={isOpen ? 'open now' : when(entry.updatedAt)}
          onClick={() => onOpenDocument(entry)}
        />
      )}
      <div class="card-actions">
        {mode === 'confirming' ? (
          <>
            <span>Delete?</span>
            <button class="link-button danger" onClick={() => onDelete(entry.id)}>
              Yes
            </button>
            <button class="link-button" onClick={() => setMode('idle')}>
              No
            </button>
          </>
        ) : (
          <>
            <button
              class="link-button"
              onClick={() => {
                setName(entry.document.name)
                setMode('renaming')
              }}
            >
              Rename
            </button>
            <button class="link-button" onClick={() => onDuplicate(entry.id)}>
              Duplicate
            </button>
            <button class="link-button danger" onClick={() => setMode('confirming')}>
              Delete
            </button>
          </>
        )}
      </div>
    </li>
  )
}
