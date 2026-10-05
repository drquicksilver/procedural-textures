import { useState } from 'preact/hooks'
import type { StoredDocument } from '../library'
import type { Example, TextureDocument } from '../types'
import { Dialog } from './Dialog'
import { Thumbnail } from './Thumbnail'

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
      <ul class="document-grid">
        {examples.map((example) => (
          <li key={example.id}>
            <DocumentButton document={example.document} onClick={() => props.onOpenExample(example)} />
          </li>
        ))}
      </ul>
    </Dialog>
  )
}

function DocumentButton({ document, detail, onClick }: { document: TextureDocument; detail?: string; onClick: () => void }) {
  return (
    <button class="document-card" title={document.description || undefined} onClick={onClick}>
      <Thumbnail texture={document.texture} ramps={document.ramps} />
      <span class="document-title">{document.name || 'Untitled'}</span>
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
