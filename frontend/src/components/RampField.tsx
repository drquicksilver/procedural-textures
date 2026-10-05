import { useState } from 'preact/hooks'
import type { StoredRamp } from '../library'
import {
  detachRamp,
  renameNamedRamp,
  resolveRamp,
  setNamedRamp,
  shareRamp,
  sourcesOf,
  useBuiltin,
  useCopiedRamp,
  useNamed,
  usageCount,
} from '../rampRefs'
import { asMode } from '../ramp'
import { getAt, isNode, pathKey, setAt, type Path } from '../tree'
import type { Field, LibraryRamp, Node, Schema, TextureDocument } from '../types'
import { NameForm } from './NameForm'
import { RampEditor } from './RampEditor'
import { RampPicker, type RampChoice } from './RampPicker'

/** What a ramp field needs beyond its own node: the whole document and the ramp libraries. */
export interface RampContext {
  document: TextureDocument
  builtins: LibraryRamp[]
  savedRamps: StoredRamp[]
  /** Change the document as one undoable edit; `key` coalesces repeated edits. */
  editDocument: (change: (document: TextureDocument) => TextureDocument, key: string | null) => void
  saveRamp: (name: string, ramp: Node) => void
  deleteSavedRamp: (id: string) => void
}

interface Props {
  schema: Schema
  context: RampContext
  path: Path
  field: Field
}

type Naming = null | 'share' | 'save' | 'rename'

/**
 * A ramp field: shows where the ramp comes from (local, shared within the
 * texture, or the built-in library), offers the matching actions, and edits
 * it in place. Editing a shared ramp changes every use of it.
 */
export function RampField({ schema, context, path, field }: Props) {
  const { document, builtins, editDocument } = context
  const [picking, setPicking] = useState(false)
  const [naming, setNaming] = useState<Naming>(null)
  const ramp = getAt(document.texture, path)?.[field.key]
  if (!isNode(ramp)) return null

  const key = `${pathKey(path)}:${field.key}`
  const mode = asMode(getAt(document.texture, path)?.mode)
  const resolved = resolveRamp(ramp, sourcesOf(document, builtins))
  const builtin = ramp.type === 'builtin' ? builtins.find((b) => b.id === ramp.name) : undefined
  const name = String(ramp.name ?? '')
  const uses = ramp.type === 'named' ? usageCount(schema, document, name) : 0

  const pick = (choice: RampChoice) => {
    setPicking(false)
    editDocument((d) => {
      switch (choice.kind) {
        case 'named':
          return useNamed(d, path, field.key, choice.name)
        case 'builtin':
          return useBuiltin(d, path, field.key, choice.id)
        case 'saved':
          return useCopiedRamp(d, path, field.key, choice.ramp.name, choice.ramp.ramp)
      }
    }, null)
  }

  const nameForm = (() => {
    switch (naming) {
      case 'share':
        return (
          <NameForm
            label="Name for the shared ramp"
            initial={field.label.toLowerCase()}
            submitLabel="Share"
            onCancel={() => setNaming(null)}
            onSubmit={(n) => {
              editDocument((d) => shareRamp(d, path, field.key, n), null)
              setNaming(null)
            }}
          />
        )
      case 'rename':
        return (
          <NameForm
            label="New name"
            initial={name}
            submitLabel="Rename"
            onCancel={() => setNaming(null)}
            onSubmit={(n) => {
              editDocument((d) => renameNamedRamp(schema, d, name, n), null)
              setNaming(null)
            }}
          />
        )
      case 'save':
        return (
          <NameForm
            label="Name in My ramps"
            initial={builtin?.name ?? (ramp.type === 'named' ? name : document.name)}
            submitLabel="Save"
            onCancel={() => setNaming(null)}
            onSubmit={(n) => {
              if (resolved) context.saveRamp(n, resolved)
              setNaming(null)
            }}
          />
        )
      default:
        return null
    }
  })()

  return (
    <div class="field-group ramp-field">
      <div class="ramp-source">
        <span class="field-label">{field.label}</span>
        <span class="ramp-origin">
          {ramp.type === 'named' ? (
            <>
              Shared ramp <strong>{name}</strong>
              <span class="muted">{uses > 1 ? ` · used in ${uses} places` : ' · used only here'}</span>
            </>
          ) : builtin ? (
            <>
              Library ramp <strong>{builtin.name}</strong>
            </>
          ) : ramp.type === 'builtin' || !resolved ? (
            <span class="warning">Missing ramp “{name}”</span>
          ) : (
            'Custom ramp'
          )}
        </span>
      </div>
      <div class="ramp-actions">
        <button class="button" onClick={() => setPicking(true)}>
          Choose…
        </button>
        {ramp.type === 'stops' || ramp.type === 'sinusoidal' ? (
          <button class="button" title="Define this ramp once in the texture, so other nodes can use it" onClick={() => setNaming('share')}>
            Share…
          </button>
        ) : null}
        {ramp.type === 'named' && (
          <button class="button" onClick={() => setNaming('rename')}>
            Rename…
          </button>
        )}
        {(ramp.type === 'named' || ramp.type === 'builtin') && resolved && (
          <button
            class="button"
            title={ramp.type === 'builtin' ? 'Make an editable copy here' : 'Use a separate copy here, leaving the shared ramp alone'}
            onClick={() => editDocument((d) => detachRamp(d, path, field.key, builtins), null)}
          >
            {ramp.type === 'builtin' ? 'Customise' : 'Detach'}
          </button>
        )}
        {resolved && (
          <button class="button" title="Keep a copy in My ramps, to use in other textures" onClick={() => setNaming('save')}>
            Save to My ramps…
          </button>
        )}
      </div>
      {nameForm}
      {builtin && <p class="description">{builtin.description}</p>}
      {resolved &&
        (ramp.type === 'builtin' ? (
          <RampEditor schema={schema} ramp={resolved} mode={mode} readOnly onChange={() => {}} />
        ) : ramp.type === 'named' ? (
          <RampEditor schema={schema} ramp={resolved} mode={mode} onChange={(r, k) => editDocument((d) => setNamedRamp(d, name, r), `ramp:${name}:${k}`)} />
        ) : (
          <RampEditor
            schema={schema}
            ramp={ramp}
            mode={mode}
            onChange={(r, k) =>
              editDocument((d) => {
                const node = getAt(d.texture, path)
                return node ? { ...d, texture: setAt(d.texture, path, { ...node, [field.key]: r }) } : d
              }, `${key}:${k}`)
            }
          />
        ))}
      {picking && (
        <RampPicker
          documentRamps={document.ramps ?? {}}
          builtins={builtins}
          savedRamps={context.savedRamps}
          onPick={pick}
          onDeleteSaved={context.deleteSavedRamp}
          onClose={() => setPicking(false)}
        />
      )}
    </div>
  )
}
