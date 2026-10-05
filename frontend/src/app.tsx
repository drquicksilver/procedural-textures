import { useCallback, useEffect, useRef, useState } from 'preact/hooks'
import { fetchExamples, fetchSchema, migrateDocument } from './api'
import { Handles } from './components/Handles'
import { Inspector } from './components/Inspector'
import { LibraryDialog } from './components/LibraryDialog'
import { Preview } from './components/Preview'
import { TreeView } from './components/TreeView'
import { canRedo, canUndo, createHistory, record, redo, undo, type History } from './history'
import { browserStorage, Library, type Source, type StoredDocument, type WorkingState } from './library'
import { clone, getAt, pathKey, setAt, type Path } from './tree'
import type { Example, Node, Schema, TextureDocument } from './types'
import { currentVersion } from './version'

interface Loaded {
  schema: Schema
  examples: Example[]
  library: Library
  persistent: boolean
  initial: WorkingState
  notice: string | null
}

function message(error: unknown): string {
  return error instanceof Error ? error.message : String(error)
}

/** Bring a stored or imported document up to date via the server, if needed. */
async function upToDate(document: unknown): Promise<TextureDocument> {
  const d = document as Partial<TextureDocument> | null
  if (d && d.version === currentVersion && typeof d.name === 'string' && d.texture) return d as TextureDocument
  return migrateDocument(document)
}

async function start(): Promise<Loaded> {
  const [schema, examples] = await Promise.all([fetchSchema(), fetchExamples()])
  const { storage, persistent } = browserStorage()
  const library = new Library(storage)
  const fallback: WorkingState = examples[0]
    ? { source: { kind: 'example', id: examples[0].id }, document: examples[0].document }
    : { source: { kind: 'library', id: library.save(blankDocument(schema)) }, document: blankDocument(schema) }
  const working = library.loadWorking()
  let initial = fallback
  let notice: string | null = null
  if (working) {
    try {
      initial = { source: working.source, document: await upToDate(working.document) }
    } catch (error) {
      notice = `Could not restore your last texture: ${message(error)}`
    }
  }
  return { schema, examples, library, persistent, initial, notice }
}

function blankDocument(schema: Schema): TextureDocument {
  return { version: currentVersion, name: 'Untitled', description: '', texture: clone(schema.defaultTexture) }
}

export function App() {
  const [loaded, setLoaded] = useState<Loaded | null>(null)
  const [loadError, setLoadError] = useState<string | null>(null)

  useEffect(() => {
    start().then(setLoaded, (error) => setLoadError(message(error)))
  }, [])

  if (loadError) {
    return (
      <div class="fatal">
        <h1>Cannot reach the texture server</h1>
        <p>{loadError}</p>
        <p>
          Start it with <code>make app</code> (or <code>stack run texture-server</code>).
        </p>
      </div>
    )
  }
  if (!loaded) return <div class="fatal">Loading…</div>
  return <Editor {...loaded} />
}

/** The longest prefix of `path` that still leads to a node. */
function validPrefix(root: Node, path: Path): Path {
  let valid: Path = []
  for (let i = 1; i <= path.length; i++) {
    if (!getAt(root, path.slice(0, i))) break
    valid = path.slice(0, i)
  }
  return valid
}

function downloadJson(document: TextureDocument): void {
  const slug = document.name.trim().toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '') || 'texture'
  const url = URL.createObjectURL(new Blob([JSON.stringify(document, null, 2) + '\n'], { type: 'application/json' }))
  const link = Object.assign(window.document.createElement('a'), { href: url, download: `${slug}.json` })
  link.click()
  setTimeout(() => URL.revokeObjectURL(url), 1000)
}

const AUTOSAVE_MS = 400

function Editor({ schema, examples, library, persistent, initial, notice }: Loaded) {
  const [history, setHistory] = useState<History<TextureDocument>>(() => createHistory(initial.document))
  const [source, setSource] = useState<Source>(initial.source)
  const [rawSelection, setSelection] = useState<Path>([])
  const [collapsed, setCollapsed] = useState<Set<string>>(new Set())
  const [libraryOpen, setLibraryOpen] = useState(false)
  const [documents, setDocuments] = useState<StoredDocument[]>(() => library.list())
  const [pending, setPending] = useState(false)
  const [toast, setToast] = useState<string | null>(notice)
  const importInput = useRef<HTMLInputElement>(null)

  const document = history.present
  const selection = validPrefix(document.texture, rawSelection)
  const selectedNode = getAt(document.texture, selection)

  // Autosave: the last saved document, and the latest state for flushing.
  const saved = useRef<TextureDocument>(initial.document)
  const latest = useRef({ source, document })
  latest.current = { source, document }

  const refresh = () => setDocuments(library.list())

  const showError = (text: string) => setToast(text)

  const flush = useCallback(() => {
    const { source: currentSource, document: current } = latest.current
    if (current === saved.current) return
    try {
      const nextSource = library.commit({ source: currentSource, document: current })
      saved.current = current
      latest.current = { source: nextSource, document: current }
      setSource(nextSource)
      setDocuments(library.list())
    } catch (error) {
      setToast(`Could not save: ${message(error)}`)
    }
    setPending(false)
  }, [library])

  useEffect(() => {
    if (document === saved.current) return
    setPending(true)
    const timer = setTimeout(flush, AUTOSAVE_MS)
    return () => clearTimeout(timer)
  }, [document, flush])

  // Save before the page goes away, so a reload loses nothing.
  useEffect(() => {
    window.addEventListener('pagehide', flush)
    return () => window.removeEventListener('pagehide', flush)
  }, [flush])

  // Remember the open document even before it is edited.
  useEffect(() => {
    try {
      library.saveWorking({ source: initial.source, document: initial.document })
    } catch {
      // Storage full or unavailable; edits will report it.
    }
  }, [library, initial])

  useEffect(() => {
    if (!toast) return
    const timer = setTimeout(() => setToast(null), 6000)
    return () => clearTimeout(timer)
  }, [toast])

  const edit = useCallback((change: (doc: TextureDocument) => TextureDocument, key: string | null = null) => {
    setHistory((h) => record(h, change(h.present), key))
  }, [])

  const replaceNode = (path: Path, node: Node, key: string | null) =>
    edit((doc) => ({ ...doc, texture: setAt(doc.texture, path, node) }), key)

  const open = (doc: TextureDocument, nextSource: Source) => {
    flush()
    saved.current = doc
    setHistory(createHistory(doc))
    setSource(nextSource)
    setSelection([])
    setCollapsed(new Set())
    setLibraryOpen(false)
    try {
      library.saveWorking({ source: nextSource, document: doc })
    } catch (error) {
      showError(`Could not save: ${message(error)}`)
    }
  }

  const openExample = (example: Example) => open(clone(example.document), { kind: 'example', id: example.id })

  const openStored = async (entry: StoredDocument) => {
    try {
      const doc = await upToDate(entry.document)
      if (doc !== entry.document) library.save(doc, entry.id)
      open(doc, { kind: 'library', id: entry.id })
    } catch (error) {
      showError(`Could not open “${entry.document.name}”: ${message(error)}`)
    }
  }

  const newBlank = () => {
    const doc = blankDocument(schema)
    const id = library.save(doc)
    refresh()
    open(doc, { kind: 'library', id })
  }

  const importFile = async (file: File) => {
    try {
      // Always validate imports on the server, whatever their version says.
      const doc = await migrateDocument(JSON.parse(await file.text()))
      const id = library.save(doc)
      refresh()
      open(doc, { kind: 'library', id })
    } catch (error) {
      showError(`Could not import ${file.name}: ${message(error)}`)
    }
  }

  const isOpen = (id: string) => source.kind === 'library' && source.id === id

  const rename = (id: string, name: string) => {
    if (isOpen(id)) edit((doc) => ({ ...doc, name }))
    else library.rename(id, name)
    refresh()
  }

  const remove = (id: string) => {
    library.remove(id)
    refresh()
    if (isOpen(id)) {
      saved.current = document // nothing left to save for the deleted document
      if (examples[0]) openExample(examples[0])
      else newBlank()
      setLibraryOpen(true)
    }
  }

  // Cmd/Ctrl+Z undoes, Shift+Cmd/Ctrl+Z or Ctrl+Y redoes, everywhere: every
  // keystroke in the editor's own fields is already an undoable edit.
  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      if (!(e.metaKey || e.ctrlKey)) return
      const key = e.key.toLowerCase()
      if (key === 'z') {
        e.preventDefault()
        setHistory((h) => (e.shiftKey ? redo(h) : undo(h)))
      } else if (key === 'y' && e.ctrlKey) {
        e.preventDefault()
        setHistory(redo)
      }
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [])

  const toggle = (path: Path) =>
    setCollapsed((c) => {
      const next = new Set(c)
      const key = pathKey(path)
      if (next.has(key)) next.delete(key)
      else next.add(key)
      return next
    })

  const status = !persistent
    ? { text: 'Not saved', title: 'This browser is not allowing local storage, so nothing will be kept.' }
    : pending
      ? { text: 'Saving…', title: '' }
      : source.kind === 'example'
        ? { text: 'Example', title: 'Examples are read-only: your first edit saves a copy to your textures.' }
        : { text: 'Saved', title: 'Saved in this browser.' }

  return (
    <div class="app">
      <header class="topbar">
        <h1>Procedural Textures</h1>
        <button class="button" onClick={() => setLibraryOpen(true)}>
          Library…
        </button>
        <button class="button" onClick={newBlank}>
          New
        </button>
        <input
          class="document-name"
          aria-label="Document name"
          value={document.name}
          onInput={(e) => {
            const name = e.currentTarget.value
            edit((doc) => ({ ...doc, name }), 'name')
          }}
        />
        <span class={`save-status ${persistent ? '' : 'is-warning'}`} title={status.title}>
          {status.text}
        </span>
        <div class="spacer" />
        {toast && (
          <div class="toast" role="alert">
            {toast}
            <button class="icon-button" aria-label="Dismiss" onClick={() => setToast(null)}>
              ×
            </button>
          </div>
        )}
        <button class="button" disabled={!canUndo(history)} onClick={() => setHistory(undo)} title="Undo (⌘Z)">
          Undo
        </button>
        <button class="button" disabled={!canRedo(history)} onClick={() => setHistory(redo)} title="Redo (⇧⌘Z)">
          Redo
        </button>
        <button class="button" onClick={() => importInput.current?.click()} title="Open a texture JSON file">
          Import
        </button>
        <button class="button" onClick={() => downloadJson(document)} title="Download this texture as JSON">
          Export
        </button>
        <input
          ref={importInput}
          type="file"
          accept=".json,application/json"
          hidden
          onChange={(e) => {
            const file = e.currentTarget.files?.[0]
            e.currentTarget.value = ''
            if (file) void importFile(file)
          }}
        />
      </header>
      <aside class="sidebar">
        <h2>Structure</h2>
        <TreeView
          schema={schema}
          root={document.texture}
          selection={selection}
          collapsed={collapsed}
          onSelect={setSelection}
          onToggle={toggle}
        />
      </aside>
      <main class="stage">
        <Preview
          document={document}
          overlay={
            selectedNode && (
              <Handles
                schema={schema}
                node={selectedNode}
                onChange={(field, value) => replaceNode(selection, { ...selectedNode, [field]: value }, `${pathKey(selection)}:${field}`)}
              />
            )
          }
        />
        <textarea
          class="caption-input"
          aria-label="Description"
          placeholder="Add a description…"
          rows={2}
          value={document.description}
          onInput={(e) => {
            const description = e.currentTarget.value
            edit((doc) => ({ ...doc, description }), 'description')
          }}
        />
      </main>
      <aside class="inspector">
        <Inspector
          schema={schema}
          root={document.texture}
          path={selection}
          examples={examples}
          onReplace={replaceNode}
          onSelect={setSelection}
        />
      </aside>
      {libraryOpen && (
        <LibraryDialog
          documents={documents}
          examples={examples}
          openId={source.kind === 'library' ? source.id : null}
          onOpenDocument={(entry) => void openStored(entry)}
          onOpenExample={openExample}
          onNew={newBlank}
          onImport={() => importInput.current?.click()}
          onRename={rename}
          onDuplicate={(id) => {
            library.duplicate(id)
            refresh()
          }}
          onDelete={remove}
          onClose={() => setLibraryOpen(false)}
        />
      )}
    </div>
  )
}
