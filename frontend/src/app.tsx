import { useCallback, useEffect, useState } from 'preact/hooks'
import { fetchExamples, fetchSchema } from './api'
import { Handles } from './components/Handles'
import { Inspector } from './components/Inspector'
import { OpenDialog } from './components/OpenDialog'
import { Preview } from './components/Preview'
import { TreeView } from './components/TreeView'
import { canRedo, canUndo, createHistory, record, redo, undo, type History } from './history'
import { clone, getAt, pathKey, setAt, type Path } from './tree'
import type { Example, Node, Schema, TextureDocument } from './types'

type Loaded = { schema: Schema; examples: Example[] }

export function App() {
  const [loaded, setLoaded] = useState<Loaded | null>(null)
  const [loadError, setLoadError] = useState<string | null>(null)

  useEffect(() => {
    Promise.all([fetchSchema(), fetchExamples()]).then(
      ([schema, examples]) => setLoaded({ schema, examples }),
      (error) => setLoadError(error instanceof Error ? error.message : String(error)),
    )
  }, [])

  if (loadError) {
    return (
      <div class="fatal">
        <h1>Cannot reach the texture server</h1>
        <p>{loadError}</p>
        <p>
          Start it with <code>stack run texture-server</code>.
        </p>
      </div>
    )
  }
  if (!loaded) return <div class="fatal">Loading…</div>
  return <Editor schema={loaded.schema} examples={loaded.examples} />
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

function Editor({ schema, examples }: Loaded) {
  const [history, setHistory] = useState<History<TextureDocument>>(() =>
    createHistory(clone(examples[0]?.document ?? { version: 1, name: 'Untitled', description: '', texture: schema.defaultTexture })),
  )
  const [rawSelection, setSelection] = useState<Path>([])
  const [collapsed, setCollapsed] = useState<Set<string>>(new Set())
  const [opening, setOpening] = useState(false)

  const document = history.present
  const selection = validPrefix(document.texture, rawSelection)
  const selectedNode = getAt(document.texture, selection)

  const edit = useCallback((change: (doc: TextureDocument) => TextureDocument, key: string | null = null) => {
    setHistory((h) => record(h, change(h.present), key))
  }, [])

  const replaceNode = (path: Path, node: Node, key: string | null) =>
    edit((doc) => ({ ...doc, texture: setAt(doc.texture, path, node) }), key)

  const open = (doc: TextureDocument) => {
    setHistory(createHistory(clone(doc)))
    setSelection([])
    setCollapsed(new Set())
    setOpening(false)
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

  return (
    <div class="app">
      <header class="topbar">
        <h1>Procedural Textures</h1>
        <input
          class="document-name"
          aria-label="Document name"
          value={document.name}
          onInput={(e) => {
            const name = e.currentTarget.value
            edit((doc) => ({ ...doc, name }), 'name')
          }}
        />
        <button class="button" onClick={() => setOpening(true)}>
          Open…
        </button>
        <div class="spacer" />
        <button class="button" disabled={!canUndo(history)} onClick={() => setHistory(undo)} title="Undo (⌘Z)">
          Undo
        </button>
        <button class="button" disabled={!canRedo(history)} onClick={() => setHistory(redo)} title="Redo (⇧⌘Z)">
          Redo
        </button>
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
        {document.description && <p class="caption">{document.description}</p>}
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
      {opening && <OpenDialog examples={examples} onOpen={open} onClose={() => setOpening(false)} />}
    </div>
  )
}
