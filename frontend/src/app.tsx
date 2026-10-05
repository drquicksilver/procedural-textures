import { useEffect, useState } from 'preact/hooks'
import { fetchExamples, fetchSchema } from './api'
import { Preview } from './components/Preview'
import { Thumbnail } from './components/Thumbnail'
import type { Example, Schema } from './types'

type Loaded = { schema: Schema; examples: Example[] }

export function App() {
  const [loaded, setLoaded] = useState<Loaded | null>(null)
  const [loadError, setLoadError] = useState<string | null>(null)
  const [selected, setSelected] = useState<string | null>(null)

  useEffect(() => {
    Promise.all([fetchSchema(), fetchExamples()]).then(
      ([schema, examples]) => {
        setLoaded({ schema, examples })
        setSelected(examples[0]?.id ?? null)
      },
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

  const example = loaded.examples.find((e) => e.id === selected) ?? loaded.examples[0]

  return (
    <div class="app">
      <header class="topbar">
        <h1>Procedural Textures</h1>
      </header>
      <aside class="sidebar">
        <h2>Examples</h2>
        <ul class="example-list">
          {loaded.examples.map((e) => (
            <li key={e.id}>
              <button
                class={`example ${e.id === example?.id ? 'is-selected' : ''}`}
                onClick={() => setSelected(e.id)}
                title={e.document.description}
              >
                <Thumbnail document={e.document} />
                <span>{e.document.name}</span>
              </button>
            </li>
          ))}
        </ul>
      </aside>
      <main class="stage">
        {example && (
          <>
            <Preview document={example.document} />
            <p class="caption">{example.document.description}</p>
          </>
        )}
      </main>
    </div>
  )
}
