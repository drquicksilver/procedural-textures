import { describe, expect, it } from 'vitest'
import { Library, MemoryStorage } from './library'
import type { TextureDocument } from './types'

function doc(name: string): TextureDocument {
  return { version: 1, name, description: '', texture: { type: 'flat', colour: '#000000ff' } }
}

function setup() {
  let time = 1000
  let counter = 0
  const storage = new MemoryStorage()
  const library = new Library(
    storage,
    () => (time += 10),
    () => `id-${++counter}`,
  )
  return { storage, library }
}

describe('Library', () => {
  it('saves, lists newest first, and gets documents', () => {
    const { library } = setup()
    const a = library.save(doc('A'))
    const b = library.save(doc('B'))
    expect(library.list().map((e) => e.document.name)).toEqual(['B', 'A'])
    expect(library.get(a)?.document.name).toBe('A')
    library.save(doc('A2'), a)
    expect(library.list().map((e) => e.id)).toEqual([a, b])
  })

  it('renames, duplicates and removes', () => {
    const { library } = setup()
    const a = library.save(doc('A'))
    library.rename(a, 'Renamed')
    expect(library.get(a)?.document.name).toBe('Renamed')
    const copy = library.duplicate(a)!
    expect(library.get(copy)?.document.name).toBe('Renamed copy')
    library.remove(a)
    expect(library.list().map((e) => e.id)).toEqual([copy])
  })

  it('copies an edited example into the library, then saves in place', () => {
    const { library } = setup()
    const first = library.commit({ source: { kind: 'example', id: 'marble' }, document: doc('Marble') })
    expect(first.kind).toBe('library')
    expect(library.list()).toHaveLength(1)
    const second = library.commit({ source: first, document: doc('Marble edited') })
    expect(second).toEqual(first)
    expect(library.list().map((e) => e.document.name)).toEqual(['Marble edited'])
    expect(library.loadWorking()?.document.name).toBe('Marble edited')
  })

  it('re-creates a library entry that was deleted while open', () => {
    const { library } = setup()
    const source = library.commit({ source: { kind: 'example', id: 'x' }, document: doc('X') })
    if (source.kind === 'library') library.remove(source.id)
    library.commit({ source, document: doc('X again') })
    expect(library.list().map((e) => e.document.name)).toEqual(['X again'])
  })

  it('survives corrupt storage', () => {
    const { storage, library } = setup()
    storage.setItem('procedural-textures.library.v1', '{not json')
    storage.setItem('procedural-textures.working.v1', '"nope"')
    expect(library.list()).toEqual([])
    expect(library.loadWorking()).toBeNull()
    storage.setItem('procedural-textures.library.v1', JSON.stringify({ a: { id: 'a', updatedAt: 1, document: { name: 'no texture' } } }))
    expect(library.list()).toEqual([])
  })
})
