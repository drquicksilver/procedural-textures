// The user's texture library and the working document, kept in local
// storage. Storage is injected so the logic can be tested without a browser.

import type { Node, TextureDocument } from './types'

export interface StoredDocument {
  id: string
  document: TextureDocument
  updatedAt: number
}

/** Where the open document came from, which decides how edits are saved. */
export type Source =
  | { kind: 'library'; id: string }
  | { kind: 'example'; id: string }

export type LibrarySource = Extract<Source, { kind: 'library' }>

/** One of your saved ramps. Using it copies it into the document. */
export interface StoredRamp {
  id: string
  name: string
  ramp: Node
  updatedAt: number
}

export interface WorkingState {
  source: Source
  document: TextureDocument
}

export type StorageLike = Pick<Storage, 'getItem' | 'setItem' | 'removeItem'>

const LIBRARY_KEY = 'procedural-textures.library.v1'
const WORKING_KEY = 'procedural-textures.working.v1'
const RAMPS_KEY = 'procedural-textures.ramps.v1'

function isDocument(value: unknown): value is TextureDocument {
  if (typeof value !== 'object' || value === null) return false
  const d = value as Record<string, unknown>
  return typeof d.version === 'number' && typeof d.name === 'string' && typeof d.texture === 'object' && d.texture !== null
}

function readJson(storage: StorageLike, key: string): unknown {
  try {
    const raw = storage.getItem(key)
    return raw === null ? null : JSON.parse(raw)
  } catch {
    return null
  }
}

export class Library {
  private readonly storage: StorageLike
  private readonly now: () => number
  private readonly newId: () => string

  constructor(storage: StorageLike, now: () => number = Date.now, newId: () => string = () => crypto.randomUUID()) {
    this.storage = storage
    this.now = now
    this.newId = newId
  }

  /** Every stored document, most recently changed first. Corrupt entries are skipped. */
  list(): StoredDocument[] {
    const raw = readJson(this.storage, LIBRARY_KEY)
    if (typeof raw !== 'object' || raw === null || Array.isArray(raw)) return []
    return Object.values(raw as Record<string, unknown>)
      .filter((entry): entry is StoredDocument => {
        const e = entry as StoredDocument
        return typeof e?.id === 'string' && typeof e.updatedAt === 'number' && isDocument(e.document)
      })
      .sort((a, b) => b.updatedAt - a.updatedAt)
  }

  get(id: string): StoredDocument | undefined {
    return this.list().find((entry) => entry.id === id)
  }

  /** Store a document, under a new id unless one is given. Returns the id. */
  save(document: TextureDocument, id: string = this.newId()): string {
    const entries = Object.fromEntries(this.list().map((e) => [e.id, e]))
    entries[id] = { id, document, updatedAt: this.now() }
    this.storage.setItem(LIBRARY_KEY, JSON.stringify(entries))
    return id
  }

  rename(id: string, name: string): void {
    const entry = this.get(id)
    if (entry) this.save({ ...entry.document, name }, id)
  }

  duplicate(id: string): string | undefined {
    const entry = this.get(id)
    return entry ? this.save({ ...entry.document, name: `${entry.document.name} copy` }) : undefined
  }

  remove(id: string): void {
    const entries = Object.fromEntries(this.list().filter((e) => e.id !== id).map((e) => [e.id, e]))
    this.storage.setItem(LIBRARY_KEY, JSON.stringify(entries))
  }

  /** Your saved ramps, most recently changed first. Corrupt entries are skipped. */
  listRamps(): StoredRamp[] {
    const raw = readJson(this.storage, RAMPS_KEY)
    if (typeof raw !== 'object' || raw === null || Array.isArray(raw)) return []
    return Object.values(raw as Record<string, unknown>)
      .filter((entry): entry is StoredRamp => {
        const e = entry as StoredRamp
        return typeof e?.id === 'string' && typeof e.name === 'string' && typeof e.updatedAt === 'number' && typeof e.ramp?.type === 'string'
      })
      // Ramps saved before document version 3 carried a mode; it now belongs to each use.
      .map((e) => {
        if (!('mode' in e.ramp)) return e
        const { mode: _mode, ...ramp } = e.ramp
        return { ...e, ramp: ramp as Node }
      })
      .sort((a, b) => b.updatedAt - a.updatedAt)
  }

  saveRamp(name: string, ramp: Node, id: string = this.newId()): string {
    const entries = Object.fromEntries(this.listRamps().map((e) => [e.id, e]))
    entries[id] = { id, name, ramp, updatedAt: this.now() }
    this.storage.setItem(RAMPS_KEY, JSON.stringify(entries))
    return id
  }

  renameRamp(id: string, name: string): void {
    const entry = this.listRamps().find((e) => e.id === id)
    if (entry) this.saveRamp(name, entry.ramp, id)
  }

  removeRamp(id: string): void {
    const entries = Object.fromEntries(this.listRamps().filter((e) => e.id !== id).map((e) => [e.id, e]))
    this.storage.setItem(RAMPS_KEY, JSON.stringify(entries))
  }

  loadWorking(): WorkingState | null {
    const raw = readJson(this.storage, WORKING_KEY) as WorkingState | null
    if (!raw || !isDocument(raw.document)) return null
    const source = raw.source
    if (!source || (source.kind !== 'library' && source.kind !== 'example') || typeof source.id !== 'string') return null
    return raw
  }

  saveWorking(state: WorkingState): void {
    this.storage.setItem(WORKING_KEY, JSON.stringify(state))
  }

  /** Allocate a save's identity before writing; retain it across failed retries. */
  prepareCommit(source: Source): LibrarySource {
    return source.kind === 'library' ? source : { kind: 'library', id: this.newId() }
  }

  /**
   * Save an edit under the identity from prepareCommit. Returns only after
   * both writes succeed; retry with the same identity and the latest document.
   */
  commit(state: { source: LibrarySource; document: TextureDocument }): LibrarySource {
    const source = state.source
    // Prepared sources are always library identities, even if the first write
    // failed or the entry was deleted. Reuse their id instead of allocating again.
    this.save(state.document, source.id)
    this.saveWorking({ source, document: state.document })
    return source
  }
}

/** A Map-backed stand-in for localStorage, for tests and when storage is unavailable. */
export class MemoryStorage implements StorageLike {
  private readonly items = new Map<string, string>()

  getItem(key: string): string | null {
    return this.items.get(key) ?? null
  }

  setItem(key: string, value: string): void {
    this.items.set(key, value)
  }

  removeItem(key: string): void {
    this.items.delete(key)
  }
}

/** Local storage if the browser allows it, otherwise memory (nothing persists). */
export function browserStorage(): { storage: StorageLike; persistent: boolean } {
  try {
    const probe = '__procedural-textures-probe__'
    window.localStorage.setItem(probe, probe)
    window.localStorage.removeItem(probe)
    return { storage: window.localStorage, persistent: true }
  } catch {
    return { storage: new MemoryStorage(), persistent: false }
  }
}
