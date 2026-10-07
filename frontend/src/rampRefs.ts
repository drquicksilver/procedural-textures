// Operations on ramps as first-class objects: named ramps shared within a
// document, references to the built-in library, and copying ramps between
// documents. Every operation returns a new document, so each is one undo
// step.

import { clone, categoryOf, childCategory, getAt, isNode, setAt, type Path } from './tree'
import type { LibraryRamp, Node, Schema, TextureDocument } from './types'

export interface RampSources {
  documentRamps: Record<string, Node>
  builtins: LibraryRamp[]
}

export function isReference(ramp: Node): boolean {
  return ramp.type === 'named' || ramp.type === 'builtin'
}

/** The concrete ramp a ramp stands for, or undefined if a reference is dangling. */
export function resolveRamp(ramp: Node, sources: RampSources): Node | undefined {
  if (ramp.type === 'named') return sources.documentRamps[String(ramp.name)]
  if (ramp.type === 'builtin') return sources.builtins.find((b) => b.id === ramp.name)?.ramp
  return ramp
}

export function sourcesOf(document: TextureDocument, builtins: LibraryRamp[]): RampSources {
  return { documentRamps: document.ramps ?? {}, builtins }
}

/** Visit every ramp field in a texture, in display order. */
export function forEachRamp(schema: Schema, texture: Node, visit: (ramp: Node, path: Path, key: string) => void, path: Path = []): void {
  const variant = schema[categoryOf(schema, texture)]?.find((v) => v.type === texture.type)
  for (const field of variant?.fields ?? []) {
    const value = texture[field.key]
    if (!isNode(value)) continue
    if (field.kind === 'ramp') visit(value, path, field.key)
    else if (childCategory(field.kind) !== undefined) forEachRamp(schema, value, visit, [...path, field.key])
  }
}

/** Replace every ramp in a texture by `change` (which may return it unchanged). */
export function mapRamps(schema: Schema, texture: Node, change: (ramp: Node) => Node): Node {
  const variant = schema[categoryOf(schema, texture)]?.find((v) => v.type === texture.type)
  let result = texture
  for (const field of variant?.fields ?? []) {
    const value = texture[field.key]
    if (!isNode(value)) continue
    const next = field.kind === 'ramp' ? change(value) : childCategory(field.kind) !== undefined ? mapRamps(schema, value, change) : value
    if (next !== value) result = { ...result, [field.key]: next }
  }
  return result
}

/** How many ramp fields in the document refer to a named ramp. */
export function usageCount(schema: Schema, document: TextureDocument, name: string): number {
  let count = 0
  forEachRamp(schema, document.texture, (ramp) => {
    if (ramp.type === 'named' && ramp.name === name) count++
  })
  return count
}

/** `base`, or `base 2`, `base 3`… whichever is not yet a named ramp. */
export function uniqueName(existing: Record<string, unknown>, base: string): string {
  const clean = base.trim() || 'ramp'
  if (!(clean in existing)) return clean
  for (let i = 2; ; i++) if (!(`${clean} ${i}` in existing)) return `${clean} ${i}`
}

function setRampField(document: TextureDocument, path: Path, key: string, ramp: Node): TextureDocument {
  const node = getAt(document.texture, path)
  if (!node) throw new Error(`No texture at ${path.join('.')}`)
  return { ...document, texture: setAt(document.texture, path, { ...node, [key]: ramp }) }
}

function reference(name: string): Node {
  return { type: 'named', name }
}

/** Move the (concrete) ramp in a field into the document's named ramps, and refer to it. */
export function shareRamp(document: TextureDocument, path: Path, key: string, name: string): TextureDocument {
  const node = getAt(document.texture, path)
  const ramp = node?.[key]
  if (!isNode(ramp) || isReference(ramp)) return document
  const ramps = document.ramps ?? {}
  const finalName = uniqueName(ramps, name)
  return setRampField({ ...document, ramps: { ...ramps, [finalName]: ramp } }, path, key, reference(finalName))
}

/** Replace a reference in a field by a local copy of what it refers to. */
export function detachRamp(document: TextureDocument, path: Path, key: string, builtins: LibraryRamp[]): TextureDocument {
  const ramp = getAt(document.texture, path)?.[key]
  if (!isNode(ramp)) return document
  const resolved = resolveRamp(ramp, sourcesOf(document, builtins))
  return resolved ? setRampField(document, path, key, clone(resolved)) : document
}

/** Point a field at a library ramp. */
export function useBuiltin(document: TextureDocument, path: Path, key: string, id: string): TextureDocument {
  return setRampField(document, path, key, { type: 'builtin', name: id })
}

/** Point a field at one of the document's named ramps. */
export function useNamed(document: TextureDocument, path: Path, key: string, name: string): TextureDocument {
  return setRampField(document, path, key, reference(name))
}

/**
 * Use a ramp from outside the document (one of your saved ramps): it is
 * copied into the document's named ramps, so the document stays
 * self-contained, and the field refers to the copy. An identical copy
 * already in the document is reused.
 */
export function useCopiedRamp(document: TextureDocument, path: Path, key: string, name: string, ramp: Node): TextureDocument {
  const ramps = document.ramps ?? {}
  const existing = Object.entries(ramps).find(([, r]) => JSON.stringify(r) === JSON.stringify(ramp))
  if (existing) return useNamed(document, path, key, existing[0])
  const finalName = uniqueName(ramps, name)
  return setRampField({ ...document, ramps: { ...ramps, [finalName]: clone(ramp) } }, path, key, reference(finalName))
}

/** Change a named ramp's definition (affecting every use). */
export function setNamedRamp(document: TextureDocument, name: string, ramp: Node): TextureDocument {
  return { ...document, ramps: { ...(document.ramps ?? {}), [name]: ramp } }
}

/** Rename a named ramp and every reference to it. Returns the document unchanged if the name is taken. */
export function renameNamedRamp(schema: Schema, document: TextureDocument, from: string, to: string): TextureDocument {
  const ramps = document.ramps ?? {}
  const target = to.trim()
  if (!target || target === from || target in ramps || !(from in ramps)) return document
  const renamed = Object.fromEntries(Object.entries(ramps).map(([k, v]) => [k === from ? target : k, v]))
  const texture = mapRamps(schema, document.texture, (r) => (r.type === 'named' && r.name === from ? reference(target) : r))
  return { ...document, ramps: renamed, texture }
}

/** Remove a named ramp that nothing uses. */
export function removeNamedRamp(schema: Schema, document: TextureDocument, name: string): TextureDocument {
  if (usageCount(schema, document, name) > 0) return document
  const ramps = { ...(document.ramps ?? {}) }
  delete ramps[name]
  return { ...document, ramps }
}

/**
 * Bring a texture from another document (an example, say) into this one.
 * The named ramps it uses are copied across, renamed where they would clash
 * with different ramps already here, and its references rewritten to match.
 */
export function importTexture(schema: Schema, target: TextureDocument, source: TextureDocument): { ramps: Record<string, Node>; texture: Node } {
  const ramps = { ...(target.ramps ?? {}) }
  const renames = new Map<string, string>()
  forEachRamp(schema, source.texture, (ramp) => {
    if (ramp.type !== 'named') return
    const name = String(ramp.name)
    const definition = source.ramps?.[name]
    if (!definition || renames.has(name)) return
    if (name in ramps && JSON.stringify(ramps[name]) === JSON.stringify(definition)) {
      renames.set(name, name)
    } else {
      const finalName = uniqueName(ramps, name)
      ramps[finalName] = clone(definition)
      renames.set(name, finalName)
    }
  })
  const texture = mapRamps(schema, clone(source.texture), (r) =>
    r.type === 'named' && renames.has(String(r.name)) ? reference(renames.get(String(r.name))!) : r,
  )
  return { ramps, texture }
}

