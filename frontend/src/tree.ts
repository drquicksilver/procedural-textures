// Pure operations on texture trees, driven by the schema. A path is the list
// of texture-field keys leading from the root texture to a node, e.g.
// ["top", "base"] for root.top.base. Ramps are not tree nodes: they are edited
// as fields of the texture that owns them.

import type { Field, Json, Node, Schema, Variant } from './types'

export type Path = string[]
export type Category = 'texture' | 'ramp' | 'scalar' | 'vector' | 'domain'

export function clone<T>(value: T): T {
  return structuredClone(value)
}

export function samePath(a: Path, b: Path): boolean {
  return a.length === b.length && a.every((key, i) => key === b[i])
}

export function isPrefix(prefix: Path, path: Path): boolean {
  return prefix.length <= path.length && prefix.every((key, i) => key === path[i])
}

export function variantOf(schema: Schema, category: Category, type: string): Variant | undefined {
  return schema[category]?.find((v) => v.type === type)
}

/** Typed expression child slots, including colour, scalar, vector and domain. */
export function textureFields(schema: Schema, node: Node): Field[] {
  return variantOf(schema, categoryOf(schema, node), node.type)?.fields.filter((f) => childCategory(f.kind) !== undefined) ?? []
}

export function isNode(value: Json | undefined): value is Node {
  return typeof value === 'object' && value !== null && !Array.isArray(value) && typeof value.type === 'string'
}

/** The node at a path, or undefined if the path no longer exists. */
export function getAt(root: Node, path: Path): Node | undefined {
  let node: Node | undefined = root
  for (const key of path) {
    const child: Json | undefined = node?.[key]
    node = isNode(child) ? child : undefined
  }
  return node
}

/** A copy of the tree with the node at `path` replaced. */
export function setAt(root: Node, path: Path, replacement: Node): Node {
  if (path.length === 0) return replacement
  const [key, ...rest] = path
  const child = root[key]
  if (!isNode(child)) throw new Error(`No texture at ${path.join('.')}`)
  return { ...root, [key]: setAt(child, rest, replacement) }
}

/** A copy of the tree with one field of the node at `path` changed. */
export function setField(root: Node, path: Path, key: string, value: Json): Node {
  const node = getAt(root, path)
  if (!node) throw new Error(`No texture at ${path.join('.')}`)
  return setAt(root, path, { ...node, [key]: value })
}

/**
 * Switch a node to another variant, keeping every field the two variants
 * share (same key and kind) and taking the rest from the new variant's
 * default.
 */
export function changeType(schema: Schema, category: Category, node: Node, type: string): Node {
  const from = variantOf(schema, category, node.type)
  const to = variantOf(schema, category, type)
  if (!to) throw new Error(`Unknown ${category} type ${type}`)
  const result: Node = clone(to.default)
  for (const field of to.fields) {
    const old = from?.fields.find((f) => f.key === field.key)
    if (old && old.kind === field.kind && node[field.key] !== undefined) {
      result[field.key] = node[field.key]
    }
  }
  return result
}

export interface WrapOption {
  type: string
  key: string
  label: string
}

/** Every way of wrapping a node: as each texture field of each variant. */
export function wrapOptions(schema: Schema, category: Category = 'texture'): WrapOption[] {
  return (schema[category] ?? []).flatMap((variant) => {
    const slots = variant.fields.filter((f) => childCategory(f.kind) === category)
    return slots.map((field) => ({
      type: variant.type,
      key: field.key,
      label: slots.length > 1 ? `${variant.label}, as ${field.label.toLowerCase()}` : variant.label,
    }))
  })
}

/** Put a node inside a new node of the given variant, in the given field. */
export function wrap(schema: Schema, node: Node, type: string, key: string): Node {
  const variant = variantOf(schema, categoryOf(schema, node), type)
  if (!variant) throw new Error(`Unknown texture type ${type}`)
  return { ...clone(variant.default), [key]: node }
}

/** Swap the two children of a node that has exactly two texture fields. */
export function swapChildren(schema: Schema, node: Node): Node | undefined {
  const fields = textureFields(schema, node).filter((f) => childCategory(f.kind) === categoryOf(schema,node))
  if (fields.length !== 2 || fields[0].kind !== fields[1].kind) return undefined
  const [a, b] = fields
  return { ...node, [a.key]: node[b.key], [b.key]: node[a.key] }
}

export interface TreeEntry {
  path: Path
  node: Node
  depth: number
  /** The label of the field holding this node in its parent, if any. */
  fieldLabel?: string
  hasChildren: boolean
}

/** The tree flattened in display order, skipping collapsed subtrees. */
export function flatten(schema: Schema, root: Node, collapsed: Set<string>): TreeEntry[] {
  const entries: TreeEntry[] = []
  const visit = (node: Node, path: Path, depth: number, fieldLabel?: string) => {
    const fields = textureFields(schema, node)
    entries.push({ path, node, depth, fieldLabel, hasChildren: fields.length > 0 })
    if (collapsed.has(pathKey(path))) return
    for (const field of fields) {
      const child = node[field.key]
      if (isNode(child)) visit(child, [...path, field.key], depth + 1, field.label)
    }
  }
  visit(root, [], 0)
  return entries
}

export function pathKey(path: Path): string {
  return path.join('/')
}

export function childCategory(kind: string): Category | undefined {
  return kind === 'scalarNode' ? 'scalar' : kind === 'vectorNode' ? 'vector' : kind === 'domain' ? 'domain' : kind === 'texture' ? 'texture' : undefined
}
export function categoryOf(schema: Schema, node: Node): Category {
  return (['texture','scalar','vector','domain'] as const).find((category) => schema[category]?.some((v) => v.type === node.type)) ?? 'texture'
}
export function defaultNode(schema: Schema, category: Category): Node {
  return clone(category === 'texture' ? schema.defaultTexture : schema[category]![0].default)
}
/** A standalone colour document for scalar/vector/domain inspection. */
export function inspectionTexture(schema: Schema, node: Node): Node {
  const category = categoryOf(schema, node)
  if (category === 'scalar') return { type: 'colourise', field: node, mode: 'clamp', ramp: { type: 'builtin', name: 'greyscale' } }
  if (category === 'vector') return { type: 'vector-colour', field: node }
  if (category === 'domain') return { type: 'vector-colour', field: { type: 'vector-domain', domain: node, source: { type: 'position' } } }
  return node
}
