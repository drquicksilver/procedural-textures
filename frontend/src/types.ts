// Shapes of the JSON exchanged with the texture-server. Documents and
// texture nodes are kept as plain JSON so the editor stays generic: what a
// node may contain is described by the schema (GET /api/schema), not by
// TypeScript types.

export type Json = null | boolean | number | string | Json[] | { [key: string]: Json }

/** A texture or ramp node: an object tagged with its variant's "type". */
export interface Node {
  type: string
  [field: string]: Json
}

export interface TextureDocument {
  version: number
  name: string
  description: string
  /** Groups examples: "natural", "pattern", ... */
  category?: string
  /** Named ramps, referred to as {"type": "named", "name": ...}. Always concrete. */
  ramps?: Record<string, Node>
  texture: Node
}

/** A read-only ramp from the built-in library (GET /api/ramps). */
export interface LibraryRamp {
  id: string
  name: string
  description: string
  category: string
  ramp: Node
}

export interface Example {
  id: string
  document: TextureDocument
}

export interface Schema {
  version: number
  texture: Variant[]
  ramp: Variant[]
  defaultTexture: Node
}

export interface Variant {
  type: string
  label: string
  description: string
  fields: Field[]
  guides: Guide[]
  default: Node
}

export type FieldKind =
  | 'scalar'
  | 'int'
  | 'point'
  | 'vector'
  | 'colour'
  | 'enum'
  | 'stops'
  | 'text'
  | 'ramp'
  | 'texture'

export interface Field {
  key: string
  label: string
  help: string
  kind: FieldKind
  /** Slider range for scalar, int, point and vector fields. */
  min?: number
  max?: number
  step?: number
  /** Allowed values for enum fields. */
  options?: { value: string; label: string }[]
  handle?: Handle
}

export type Handle = { kind: 'point' } | { kind: 'radius'; centre: string }

export type Guide = { kind: 'line'; from: string; to: string }
