import {validateReaction,type ReactionConfig} from './reaction'
import metadata from './metadata'
import { formatColour, parseColour } from './colour'
import type { Json, Node, TextureDocument } from './types'

type ObjectValue = Record<string, unknown>
const object = (v: unknown, p: string): ObjectValue => {
  if (!v || typeof v !== 'object' || Array.isArray(v)) fail(p, 'Expected an object')
  return v as ObjectValue
}
function fail(path: string, reason: string): never { throw new Error(`Error in ${path}: ${reason}`) }
const text = (v: unknown, p: string): string => typeof v === 'string' ? v : fail(p, 'Expected a string')
const number = (v: unknown, p: string): number => typeof v === 'number' && Number.isFinite(v) ? v : fail(p, 'Expected a finite number')
const integer = (v: unknown, p: string): number => {
  const n = number(v, p)
  return Number.isSafeInteger(n) ? n : fail(p, 'Expected a safely representable integer')
}
const colour = (v: unknown, p: string): Json => {
  if (typeof v === 'string' && /^#(?:[0-9a-f]{6}|[0-9a-f]{8})$/i.test(v)) return formatColour(parseColour(v))
  if (Array.isArray(v) && (v.length === 3 || v.length === 4)) {
    const c = v.map((x) => number(x, p))
    return formatColour({ r: c[0], g: c[1], b: c[2], a: c[3] ?? 1 })
  }
  return fail(p, 'Colour must be #rrggbb, #rrggbbaa or an array of 3 or 4 numbers')
}
const vector = (v: unknown, p: string): number[] => {
  if (!Array.isArray(v) || v.length !== 3) fail(p, 'Expected three coordinates')
  return (v as unknown[]).map((x) => number(x, p))
}
const enumeration = (v: unknown, choices: string[], p: string): string => {
  const s = text(v, p)
  return choices.includes(s) ? s : fail(p, `Expected ${choices.join(', ')}`)
}

/** Haskell's v1→v5 migrations, followed by parsing, canonicalisation and reference checks.
 * Schema ranges are editing hints, not format restrictions. Work on a copy, never a saved value.
 */
export function processDocument(input: unknown): TextureDocument {
  // Bound input before cloning/recursing, including cycles and oversized foreign JSON.
  const pending: { value: unknown; depth: number; leave?: boolean }[] = [{ value: input, depth: 0 }]
  const ancestors = new Set<object>()
  let visits = 0
  while (pending.length) {
    const { value, depth, leave } = pending.pop()!
    if (leave) { ancestors.delete(value as object); continue }
    if (++visits > 100000 || depth > 128) fail('$', 'Document exceeds processing limits')
    if (value && typeof value === 'object') {
      if (ancestors.has(value)) fail('$', 'Document must be a JSON tree')
      ancestors.add(value)
      pending.push({ value, depth, leave: true })
      pending.push(...Object.values(value).map((v) => ({ value: v, depth: depth + 1 })))
    }
  }
  const source = object(input, '$'), version = number(source.version, '$.version')
  if (![1, 2, 3, 4, 5].includes(version)) fail('$.version', version > 5 ? 'Document version is newer than this program supports (5)' : 'Unknown document version')
  const d = structuredClone(source)
  const oldMode = (r: unknown): unknown => {
    if (!r || typeof r !== 'object' || Array.isArray(r)) return 'clamp'
    const o = r as ObjectValue
    return o.type === 'sinusoidal' ? 'mirror' : o.mode ?? 'clamp'
  }
  const strip = (r: unknown): unknown => {
    if (!r || typeof r !== 'object' || Array.isArray(r)) return r
    const o = { ...r as ObjectValue }; delete o.mode; return o
  }
  if (version <= 2) {
    const definitions = d.ramps && typeof d.ramps === 'object' && !Array.isArray(d.ramps) ? d.ramps as ObjectValue : {}
    const modes = new Map(Object.entries(definitions).map(([k, v]) => [k, oldMode(v)]))
    if (d.ramps && typeof d.ramps === 'object' && !Array.isArray(d.ramps)) d.ramps = Object.fromEntries(Object.entries(definitions).map(([k, v]) => [k, strip(v)]))
    const migrate = (v: unknown): unknown => {
      if (!v || typeof v !== 'object' || Array.isArray(v)) return v
      const o = v as ObjectValue, out: ObjectValue = Object.fromEntries(Object.entries(o).filter(([k]) => k !== 'ramp').map(([k, v]) => [k, migrate(v)]))
      if ('ramp' in o) {
        const r = o.ramp as ObjectValue | null
        out.ramp = strip(r)
        out.mode = r?.type === 'named' ? modes.get(String(r.name)) ?? 'clamp'
          : r?.type === 'builtin' ? metadata.version2WrappingLibraryRamps.includes(String(r.name)) ? 'wrap' : 'clamp'
          : oldMode(r)
      }
      return out
    }
    d.texture = migrate(d.texture)
  }
  if (version <= 3) {
    const lift = (v: unknown): unknown => {
      if (!v || typeof v !== 'object' || Array.isArray(v)) return v
      const o = v as ObjectValue
      for (const k of ['base', 'top', 'bottom', 'a', 'b']) if (k in o) o[k] = lift(o[k])
      for (const k of ['from', 'to', 'centre']) if (Array.isArray(o[k]) && o[k].length === 2) o[k] = [...o[k], 0]
      if (Array.isArray(o.scale) && o.scale.length === 2 && o.scale.every((v) => typeof v === 'number')) {
        const [x, y] = o.scale as number[], product = x * y
        o.scale = [x, y, Number.isFinite(x) && Number.isFinite(y) ? Number.isFinite(product) ? Math.sqrt(Math.abs(product)) : Math.sqrt(Math.abs(x)) * Math.sqrt(Math.abs(y)) : 0]
      }
      if (o.type === 'radial') o.axis = [0, 0, 1]
      if (o.type === 'tiled') o.depth = 1
      return o
    }
    d.texture = lift(d.texture)
  }
  const definitions: Record<string, Node> = Object.create(null)
  const ramp = (v: unknown, p: string, definition = false): Node => {
    const o = object(v, p), kind = text(o.type, `${p}.type`)
    if (kind === 'stops') {
      if (!Array.isArray(o.stops)) fail(`${p}.stops`, 'Expected an array')
      return { type: kind, stops: (o.stops as unknown[]).map((v, i) => {
        const path = `${p}.stops[${i}]`, s = object(v, path)
        return { position: number(s.position, `${path}.position`), colour: colour(s.colour, `${path}.colour`) }
      }) }
    }
    if (kind === 'sinusoidal') return { type: kind, from: colour(o.from, `${p}.from`), to: colour(o.to, `${p}.to`) }
    if (kind === 'named' || kind === 'builtin') {
      if (definition) fail(p, 'Named ramp definitions must be concrete ramps, not references')
      const name = text(o.name, `${p}.name`)
      if (kind === 'named' ? !Object.hasOwn(definitions, name) : !metadata.ramps.some((r) => r.id === name)) fail(p, `Unknown ${kind === 'named' ? 'named ramp' : 'library ramp'} "${name}"`)
      return { type: kind, name }
    }
    return fail(p, `Unknown ramp type "${kind}"`)
  }
  if (d.ramps != null) for (const [name, value] of Object.entries(object(d.ramps, '$.ramps')).sort(([a], [b]) => a < b ? -1 : a > b ? 1 : 0)) {
    definitions[name] = ramp(value, `$.ramps[${JSON.stringify(name)}]`, true)
  }
  const expression = (v: unknown, p: string, category: 'texture' | 'scalar' | 'vector' | 'domain' = 'texture'): Node => {
    const o = object(v, p), kind = text(o.type, `${p}.type`), out: Node = { type: kind }
    const variant = metadata.schema.validation[category]?.find((v) => v.type === kind)
    if (!variant) fail(p, `Unknown ${category} type "${kind}"`)
    for (const field of variant.fields) {
      const k = field.key, path = `${p}.${k}`, value = o[k] ?? field.default
      switch (field.kind) {
        case 'texture': out[k] = expression(value, path); break
        case 'scalarNode': out[k] = expression(value, path, 'scalar'); break
        case 'vectorNode': out[k] = expression(value, path, 'vector'); break
        case 'domain': out[k] = expression(value, path, 'domain'); break
        case 'ramp': out[k] = ramp(value, path); break
        case 'vector3': out[k] = vector(value, path); break
        case 'integer': out[k] = integer(value, path); break
        case 'number': out[k] = number(value, path); break
        case 'colour': out[k] = colour(value, path); break
        case 'enum': out[k] = enumeration(value, field.choices!, path); break
        case 'string': out[k] = text(value, path); break
        case 'stops': return fail(path, 'Stops belong to ramp definitions')
      }
    }
    if(kind==='periodic-noise'||kind==='periodic-fractal') {
      for(const key of ['periodX','periodY','periodZ']) if(!Number.isInteger(out[key])||Number(out[key])<1||Number(out[key])>256) fail(p+'.'+key,'Period must be an integer in 1–256')
      if(kind==='periodic-fractal'&&(!Number.isInteger(out.octaves)||Number(out.octaves)<1||Number(out.octaves)>8||Number(out.persistence)<0||Number(out.persistence)>1||!Number.isInteger(out.lacunarity)||Number(out.lacunarity)<1||Number(out.lacunarity)>4)) fail(p,'Invalid periodic fractal parameters')
    }
    if(kind==='reaction-diffusion') { try { validateReaction(out as unknown as ReactionConfig) } catch(error) {fail(p,error instanceof Error ? error.message : String(error))} }
    if(['worley','cell-value','cell-edge','cell-id','cell-colour'].includes(kind)) {
      if(out.dimensions!==2 && out.dimensions!==3) fail(`${p}.dimensions`,'Cellular dimensions must be 2 or 3')
      if(typeof out.seed!=='number' || out.seed<0 || out.seed>4294967295) fail(`${p}.seed`,'Cellular seed must be an unsigned 32-bit integer')
    }
    return out
  }
  const result = { version: 5, name: text(d.name, '$.name'), description: d.description == null ? '' : text(d.description, '$.description') } as TextureDocument
  const category = d.category == null ? '' : text(d.category, '$.category')
  if (category) result.category = category
  if (d.guide != null) {
    const g = object(d.guide, '$.guide')
    const role = enumeration(g.role, ['preset', 'study', 'comparison', 'composition'], '$.guide.role') as NonNullable<TextureDocument['guide']>['role']
    const order = g.order == null ? 100 : integer(g.order, '$.guide.order')
    if (order < 0 || order > 9999) fail('$.guide.order', 'Example order must be 0–9999')
    if (g.tags != null && !Array.isArray(g.tags)) fail('$.guide.tags', 'Expected an array')
    result.guide = { role, tags: ((g.tags ?? []) as unknown[]).map((tag, i) => text(tag, `$.guide.tags[${i}]`)), order }
    for (const key of ['family', 'hint'] as const) {
      const value = g[key] == null ? '' : text(g[key], `$.guide.${key}`)
      if (value) result.guide[key] = value
    }
    if (g.preview != null) {
      const preview = object(g.preview, '$.guide.preview')
      const axis = enumeration(preview.axis ?? 'xy', ['xy', 'xz', 'yz'], '$.guide.preview.axis') as 'xy' | 'xz' | 'yz'
      const position = number(preview.position ?? 0, '$.guide.preview.position')
      if (position < -2 || position > 2) fail('$.guide.preview.position', 'Preview position must be finite and between -2 and 2')
      result.guide.preview = { axis, position }
    }
  }
  if (Object.keys(definitions).length) result.ramps = definitions
  result.texture = expression(d.texture, '$.texture')
  // Keep identity for already canonical autosaves/library documents, regardless of key order.
  return equal(input, result) ? input as TextureDocument : result
}
function equal(a: unknown, b: unknown): boolean {
  if (a === b) return true
  if (!a || !b || typeof a !== 'object' || typeof b !== 'object' || Array.isArray(a) !== Array.isArray(b)) return false
  const x = a as ObjectValue, y = b as ObjectValue, keys = Object.keys(x)
  return keys.length === Object.keys(y).length && keys.every((k) => Object.hasOwn(y, k) && equal(x[k], y[k]))
}
