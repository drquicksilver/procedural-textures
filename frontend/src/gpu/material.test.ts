import { expect, it } from 'vitest'
import metadata from '../metadata'
import { processDocument } from '../document'
import { resolveMaterial } from './material'
import { compileMaterial } from './compiler'

it('resolves every reference constructor default through the typed compiler boundary', () => {
  for (const variant of metadata.schema.texture) {
    const document = { version: 4, name: variant.type, description: '', texture: variant.default }
    expect(resolveMaterial(document).type).toBe(variant.type)
    expect(compileMaterial(document).parameters.length).toBeGreaterThan(0)
  }
})

it('resolves concrete ramps and preserves finite out-of-range colour semantics', () => {
  const colour = [0.2, 0.3, 0.4, 2]
  const document = { version: 4, name: 'Constant', description: '', ramps: { shared: { type: 'stops', stops: [{ position: 1, colour }, { position: 0, colour }] } }, texture: { type: 'linear', from: [0, 0, 0], to: [1, 0, 0], ramp: { type: 'named', name: 'shared' } } }
  const material = resolveMaterial(document)
  expect(material.type).toBe('linear')
  if (material.type !== 'linear' || material.ramp.type !== 'stops') throw new Error('Expected resolved stops')
  expect(material.mode).toBe('clamp')
  expect(material.ramp.stops.map((s) => s.position)).toEqual([0, 1])
  expect(material.ramp.stops[0].colour.a).toBe(2)
  expect(document.ramps.shared.stops[0].position).toBe(1)
})

it('uses structural metadata without treating slider limits as document restrictions', () => {
  const document = { version: 4, name: 'Outside sliders', texture: { type: 'fbm', scale: [1, 1, 1], octaves: 2, persistence: -2, lacunarity: 2, style: 'ridged', ramp: { type: 'stops', stops: [] } } }
  expect(processDocument(document).texture.persistence).toBe(-2)
  expect(() => resolveMaterial({ ...document, description: '' })).not.toThrow()
  expect(() => processDocument({ ...document, texture: { ...document.texture, style: 'unknown' } })).toThrow('$.texture.style')
  expect(() => processDocument({ ...document, texture: { ...document.texture, octaves: 2.5 } })).toThrow('$.texture.octaves')
  for (const variant of metadata.schema.validation.texture) for (const field of variant.fields) {
    expect(field).not.toHaveProperty('min')
    expect(field).not.toHaveProperty('max')
  }
})

it('accepts repeated acyclic values but rejects cycles before resolving materials', () => {
  const shared = { type: 'flat', colour: '#ffffff' }
  expect(resolveMaterial({ version: 4, name: 'Sharing', description: '', texture: { type: 'layer', top: shared, bottom: shared } }).type).toBe('layer')
  const cyclic = { type: 'layer', top: shared, bottom: shared }
  cyclic.bottom = cyclic as unknown as typeof shared
  expect(() => resolveMaterial({ version: 4, name: 'Cycle', description: '', texture: cyclic })).toThrow('JSON tree')
})

it('uses structural feedback keys without treating numeric changes as cold programs', async () => {
  const { materialStructure } = await import('./material')
  const document = metadata.examples.find((e) => e.id === 'marble')!.document
  const changed = structuredClone(document)
  if (changed.texture.type !== 'layer') throw new Error('Expected Marble layers')
  ;(changed.texture.top as { amount: number }).amount = 12
  expect(materialStructure(changed)).toBe(materialStructure(document))
  expect(materialStructure({ ...document, texture: { type: 'flat', colour: '#ffffff' } })).not.toBe(materialStructure(document))
  expect(() => materialStructure({ ...document, texture: { type: 'unknown' } })).not.toThrow()
})
