import { describe, expect, it } from 'vitest'
import type { TextureDocument } from '../types'
import { compileGeometry, shapeNames } from './geometry'
import metadata from '../metadata'
import { compileMaterial } from './compiler'

const document = (texture: TextureDocument['texture']): TextureDocument => ({ version: 4, name: 'Test', description: '', texture })
const ramp = { type: 'stops', stops: [{ position: 0, colour: '#000000ff' }, { position: 1, colour: '#ffffffff' }] }

describe('GPU material compiler', () => {
  it('puts the GLSL version directive on the first line before helper definitions', () => {
    const source = compileMaterial(document({ type: 'flat', colour: '#ffffffff' })).source
    expect(source.startsWith('#version 300 es\n')).toBe(true)
  })

  it('keeps shader source stable through scalar, vector, colour, mode and octave edits', () => {
    const a = document({ type: 'fbm', scale: [1, 2, 3], octaves: 3, persistence: 0.5, lacunarity: 2, style: 'smooth', mode: 'clamp', ramp })
    const b = structuredClone(a)
    Object.assign(b.texture, { scale: [4, 5, 6], octaves: 7, persistence: 0.4, lacunarity: 1.9, style: 'ridged', mode: 'mirror', ramp: { ...ramp, stops: [{ position: -1, colour: '#123456ff' }, { position: 3, colour: '#abcdef80' }] } })
    const before = JSON.stringify(a), ca = compileMaterial(a), cb = compileMaterial(b)
    expect(ca.source).toBe(cb.source)
    expect(ca.parameters).not.toEqual(cb.parameters)
    expect(JSON.stringify(a)).toBe(before)
  })

  it('updates constant-span identity without recompiling, even for colours that round alike', () => {
    const colour = [0.85, 0.9, 1, 1]
    const a = document({ type: 'linear', from: [0, 0, 0], to: [1, 0, 0], mode: 'clamp', ramp: { type: 'stops', stops: [{ position: 0, colour }, { position: 1, colour }] } })
    const b = structuredClone(a)
    b.texture.ramp = { type: 'stops', stops: [{ position: 0, colour }, { position: 1, colour: [0.85, 0.9 + 1e-9, 1, 1] }] }
    const ca = compileMaterial(a), cb = compileMaterial(b)
    expect(ca.source).toBe(cb.source)
    expect(ca.parameters).not.toEqual(cb.parameters)
    expect(Math.fround(0.9)).toBe(Math.fround(0.9 + 1e-9))
  })

  it('changes program structure for new branches and ramp stop counts', () => {
    const a = document({ type: 'flat', colour: '#ffffffff' })
    const b = document({ type: 'layer', top: a.texture, bottom: a.texture })
    expect(compileMaterial(a).source).not.toBe(compileMaterial(b).source)
    const linear = document({ type: 'linear', from: [0, 0, 0], to: [1, 0, 0], mode: 'clamp', ramp })
    const extended = structuredClone(linear)
    extended.texture.ramp = { ...ramp, stops: [...ramp.stops, { position: 0.5, colour: '#ff0000ff' }] }
    expect(compileMaterial(linear).source).not.toBe(compileMaterial(extended).source)
  })

  it('resolves document ramps without changing the input or shader structure', () => {
    const inline = document({ type: 'linear', from: [0, 0, 0], to: [1, 0, 0], mode: 'clamp', ramp })
    const shared = structuredClone(inline)
    shared.ramps = { test: ramp }
    shared.texture.ramp = { type: 'named', name: 'test' }
    expect(compileMaterial(shared)).toEqual(compileMaterial(inline))
    shared.texture.ramp = { type: 'named', name: 'missing' }
    expect(() => compileMaterial(shared)).toThrow('Unknown named ramp')
  })

  it('fails clearly for unsupported nodes and unsafe numerical workloads', () => {
    expect(() => compileMaterial(document({ type: 'unknown' }))).toThrow('Unknown texture type')
    const n = { type: 'fbm', scale: [1, 1, 1], octaves: 33, persistence: 0.5, lacunarity: 2, style: 'smooth', ramp }
    expect(() => compileMaterial(document(n))).toThrow('1–32 octaves')
    expect(() => compileMaterial(document({ ...n, octaves: 4, lacunarity: 1e40 }))).toThrow('Non-finite GPU parameter')
    expect(() => compileMaterial(document({ ...n, scale: [NaN, 1, 1] }))).toThrow('Expected a finite number')
  })
  it('compiles every shipped document and shape, including built-in references', () => {
    const examples = import.meta.glob<TextureDocument>('../../../examples/*.json', { eager: true, import: 'default' })
    expect(Object.keys(examples).length).toBe(metadata.examples.length)
    for (const doc of Object.values(examples)) expect(compileMaterial(doc).parameters.length).toBeGreaterThan(0)
    for (const shape of shapeNames) expect(compileMaterial(document({ type: 'flat', colour: '#ffffffff' }), { shape }).source).toContain('float solid(vec3 p)')
    expect(() => compileMaterial(document({ type: 'perlin', scale: [1, 1, 1], ramp: { type: 'builtin', name: 'absent' } }))).toThrow('Unknown library ramp')
  })

  it('bounds deep trees before reaching the JavaScript or driver stack limit', () => {
    let tree = { type: 'flat', colour: '#ffffffff' } as TextureDocument['texture']
    for (let i = 0; i < 70; i++) tree = { type: 'layer', top: tree, bottom: { type: 'flat', colour: '#ffffffff' } }
    expect(() => compileMaterial(document(tree))).toThrow('texture nesting exceeds 64')
  })

  it('keeps the coordinate-aware warp cache stable when matching configurations diverge', () => {
    const warp = { type: 'turbulence', amount: 0.2, octaves: 4, persistence: 0.5, lacunarity: 2, base: { type: 'flat', colour: '#ff000080' } }
    const a = document({ type: 'layer', top: warp, bottom: structuredClone(warp) })
    const b = structuredClone(a)
    ;(b.texture.bottom as typeof warp).lacunarity = 1.7
    expect(compileMaterial(a).source).toBe(compileMaterial(b).source)
    expect(compileMaterial(a).source).toContain('equal(cachedConfig,data(')
  })

  it('rejects excessive geometry before serialising or recursing through it', () => {
    let tree = { type: 'sphere', centre: [0, 0, 0], radius: 1 } as Parameters<typeof compileGeometry>[0]
    for (let i = 0; i < 70; i++) tree = { type: 'rounded', amount: 0.1, base: tree }
    expect(() => compileGeometry(tree)).toThrow('Geometry nesting exceeds 64')
    expect(compileGeometry({ type: 'sphere', centre: [0, 0, 0], radius: 1e30 })).toContain('(1e+30)')
  })

})

it('shares U/V volumes, keeps chemistry edits out of shader source and bounds distinct volumes',()=>{
  const field={type:'reaction-diffusion',resolution:8,iterations:0,feed:.022,kill:.051,diffusionU:.9,diffusionV:.45,timeStep:1,seed:42,initial:'spots',output:'v'}
  const colour=(f:typeof field)=>({type:'colourise',field:f,mode:'clamp',ramp})
  const a={...document(colour(field)),version:5}
  const b={...document(colour({...field,seed:99,output:'u'})),version:5}
  expect(compileMaterial(a).source).toBe(compileMaterial(b).source)
  expect(compileMaterial(a).volumes).not.toEqual(compileMaterial(b).volumes)
  const shared={...document({type:'blend',mode:'screen',opacity:1,top:colour(field),bottom:colour({...field,output:'u'})}),version:5}
  expect(compileMaterial(shared).volumes).toHaveLength(1)
  let texture=colour(field)
  for(let seed=43;seed<=46;seed++) texture={type:'blend',mode:'screen',opacity:1,top:colour({...field,seed}),bottom:texture} as unknown as typeof texture
  expect(()=>compileMaterial({...document(texture),version:5})).toThrow('four distinct reaction')
})

it('keeps prepared branching shader source stable through hierarchy and seed edits', () => {
  const field={type:'branch-distance',dimensions:2,seed:41,depth:3,length:.3,spread:28,taper:.65,radius:.015}
  const a=document({type:'colourise',field,mode:'clamp',ramp})
  const b=document({type:'colourise',field:{...field,dimensions:3,seed:4294967295,depth:7},mode:'clamp',ramp})
  expect(compileMaterial(a).source).toBe(compileMaterial(b).source)
  expect(compileMaterial(a).parameters).not.toEqual(compileMaterial(b).parameters)
  expect(()=>compileMaterial(document({type:'colourise',field:{...field,depth:8},mode:'clamp',ramp}))).toThrow()
})
