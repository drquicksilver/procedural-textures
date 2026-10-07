import { expect, it } from 'vitest'
import metadata from './metadata'
import { processDocument } from './document'
import { categoryOf, changeType, defaultNode, flatten, inspectionTexture, swapChildren, wrapOptions } from './tree'
import { compileMaterial } from './gpu/compiler'
import { materialStructure } from './gpu/material'
import type { Node, TextureDocument } from './types'

const document = (texture: Node): TextureDocument => ({ version: 5, name: 'Fields', description: '', texture })
it('compiles and inspects every scalar, vector and domain default', () => {
  for (const category of ['scalar','vector','domain'] as const) for (const variant of metadata.schema[category]!) {
    const texture=inspectionTexture(metadata.schema,variant.default)
    expect(categoryOf(metadata.schema,variant.default)).toBe(category)
    expect(() => compileMaterial(document(texture))).not.toThrow()
    expect(() => processDocument(document(texture))).not.toThrow()
  }
})
it('rejects wrong typed edges and locates the error', () => {
  const bad=document({ type:'colourise',field:{ type:'flat',colour:'#ffffff' },mode:'clamp',ramp:{type:'builtin',name:'greyscale'} })
  expect(() => processDocument(bad)).toThrow('$.texture.field')
  const domain=document({ type:'domain',domain:{type:'compose',first:{type:'translate',offset:[0,0,0]},second:{type:'noise'}},base:{type:'flat',colour:'#ffffff'} })
  expect(() => processDocument(domain)).toThrow('$.texture.domain.second')
  const vector=document({type:'vector-colour',field:{type:'components',x:{type:'constant',value:0},y:{type:'position'},z:{type:'constant',value:0}}})
  expect(() => processDocument(vector)).toThrow('$.texture.field.y')
})
it('keeps typed numeric edits in uniforms while structural edits change the program', () => {
  const a=metadata.examples.find((e) => e.id==='double-warp-opal')!.document
  const b=structuredClone(a); b.texture.amount=123 // ignored extra fields do not affect canonical expression
  ;(b.texture.domain as Node).amount=.17
  const ca=compileMaterial(a), cb=compileMaterial(b)
  expect(ca.source).toBe(cb.source); expect(ca.parameters).not.toEqual(cb.parameters)
  expect(materialStructure(a)).toBe(materialStructure(b))
  ;(b.texture.domain as Node).field={type:'vector-constant',value:[0,0,0]}
  expect(compileMaterial(b).source).not.toBe(ca.source)
  expect(materialStructure(a)).not.toBe(materialStructure(b))
})
it('bounds nested fractal sampling work before driver compilation', () => {
  let field: Node={type:'noise'}
  for(let i=0;i<3;i++) field={type:'fractal',octaves:32,persistence:.5,lacunarity:2,style:'smooth',source:field}
  expect(() => compileMaterial(document(inspectionTexture(metadata.schema,field)))).toThrow('4096 expanded noise samples')
})
it('tree operations follow typed edges and offer only compatible structural actions', () => {
  const node: Node={type:'colourise',field:{type:'scalar-domain',domain:{type:'compose',first:{type:'translate',offset:[0,0,0]},second:{type:'scale',scale:[1,1,1]}},source:{type:'noise'}},mode:'clamp',ramp:{type:'builtin',name:'greyscale'}}
  expect(flatten(metadata.schema,node,new Set()).map((e)=>e.path.join('.'))).toEqual(['','field','field.domain','field.domain.first','field.domain.second','field.source'])
  const wrappers=wrapOptions(metadata.schema,'scalar')
  expect(wrappers.some((w)=>w.type==='fractal')).toBe(true)
  expect(wrappers.some((w)=>w.type==='layer')).toBe(false)
  const composed=(node.field as Node).domain as Node
  expect(swapChildren(metadata.schema,composed)?.first).toEqual(composed.second)
  expect(swapChildren(metadata.schema,node)).toBeUndefined()
  const mixed: Node={type:'mix',mask:{type:'constant',value:.5},a:{type:'flat',colour:'#ff0000'},b:{type:'flat',colour:'#0000ff'}}
  expect(swapChildren(metadata.schema,mixed)?.a).toEqual(mixed.b)
  expect(swapChildren(metadata.schema,mixed)?.mask).toEqual(mixed.mask)
  expect(changeType(metadata.schema,'scalar',{type:'constant',value:.4},'noise')).toEqual({type:'noise'})
  expect(defaultNode(metadata.schema,'domain').type).toBe('translate')
})
it('every new expression capability appears in a material example', () => {
  const tags=new Set<string>()
  const visit=(value: unknown) => {
    if(!value || typeof value!=='object') return
    if('type' in value) tags.add(String(value.type))
    for(const child of Object.values(value)) visit(child)
  }
  for(const example of metadata.examples) visit(example.document.texture)
  for(const category of ['scalar','vector','domain'] as const) for(const variant of metadata.schema[category]!) expect(tags.has(variant.type),`${category}/${variant.type}`).toBe(true)
})

it('keeps cellular modes and full-width seeds in uniforms, and validates configuration', () => {
  const field: Node={type:'worley',dimensions:2,jitter:1,seed:4294967295,metric:'euclidean',output:'f1'}
  const a=document(inspectionTexture(metadata.schema,field))
  const b=document(inspectionTexture(metadata.schema,{...field,seed:2147483648,metric:'manhattan',output:'gap',dimensions:3}))
  const ca=compileMaterial(a),cb=compileMaterial(b)
  expect(ca.source).toBe(cb.source)
  expect(ca.parameters).not.toEqual(cb.parameters)
  expect(materialStructure(a)).toBe(materialStructure(b))
  for(const seed of [-1,4294967296,1.5]) expect(() => processDocument(document(inspectionTexture(metadata.schema,{...field,seed})))).toThrow('seed')
  expect(() => processDocument(document(inspectionTexture(metadata.schema,{...field,dimensions:4})))).toThrow('dimensions')
  expect(() => processDocument(document(inspectionTexture(metadata.schema,{...field,metric:'taxicab'})))).toThrow('metric')
})
it('bounds cellular neighbourhood work through fractal and vector composition', () => {
  const cellular: Node={type:'worley',dimensions:3,jitter:1,seed:0,metric:'euclidean',output:'f1'}
  const fractal: Node={type:'fractal',octaves:13,persistence:.5,lacunarity:2,style:'smooth',source:cellular}
  expect(() => compileMaterial(document(inspectionTexture(metadata.schema,fractal)))).toThrow('4096 expanded noise samples')
  const edge: Node={type:'cell-edge',dimensions:3,jitter:1,seed:0}
  expect(() => compileMaterial(document(inspectionTexture(metadata.schema,edge)))).not.toThrow()
  expect(() => compileMaterial(document(inspectionTexture(metadata.schema,{type:'add',a:edge,b:edge})))).toThrow('4096 expanded noise samples')
})
