// @vitest-environment node
import {expect,it} from 'vitest'
import {compileScalar,compileVector,compileDomain} from './fields-cpu'
import type {ScalarField,VectorField,Domain} from './gpu/material'
import fixtures from '../../test-vectors/fields.json'
for(const c of fixtures.scalar)it(`worker scalar agrees with Haskell: ${c.field.type}`,()=>{const f=compileScalar(c.field as ScalarField);for(const s of c.samples)expect(f(s.point)).toBeCloseTo(s.value,10)})
for(const c of fixtures.vector)it(`worker vector agrees with Haskell: ${c.field.type}`,()=>{const f=compileVector(c.field as VectorField);for(const s of c.samples)f(s.point).forEach((v,i)=>expect(v).toBeCloseTo(s.value[i],10))})
for(const c of fixtures.domain)it(`worker domain agrees with Haskell: ${c.field.type}`,()=>{const f=compileDomain(c.field as Domain);for(const s of c.samples)f(s.point).forEach((v,i)=>expect(v).toBeCloseTo(s.value[i],10))})
it('explicit vector normalisation keeps zero safe and preserves tiny directions',()=>{
 expect(compileVector({type:'normalise-vector',source:{type:'vector-constant',value:[0,0,0]}})([0,0,0])).toEqual([0,0,0])
 expect(compileVector({type:'normalise-vector',source:{type:'vector-constant',value:[1e-30,0,0]}})([0,0,0])).toEqual([1,0,0])
})
it('field rotation preserves the direction of scaled axes',()=>{
 for(const m of [1e-30,1e-13,1,1e30]){
  const p=compileDomain({type:'rotate-field',centre:[0,0,0],axis:[m,0,0],angle:{type:'constant',value:90}})([0,1,0])
  p.forEach((v,i)=>expect(v).toBeCloseTo([0,0,-1][i],12))
 }
})
