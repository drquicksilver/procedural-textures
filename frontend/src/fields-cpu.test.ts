// @vitest-environment node
import {expect,it} from 'vitest'
import {compileScalar,compileVector,compileDomain} from './fields-cpu'
import type {ScalarField,VectorField,Domain} from './gpu/material'
import fixtures from '../../test-vectors/fields.json'
for(const c of fixtures.scalar)it(`worker scalar agrees with Haskell: ${c.field.type}`,()=>{const f=compileScalar(c.field as ScalarField);for(const s of c.samples)expect(f(s.point)).toBeCloseTo(s.value,10)})
for(const c of fixtures.vector)it(`worker vector agrees with Haskell: ${c.field.type}`,()=>{const f=compileVector(c.field as VectorField);for(const s of c.samples)f(s.point).forEach((v,i)=>expect(v).toBeCloseTo(s.value[i],10))})
for(const c of fixtures.domain)it(`worker domain agrees with Haskell: ${c.field.type}`,()=>{const f=compileDomain(c.field as Domain);for(const s of c.samples)f(s.point).forEach((v,i)=>expect(v).toBeCloseTo(s.value[i],10))})
