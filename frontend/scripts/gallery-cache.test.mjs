import assert from 'node:assert/strict'
import { test } from 'node:test'
import { mkdtemp, mkdir, readFile, writeFile, rm } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { imageKey, restoreGalleryImages, saveGalleryImages } from './gallery-cache.mjs'
const png = Buffer.from('iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+jG1kAAAAASUVORK5CYII=', 'base64')
const inputs = { renderer: 'renderer-and-ramps-v1', documents: { a: { version:5,texture:{type:'flat',colour:'#ffffff'} }, b:{version:5,texture:{type:'flat',colour:'#000000'}} } }

test('render keys cover material, renderer/ramp, view and resolution inputs', () => {
  assert.notEqual(imageKey('a.png',inputs),imageKey('a-solid.png',inputs))
  assert.notEqual(imageKey('shape-cube-a.png',inputs),imageKey('shape-sphere-a.png',inputs))
  assert.notEqual(imageKey('a.png',inputs),imageKey('a.png',{...inputs,renderer:'new-resolution-or-renderer'}))
  assert.equal(imageKey('../a.png',inputs),undefined)
  assert.equal(imageKey('deleted.png',inputs),undefined)
})
test('cache reuses unchanged images, invalidates only changed materials, and detects corruption', async () => {
  const dir=await mkdtemp(join(tmpdir(),'gallery-cache-')), cache=join(dir,'cache'), output=join(dir,'output')
  try {
    await mkdir(output)
    for(const name of ['a.png','a-solid.png','shape-cube-a.png','b.png']) await writeFile(join(output,name),png)
    assert.equal(await saveGalleryImages(cache,output,inputs),4)
    await rm(output,{recursive:true});await mkdir(output)
    assert.equal(await restoreGalleryImages(cache,output,inputs),4)
    assert.deepEqual(await readFile(join(output,'a.png')),png)
    const changed=structuredClone(inputs);changed.documents.a.texture.colour='#ff0000'
    await rm(output,{recursive:true});await mkdir(output)
    assert.equal(await restoreGalleryImages(cache,output,changed),1)
    await writeFile(join(cache,imageKey('b.png',inputs)+'.png'),'corrupt')
    assert.equal(await restoreGalleryImages(cache,output,changed),0)
    assert.equal(await restoreGalleryImages(cache,output,{...inputs,renderer:'changed'}),0)
  } finally {await rm(dir,{recursive:true,force:true})}
})
test('missing or invalid cache manifests are cold caches', async () => {
  const dir=await mkdtemp(join(tmpdir(),'gallery-cache-'))
  try {
    assert.equal(await restoreGalleryImages(dir,join(dir,'output'),inputs),0)
    await writeFile(join(dir,'manifest.json'),'invalid JSON')
    assert.equal(await restoreGalleryImages(dir,join(dir,'output'),inputs),0)
  } finally {await rm(dir,{recursive:true,force:true})}
})

test('repository inputs ignore frontend/text metadata but invalidate renderer and ramp changes', async () => {
  const { galleryInputs } = await import('./gallery-cache.mjs')
  const dir=await mkdtemp(join(tmpdir(),'gallery-inputs-'))
  try {
    for (const folder of ['src','app','ramps','examples','frontend']) await mkdir(join(dir,folder))
    for (const name of ['src/Texture.hs','app/Main.hs','procedural-textures.cabal','stack.yaml','stack.yaml.lock','ramps/grey.json']) await writeFile(join(dir,name),'initial')
    const doc={version:5,name:'A',description:'Original',texture:{type:'flat',colour:'#ffffff'}}
    await writeFile(join(dir,'examples/a.json'),JSON.stringify(doc))
    const before=await galleryInputs(dir)
    doc.description='Changed description';await writeFile(join(dir,'examples/a.json'),JSON.stringify(doc))
    await writeFile(join(dir,'frontend/editor.ts'),'changed editor')
    assert.equal(imageKey('a.png',before),imageKey('a.png',await galleryInputs(dir)))
    doc.guide={role:'study',hint:'Changed hint'};await writeFile(join(dir,'examples/a.json'),JSON.stringify(doc))
    assert.equal(imageKey('a.png',before),imageKey('a.png',await galleryInputs(dir)))
    doc.guide.preview={axis:'xz',position:0.5};await writeFile(join(dir,'examples/a.json'),JSON.stringify(doc))
    assert.notEqual(imageKey('a.png',before),imageKey('a.png',await galleryInputs(dir)))
    doc.texture.colour='#ff0000';await writeFile(join(dir,'examples/a.json'),JSON.stringify(doc))
    assert.notEqual(imageKey('a.png',before),imageKey('a.png',await galleryInputs(dir)))
    await writeFile(join(dir,'ramps/grey.json'),'changed ramp')
    assert.notEqual(before.renderer,(await galleryInputs(dir)).renderer)
    await writeFile(join(dir,'src/Texture.hs'),'changed renderer')
    assert.notEqual(before.renderer,(await galleryInputs(dir)).renderer)
  } finally {await rm(dir,{recursive:true,force:true})}
})
