import { createHash } from 'node:crypto'
import { cp, mkdir, readFile, readdir, rm, writeFile } from 'node:fs/promises'
import { basename, join } from 'node:path'
const hash = (value) => createHash('sha256').update(value).digest('hex')
const pngSignature = Buffer.from('89504e470d0a1a0a', 'hex')

async function treeFiles(directory) {
  const files = []
  for (const item of (await readdir(directory, { withFileTypes: true })).sort((a,b) => a.name.localeCompare(b.name))) {
    const path = join(directory, item.name)
    files.push(...(item.isDirectory() ? await treeFiles(path) : [path]))
  }
  return files
}
export async function galleryInputs(repository) {
  // Source/config changes invalidate all renders. Frontend edits do not. Ramp
  // changes invalidate conservatively; examples invalidate their own images.
  const sources = [...await treeFiles(join(repository, 'src')), ...await treeFiles(join(repository, 'ramps')),
    ...['app/Main.hs', 'procedural-textures.cabal', 'stack.yaml', 'stack.yaml.lock'].map(p => join(repository,p))]
  const digest = createHash('sha256').update('gallery-png-v1;size=512;shape-size=1024')
  for (const path of sources) digest.update(path.slice(repository.length)).update(await readFile(path))
  const documents = {}
  for (const name of (await readdir(join(repository, 'examples'))).filter(n => n.endsWith('.json')).sort()) {
    const doc = JSON.parse(await readFile(join(repository, 'examples', name), 'utf8'))
    // Titles, descriptions and categories affect freshly generated HTML only.
    documents[name.slice(0,-5)] = { version: doc.version, texture: doc.texture, ramps: doc.ramps }
  }
  return { renderer: digest.digest('hex'), documents }
}
export function imageKey(name, inputs) {
  if (basename(name) !== name || !name.endsWith('.png')) return undefined
  const stem = name.slice(0,-4)
  const id = Object.keys(inputs.documents).sort((a,b) => b.length-a.length).find(id =>
    stem === id || stem === `${id}-solid` || (stem.startsWith('shape-') && stem.endsWith(`-${id}`)))
  return id ? hash(JSON.stringify([inputs.renderer, name, inputs.documents[id]])) : undefined
}
export async function restoreGalleryImages(cache, output, inputs) {
  let manifest
  try { manifest = JSON.parse(await readFile(join(cache,'manifest.json'),'utf8')) } catch { return 0 }
  if (!manifest || typeof manifest !== 'object') return 0
  let reused = 0
  await mkdir(output, { recursive: true })
  for (const [name, entry] of Object.entries(manifest)) {
    const key = imageKey(name,inputs)
    if (!key || entry?.key !== key || !/^[a-f0-9]{64}$/.test(entry.checksum ?? '')) continue
    try {
      const path = join(cache, `${key}.png`), bytes = await readFile(path)
      if (!bytes.subarray(0,8).equals(pngSignature) || hash(bytes) !== entry.checksum) continue
      await cp(path,join(output,name)); reused++
    } catch { /* A missing/corrupt cache entry is simply rendered again. */ }
  }
  return reused
}
export async function saveGalleryImages(cache, output, inputs) {
  await mkdir(cache, { recursive: true })
  const manifest = {}, retained = new Set(['manifest.json'])
  for (const name of (await readdir(output)).filter(n => n.endsWith('.png'))) {
    const key = imageKey(name,inputs)
    if (!key) throw new Error(`Unknown gallery image: ${name}`)
    const bytes = await readFile(join(output,name))
    if (!bytes.subarray(0,8).equals(pngSignature)) throw new Error(`Invalid gallery PNG: ${name}`)
    const filename = `${key}.png`
    await cp(join(output,name),join(cache,filename))
    manifest[name] = { key, checksum: hash(bytes) }; retained.add(filename)
  }
  await writeFile(join(cache,'manifest.json'),JSON.stringify(manifest,null,2)+'\n')
  for (const name of await readdir(cache)) if (!retained.has(name)) await rm(join(cache,name),{recursive:true,force:true})
  return Object.keys(manifest).length
}
