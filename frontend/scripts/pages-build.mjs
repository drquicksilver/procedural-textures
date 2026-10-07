// Assemble exactly what Pages publishes; gallery rendering remains a build-time tool.
import { cp, mkdir, readFile, readdir, rm, writeFile } from 'node:fs/promises'
import { execFileSync } from 'node:child_process'
import { join } from 'node:path'
import { root } from './paths.mjs'
import { cliDirectory } from './cli-tools.mjs'
import { galleryInputs, restoreGalleryImages, saveGalleryImages } from './gallery-cache.mjs'
const output = join(root, '../out/pages'), gallery = join(output, 'gallery')
await rm(output, { recursive: true, force: true }); await mkdir(output, { recursive: true })
await cp(join(root, 'dist'), output, { recursive: true })
const repository = join(root, '..'), cache = join(repository, 'out/gallery-cache')
const inputs = await galleryInputs(repository)
const reused = await restoreGalleryImages(cache, gallery, inputs)
execFileSync(join(cliDirectory(), 'procedural-textures'), ['gallery', '--out', gallery, '--reuse-images'], { cwd: repository, stdio: 'inherit' })
const total = await saveGalleryImages(cache, gallery, inputs)
console.log(`Gallery image cache: ${reused} reused, ${total - reused} rendered`)
for (const file of await readdir(gallery)) {
  if (!file.endsWith('.html')) continue
  const path = join(gallery, file), html = await readFile(path, 'utf8')
  await writeFile(path, html.replace('  <header>', '  <header>\n    <nav><a href="../index.html">Open texture editor</a></nav>'))
  // Keep previously published gallery/shape URLs working after moving the gallery.
  if (file !== 'index.html') await writeFile(join(output, file), `<!doctype html><html lang="en"><meta charset="utf-8"><meta http-equiv="refresh" content="0;url=./gallery/${file}"><title>Procedural Textures gallery</title><a href="./gallery/${file}">Open gallery</a></html>\n`)
}
await writeFile(join(output, '.nojekyll'), '')
console.log(`Static Pages artifact: ${output}`)
