import { currentVersion } from '../version'
// Secondary, self-running editor smoke checks for native Safari and Firefox.
const status = document.querySelector('#status')!
const frame = document.createElement('iframe')
frame.style.cssText = 'width:1400px;height:900px;border:0'
frame.src = '/'
document.body.append(frame)
const pause = (ms = 50) => new Promise((resolve) => setTimeout(resolve, ms))
async function until(check: () => unknown, label: string) {
  const started = performance.now()
  while (!check()) {
    if (performance.now() - started > 60000) throw new Error(`Timed out: ${label}`)
    await pause()
  }
}
const doc = () => frame.contentDocument!
const query = <T extends Element>(selector: string) => doc().querySelector<T>(selector)!
const button = (label: string) => [...doc().querySelectorAll<HTMLButtonElement>('button')].find((b) => b.textContent?.trim() === label)!
try {
  await until(() => doc()?.querySelector('.preview canvas[data-rendered]'), 'first render')
  if (query<HTMLSelectElement>('select[aria-label="Shape"]').options.length !== 13) throw new Error('Missing shapes')
  button('Library…').click()
  await until(() => doc().querySelector('.document-card'), 'library')
  const checker = [...doc().querySelectorAll<HTMLButtonElement>('.document-card')].find((c) => c.textContent?.trim() === 'Checker')!
  checker.click()
  await until(() => doc().querySelector('button[aria-label="Increase Columns"]'), 'checker inspector')
  query<HTMLButtonElement>('button[aria-label="Increase Columns"]').click()
  await until(() => query<HTMLInputElement>('.inspector .number-input').value === '9', 'edit')
  button('Undo').click()
  await until(() => query<HTMLInputElement>('.inspector .number-input').value === '8', 'undo')
  button('Redo').click()
  await until(() => query<HTMLInputElement>('.inspector .number-input').value === '9', 'redo')
  await pause(800)
  await new Promise<void>((resolve) => { frame.addEventListener('load', () => resolve(), { once: true }); frame.src = '/' })
  await until(() => doc()?.querySelector<HTMLInputElement>('.inspector .number-input')?.value === '9', 'persisted edit')
  await until(() => doc().querySelector('.preview canvas[data-rendered]'), 'restored render')
  const current = frame.contentWindow as Window & typeof globalThis
  button('Library…').click()
  await until(() => doc().querySelector('.document-card'), 'typed example library')
  ;[...doc().querySelectorAll<HTMLButtonElement>('.document-card')].find((c) => c.textContent?.trim() === 'Gated alpine')!.click()
  await until(() => query<HTMLSelectElement>('[aria-label="Texture type"]').value === 'colourise', 'typed colour root')
  const child = async (label: string, type: string) => {
    [...doc().querySelectorAll<HTMLButtonElement>('.child-link')].find((c) => c.querySelector('.field-label')?.textContent === label)!.click()
    await until(() => query<HTMLSelectElement>('[aria-label="Texture type"]').value === type, `selected ${label}`)
  }
  await child('Field', 'add'); await child('B', 'multiply'); await child('A', 'constant')
  if (query<HTMLSelectElement>('[aria-label="Texture type"]').value !== 'constant') throw new Error('Typed scalar traversal failed')
  if ([...query<HTMLSelectElement>('[aria-label="Texture type"]').options].some((o) => o.value === 'flat')) throw new Error('Colour offered in scalar slot')
  query<HTMLInputElement>('[aria-label="Inspect selected field"]').click()
  const input = query<HTMLInputElement>('input[aria-label="Value"]')
  input.value = '0.12'; input.dispatchEvent(new current.Event('input', { bubbles: true }))
  await pause()
  button('Undo').click()
  await until(() => query<HTMLInputElement>('input[aria-label="Value"]').value === '0.18', 'typed undo')
  button('Redo').click()
  await until(() => query<HTMLInputElement>('input[aria-label="Value"]').value === '0.12', 'typed redo')
  await until(() => query('.save-status').textContent === 'Saved', 'typed persisted edit')
  query<HTMLInputElement>('[aria-label="Inspect selected field"]').click()
  const transfer = new current.DataTransfer()
  transfer.items.add(new current.File([JSON.stringify({ version: 4, name: 'Compatibility alpha', description: '', texture: { type: 'flat', colour: '#ff000080' } })], 'alpha.json', { type: 'application/json' }))
  query<HTMLInputElement>('input[type=file]').files = transfer.files
  query<HTMLInputElement>('input[type=file]').dispatchEvent(new current.Event('change', { bubbles: true }))
  await until(() => query<HTMLInputElement>('.document-name').value === 'Compatibility alpha', 'import')
  const downloads: { name: string; blob: Blob }[] = []
  current.HTMLAnchorElement.prototype.click = function () {
    if (this.download) void current.fetch(this.href).then((r) => r.blob()).then((blob) => downloads.push({ name: this.download, blob }))
  }
  button('Export').click()
  await until(() => downloads.some((d) => d.name.endsWith('.json')), 'JSON export')
  const exported = JSON.parse(await downloads.find((d) => d.name.endsWith('.json'))!.blob.text())
  if (exported.version !== currentVersion || exported.texture.colour !== '#ff000080') throw new Error('JSON export mismatch')
  for (const [label, value] of [['View', 'slice'], ['PNG resolution', '256']]) {
    const select = query<HTMLSelectElement>(`select[aria-label="${label}"]`)
    select.value = value; select.dispatchEvent(new current.Event('change', { bubbles: true })); await pause()
  }
  button('Download PNG').click()
  await until(() => downloads.some((d) => d.name.endsWith('.png')), 'PNG export')
  const image = await current.createImageBitmap(downloads.find((d) => d.name.endsWith('.png'))!.blob)
  const canvas = doc().createElement('canvas'); canvas.width = image.width; canvas.height = image.height
  const ctx = canvas.getContext('2d')!; ctx.drawImage(image, 0, 0)
  const pixel = [...ctx.getImageData(128, 128, 1, 1).data]; image.close()
  if (canvas.width !== 256 || canvas.height !== 256 || pixel.join(',') !== '255,0,0,128') throw new Error(`PNG mismatch: ${pixel}`)
  frame.style.width = '390px'
  await pause(300)
  if (doc().documentElement.scrollWidth > current.innerWidth || query('.preview').getBoundingClientRect().width < 300) throw new Error('Narrow layout overflow')
  const result = { userAgent: navigator.userAgent, checks: ['WebGL2 render', '13 shapes', 'example library', 'edit', 'undo/redo', 'reload/persistence', 'typed scalar editing/inspection', 'JSON import/export', '256px transparent PNG export', '390px layout'] }
  status.textContent = `PASS: ${result.checks.join(', ')}`
  await fetch(`/__compatibility/${new URLSearchParams(location.search).get('token')}`, { method: 'POST', body: JSON.stringify(result) })
} catch (error) {
  status.textContent = String(error)
  await fetch(`/__compatibility/${new URLSearchParams(location.search).get('token')}`, { method: 'POST', body: JSON.stringify({ error: error instanceof Error ? error.stack : String(error), userAgent: navigator.userAgent }) })
}
