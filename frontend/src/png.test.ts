// @vitest-environment node
import {expect,it} from 'vitest'
import {inflateSync} from 'node:zlib'
import {rgbaPng} from './png'
it('preserves straight-alpha RGB bytes, including tiny and zero alpha', async () => {
  const pixels=new Uint8Array([17,43,221,7,255,1,64,0,91,203,4,128,11,12,13,255])
  const blob=await rgbaPng(pixels,2,2),bytes=new Uint8Array(await blob.arrayBuffer())
  expect(blob.type).toBe('image/png'); expect([...bytes.slice(0,8)]).toEqual([137,80,78,71,13,10,26,10])
  const view=new DataView(bytes.buffer);let at=8,compressed:Uint8Array|undefined
  while(at<bytes.length) { const count=view.getUint32(at),kind=new TextDecoder().decode(bytes.slice(at+4,at+8)); if(kind==='IDAT') compressed=bytes.slice(at+8,at+8+count); at+=count+12 }
  expect([...inflateSync(compressed!)]).toEqual([0,...pixels.slice(0,8),0,...pixels.slice(8)])
})
it('rejects dimensions that do not describe the supplied pixels', async () => {
  await expect(rgbaPng(new Uint8Array(4),2)).rejects.toThrow('Invalid PNG')
})
