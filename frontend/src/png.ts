/** Lossless straight-alpha RGBA PNG; Canvas2D would round via premultiplication. */
const encoder=new TextEncoder()
function crc(bytes: Uint8Array): number {
  let c=0xffffffff
  for(const byte of bytes) { c^=byte; for(let i=0;i<8;i++) c=(c>>>1)^((c&1) ? 0xedb88320 : 0) }
  return (c^0xffffffff)>>>0
}
function chunk(kind:string,data:Uint8Array): Uint8Array<ArrayBuffer> {
  const result=new Uint8Array(data.length+12),view=new DataView(result.buffer)
  view.setUint32(0,data.length); result.set(encoder.encode(kind),4); result.set(data,8)
  view.setUint32(data.length+8,crc(result.subarray(4,data.length+8)))
  return result
}
export async function rgbaPng(pixels:Uint8Array,width:number,height=width): Promise<Blob> {
  if(!Number.isInteger(width)||!Number.isInteger(height)||width<1||height<1||width>2048||height>2048||pixels.length!==width*height*4) throw new Error('Invalid PNG dimensions or RGBA bytes')
  const rows=new Uint8Array((width*4+1)*height)
  for(let y=0;y<height;y++) rows.set(pixels.subarray(y*width*4,(y+1)*width*4),y*(width*4+1)+1)
  const compressed=new Uint8Array(await new Response(new Blob([rows]).stream().pipeThrough(new CompressionStream('deflate'))).arrayBuffer())
  const header=new Uint8Array(13),view=new DataView(header.buffer)
  view.setUint32(0,width); view.setUint32(4,height); header[8]=8; header[9]=6
  return new Blob([new Uint8Array([137,80,78,71,13,10,26,10]),chunk('IHDR',header),chunk('IDAT',compressed),chunk('IEND',new Uint8Array())],{type:'image/png'})
}
export async function pngDataUrl(blob:Blob): Promise<string> {
  const bytes=new Uint8Array(await blob.arrayBuffer()); let binary=''
  for(let i=0;i<bytes.length;i+=8192) binary+=String.fromCharCode(...bytes.subarray(i,i+8192))
  return 'data:image/png;base64,'+btoa(binary)
}
