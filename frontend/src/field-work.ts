/** Conservative input-query bound shared with Texture.scalarWork. */
export function fieldWork(value:unknown):number {
 if(!value||typeof value!=='object'||Array.isArray(value))return 0
 const n=value as Record<string,unknown>
 if(n.type==='reaction-diffusion'||n.type==='field-reaction')return 8
 if(n.type==='gradient'||n.type==='curl')return 6*fieldWork(n.source)
 if(n.type==='branch-distance')return 2**Number(n.depth)-1
 if(typeof n.type==='string'&&n.type.startsWith('layout-'))return n.layout==='hex'?9:1
 if(['worley','cell-value','cell-id','cell-colour'].includes(String(n.type)))return n.dimensions===2?49:343
 if(n.type==='cell-edge')return n.dimensions===2?227:2567
 if(n.type==='periodic-fractal')return Number(n.octaves)
 if(n.type==='fractal'||n.type==='absolute-fractal')return Math.max(1,Number(n.octaves))*fieldWork(n.source)
 const children=Object.values(n).reduce<number>((sum,v)=>sum+fieldWork(v),0)
 return Math.max(1,children)+(n.type==='rotate-field'||n.type==='warp'?1:0)
}
