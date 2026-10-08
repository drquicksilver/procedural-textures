import type {Vector3} from './gpu/material'
export interface BranchConfig {dimensions:number;seed:number;depth:number;length:number;spread:number;taper:number;radius:number}
export interface Segment {a:Vector3;b:Vector3;ra:number;rb:number}
const mix=(x:number):number=>{x=Math.imul(x^(x>>>16),0x7feb352d);x=Math.imul(x^(x>>>15),0x846ca68b);return (x^(x>>>16))>>>0}
export function branchSegments(c:BranchConfig):Segment[]{
 const segments:Segment[]=[]
 const grow=(id:number,left:number,a:Vector3,v:Vector3,len:number,r:number)=>{
  if(!left)return
  const b=v.map((x,i)=>a[i]+len*x) as Vector3,rb=r*c.taper
  segments.push({a,b,ra:r,rb})
  for(const side of [-1,1]){
   const h=mix(c.seed^Math.imul(id,0x8da6b343)^Math.imul(side,0xd8163841)^Math.imul(left,0xcb1ab31f)),rx=(mix(h^0x68bc21eb)&65535)/65536,ry=(mix(h^0x02e5be93)&65535)/65536
   const angle=(side*c.spread+(rx-.5)*12)*Math.PI/180
   const q=[v[0]*Math.cos(angle)-v[1]*Math.sin(angle),v[0]*Math.sin(angle)+v[1]*Math.cos(angle),v[2]+(c.dimensions===2?0:(ry-.5)*.2)]
   const norm=Math.hypot(...q)
   grow(id*2+(side<0?0:1),left-1,b,q.map(x=>x/norm) as Vector3,len*.65,rb)
  }
 }
 grow(1,c.depth,[.5,.06,c.dimensions===2?0:.5],[0,1,0],c.length,c.radius)
 return segments
}
