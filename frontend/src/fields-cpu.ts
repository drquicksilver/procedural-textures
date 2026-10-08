/** Double-valued typed fields for worker-side simulation inputs. */
import type {ScalarField,VectorField,Domain,Vector3,Layout} from './gpu/material'
import {permutation} from './gpu/permutation'
import {branchSegments} from './branching'
type Point=readonly number[]
type Fn<T>=(p:Point)=>T
export type ReactionSampler=(config:unknown,p:Point,lane:number)=>number
const add=(a:Point,b:Point)=>a.map((x,i)=>x+b[i]) as Vector3
const sub=(a:Point,b:Point)=>a.map((x,i)=>x-b[i]) as Vector3
const mul=(a:Point,v:number)=>a.map(x=>x*v) as Vector3
const dot=(a:Point,b:Point)=>a.reduce((s,x,i)=>s+x*b[i],0)
const norm=(a:Point)=>Math.sqrt(dot(a,a))
const normal=(a:Point)=>norm(a)===0?[0,0,0] as Vector3:mul(a,1/norm(a))
const cross=(a:Point,b:Point):Vector3=>[a[1]*b[2]-a[2]*b[1],a[2]*b[0]-a[0]*b[2],a[0]*b[1]-a[1]*b[0]]
const clamp=(x:number,lo=0,hi=1)=>Math.max(lo,Math.min(hi,x))
const finite=(x:number)=>Number.isFinite(x)&&Math.abs(x)<=3.4028234663852886e38?x:0
const fract=(x:number)=>x-Math.floor(x)
const mod=(x:number,n:number)=>((x%n)+n)%n
const mix=(x:number)=>{x=Math.imul(x^(x>>>16),0x7feb352d);x=Math.imul(x^(x>>>15),0x846ca68b);return(x^(x>>>16))>>>0}
const hash=(seed:number,p:Point)=>mix(seed^Math.imul(p[0],0x8da6b343)^Math.imul(p[1],0xd8163841)^Math.imul(p[2],0xcb1ab31f))
const random=(h:number):Vector3=>[0x68bc21eb,0x02e5be93,0x967a889b].map(s=>(mix(h^s)&65535)/65536) as Vector3
const gradients=Array.from({length:32},(_,k)=>{const z=1-2*(k+.5)/32,r=Math.sqrt(1-z*z),a=k*Math.PI*(3-Math.sqrt(5))+.37,x=r*Math.cos(a),y=r*Math.sin(a),u=x*Math.cos(.41)-z*Math.sin(.41),v=x*Math.sin(.41)+z*Math.cos(.41);return[u,y*Math.cos(.29)-v*Math.sin(.29),y*Math.sin(.29)+v*Math.cos(.29)]})
const fade=(x:number)=>x*x*x*(x*(x*6-15)+10)
const lerp=(a:number,b:number,t:number)=>a+(b-a)*t
function noise(p:Point,period?:Point):number{
 const base=p.map(Math.floor),q=p.map((x,i)=>x-base[i]),t=q.map(fade)
 const corner=(x:number,y:number,z:number)=>{const d=[x,y,z],id=base.map((v,i)=>(period?mod(v+d[i],period[i]):v+d[i])&255),h=permutation[(permutation[(permutation[id[0]]+id[1])&255]+id[2])&255];return dot(gradients[h&31],sub(q,d))}
 const plane=(z:number)=>lerp(lerp(corner(0,0,z),corner(1,0,z),t[0]),lerp(corner(0,1,z),corner(1,1,z),t[0]),t[1])
 return clamp(.5+.8*lerp(plane(0),plane(1),t[2]))
}
function rotate(p:Point,axis:number,a:number):Vector3{
 const c=Math.cos(a),s=Math.sin(a),q=[...p] as Vector3,j=(axis+1)%3,k=(axis+2)%3;q[j]=p[j]*c-p[k]*s;q[k]=p[j]*s+p[k]*c;return q
}
function layout(kind:Layout,p:Point):{local:Vector3;id:Vector3;edge:number}{
 let i=Math.floor(p[0]),j=Math.floor(p[1]),q:Vector3,halfY=.5
 if(kind==='grid')q=[p[0]-i-.5,p[1]-j-.5,p[2]]
 else if(kind==='running-bond'){const shift=.5*mod(j,2);i=Math.floor(p[0]-shift);q=[p[0]-i-shift-.5,p[1]-j-.5,p[2]]}
 else if(kind==='herringbone'){const d=mod(i-j,4);if(d===3)j--;if(d===2)i--;q=d===0||d===3?[p[0]-i-.5,p[1]-j-1,p[2]]:[p[1]-j-.5,i+1-p[0],p[2]];halfY=1}
 else{const h=Math.sqrt(3)/2,j0=Math.floor(p[1]/h+.5),i0=Math.floor(p[0]-.5*j0+.5);let best=Infinity;q=[0,0,p[2]]
 for(let a=i0-1;a<=i0+1;a++)for(let b=j0-1;b<=j0+1;b++){const v:Vector3=[p[0]-a-.5*b,p[1]-h*b,p[2]],d=v[0]*v[0]+v[1]*v[1];if(d<best){best=d;i=a;j=b;q=v}}
 return{local:q,id:[i,j,0],edge:Math.max(0,.5-Math.max(Math.abs(q[0]),Math.abs(.5*q[0]+h*q[1]),Math.abs(.5*q[0]-h*q[1])))}}
 return{local:q,id:[i,j,0],edge:Math.max(0,Math.min(.5-Math.abs(q[0]),halfY-Math.abs(q[1])))}
}
function cellular(n:{dimensions:number;jitter:number;seed:number},input:Point,metric='euclidean'){
 const p=[input[0],input[1],n.dimensions===2?0:input[2]],base=p.map(Math.floor)
 const distance=(q:Point)=>metric==='manhattan'?q.reduce((s,v)=>s+Math.abs(v),0):metric==='chebyshev'?Math.max(...q.map(Math.abs)):norm(q)
 const site=(id:Point)=>{const r=random(hash(n.seed,id));return id.map((x,i)=>i===2&&n.dimensions===2?0:x+.5+clamp(n.jitter)*(r[i]-.5))}
 let first=Infinity,second=Infinity,id:Vector3=[0,0,0],nearest:Point=[0,0,0]
 const cells=(radius:number,fn:(id:Vector3)=>void)=>{for(let x=base[0]-radius;x<=base[0]+radius;x++)for(let y=base[1]-radius;y<=base[1]+radius;y++)for(let z=n.dimensions===2?0:base[2]-radius;z<=(n.dimensions===2?0:base[2]+radius);z++)fn([x,y,z])}
 cells(3,c=>{const q=site(c),d=distance(sub(q,p));if(d<first){second=first;first=d;id=c;nearest=q}else if(d<second)second=d})
 const edge=()=>{let best=Math.sqrt(n.dimensions);cells(6,c=>{if(c.every((x,i)=>x===id[i]))return;const r=site(c),v=sub(r,nearest),len=norm(v);if(len>=1e-12)best=Math.min(best,Math.max(0,dot(sub(mul(add(nearest,r),.5),p),v)/len))});return best}
 return{first,second,id,edge}
}
export function compileScalar(n:ScalarField,reaction:ReactionSampler=()=>{throw new Error('Reaction dependency not prepared')}):Fn<number>{
 const s=(v:ScalarField)=>compileScalar(v,reaction),v=(f:VectorField)=>compileVector(f,reaction),d=(f:Domain)=>compileDomain(f,reaction)
 switch(n.type){
 case 'constant':return()=>n.value
 case 'noise':return p=>noise(p)
 case 'periodic-noise':return p=>noise(p,[n.periodX,n.periodY,n.periodZ])
 case 'periodic-fractal':return p=>{let value=0,total=0,amp=1,freq=1;for(let i=0;i<n.octaves;i++){let q=noise(mul(p,freq),[n.periodX*freq,n.periodY*freq,n.periodZ*freq]);if(n.style==='billowy')q=Math.abs(2*q-1);if(n.style==='ridged')q=(1-Math.abs(2*q-1))**2;value+=amp*q;total+=amp;amp*=n.persistence;freq*=n.lacunarity}return value/total}
 case 'planar':{const dir=sub(n.to,n.from),len=dot(dir,dir);return p=>len<=0?0:dot(sub(p,n.from),dir)/len}
 case 'distance':return p=>n.radius<=0?0:norm(sub(p,n.centre))/n.radius
 case 'angular':{const axis=normal(n.axis),project=(p:Point)=>sub(p,mul(axis,dot(p,axis))),candidate=project([0,-1,0]),north=normal(norm(candidate)<1e-9?project([0,0,1]):candidate);return p=>{const r=project(sub(p,n.centre)),len=norm(r);return len<=0?.5:(1-dot(north,r)/len)/2}}
 case 'sphere':return p=>norm(sub(p,n.centre))-n.radius
 case 'box':return p=>{const q=sub(p,n.centre).map((x,i)=>Math.abs(x)-n.half[i]);return norm(q.map(x=>Math.max(0,x)))+Math.min(Math.max(...q),0)}
 case 'cylinder':return p=>{const q=sub(p,n.centre),a=Math.hypot(q[0],q[2])-n.radius,b=Math.abs(q[1])-n.height;return Math.hypot(Math.max(a,0),Math.max(b,0))+Math.min(Math.max(a,b),0)}
 case 'torus':return p=>{const q=sub(p,n.centre);return Math.hypot(Math.hypot(q[0],q[2])-n.major,q[1])-n.minor}
 case 'plane':{const axis=normal(n.normal);return p=>dot(p,axis)-n.offset}
 case 'sdf-union':case 'sdf-intersection':case 'sdf-difference':{const a=s(n.a),b=s(n.b);const minimum=(a:number,b:number)=>{if(n.amount<=0)return Math.min(a,b);const h=clamp(.5+.5*(b-a)/n.amount);return lerp(b,a,h)-n.amount*h*(1-h)};return p=>n.type==='sdf-union'?minimum(a(p),b(p)):-minimum(-a(p),n.type==='sdf-difference'?b(p):-b(p))}
 case 'scalar-domain':{const source=s(n.source),domain=d(n.domain);return p=>source(domain(p))}
 case 'add':case 'multiply':case 'min':case 'max':{const a=s(n.a),b=s(n.b);return p=>n.type==='add'?a(p)+b(p):n.type==='multiply'?a(p)*b(p):n.type==='min'?Math.min(a(p),b(p)):Math.max(a(p),b(p))}
 case 'remap':{const f=s(n.source);return p=>n.high===n.low?n.outLow:n.outLow+(f(p)-n.low)/(n.high-n.low)*(n.outHigh-n.outLow)}
 case 'sin':case 'cos':case 'abs':case 'floor':case 'fract':{const f=s(n.source),fn=n.type==='fract'?fract:Math[n.type];return p=>finite(fn(finite(f(p))))}
 case 'divide':case 'power':{const a=s(n.a),b=s(n.b);return p=>{const x=a(p),y=b(p);return n.type==='divide'?Math.abs(y)<=1e-8?0:finite(x/y):finite(x===0&&y<0||x<0&&!Number.isInteger(y)?0:x**y)}}
 case 'lerp':{const a=s(n.a),b=s(n.b),t=s(n.amount);return p=>finite(lerp(a(p),b(p),t(p)))}
 case 'clamp':{const f=s(n.source);return p=>clamp(finite(f(p)),Math.min(n.low,n.high),Math.max(n.low,n.high))}
 case 'azimuth':return p=>{const q=sub(p,n.centre);return q[0]===0&&q[1]===0?0:fract(Math.atan2(q[1],q[0])/(2*Math.PI))}
 case 'component':{const f=v(n.source),i=['x','y','z'].indexOf(n.axis);return p=>f(p)[i]}
 case 'threshold':{const f=s(n.source);return p=>{const x=f(p),t=n.high===n.low?x<n.low?0:1:clamp((x-n.low)/(n.high-n.low));return t*t*(3-2*t)}}
 case 'fractal':case 'absolute-fractal':{const f=s(n.source),count=Math.max(1,n.octaves),absolute=n.type==='absolute-fractal';return p=>{let sum=0,total=0,amp=1;for(let i=0;i<count;i++){let q=mul(rotate(rotate(rotate(p,2,i*.83),0,i*.83*.71),1,i*.83*.53),n.lacunarity**i);if(!absolute)q=add(q,mul([31.7,17.3,11.9],i));const v=f(q),shaped=absolute||n.style==='billowy'?Math.abs(2*v-1):n.style==='ridged'?(1-Math.abs(2*v-1))**2:v;sum+=amp*shaped;total+=amp;amp*=n.persistence}if(absolute)total=n.persistence===1?count:(1-n.persistence**count)/(1-n.persistence);const value=total<=0?absolute?0:.5:sum/total;return absolute?value:clamp(n.style==='smooth'?.5+(value-.5)*2:n.style==='billowy'?value*1.75:(value-.2)/.72)}}
 case 'worley':return p=>{const c=cellular(n,p,n.metric);return n.output==='f1'?c.first:n.output==='f2'?c.second:c.second-c.first}
 case 'cell-value':return p=>(hash(n.seed,cellular(n,p).id)&65535)/65536
 case 'cell-edge':return p=>cellular(n,p).edge()
 case 'layout-edge':return p=>layout(n.layout,p).edge
 case 'layout-value':return p=>(hash(n.seed,layout(n.layout,p).id)&65535)/65536
 case 'branch-distance':{const segments=branchSegments(n);return input=>{const p=n.dimensions===2?[input[0],input[1],0]:input;return Math.min(...segments.map(({a,b,ra,rb})=>{const q=sub(b,a),len=dot(q,q),t=len===0?0:clamp(dot(sub(p,a),q)/len);return norm(sub(p,add(a,mul(q,t))))-lerp(ra,rb,t)}))}}
 case 'field-reaction':case 'reaction-diffusion':return p=>reaction(n,p,n.output==='u'?0:1)
 default:throw new Error(`Unsupported CPU field ${(n as ScalarField).type}`)
 }
}
export function compileVector(n:VectorField,reaction?:ReactionSampler):Fn<Vector3>{
 switch(n.type){
 case 'vector-constant':return()=>n.value
 case 'position':return p=>[...p] as Vector3
 case 'components':{const f=[n.x,n.y,n.z].map(x=>compileScalar(x,reaction));return p=>f.map(g=>g(p)) as Vector3}
 case 'vector-add':{const a=compileVector(n.a,reaction),b=compileVector(n.b,reaction);return p=>add(a(p),b(p))}
 case 'vector-scale':{const f=compileScalar(n.amount,reaction),v=compileVector(n.source,reaction);return p=>mul(v(p),f(p))}
 case 'vector-domain':{const d=compileDomain(n.domain,reaction),v=compileVector(n.source,reaction);return p=>v(d(p))}
 case 'cell-id':return p=>cellular(n,p).id
 case 'cell-colour':return p=>sub(mul(random(hash(n.seed,cellular(n,p).id)),2),[1,1,1])
 case 'layout-id':return p=>layout(n.layout,p).id
 case 'layout-coordinates':return p=>layout(n.layout,p).local
 }
}
export function compileDomain(n:Domain,reaction?:ReactionSampler):Fn<Vector3>{
 const radians=Math.PI/180
 switch(n.type){
 case 'translate':return p=>sub(p,n.offset)
 case 'scale':return p=>p.map((x,i)=>n.scale[i]===0?0:x/n.scale[i]) as Vector3
 case 'rotate':return p=>rotate(rotate(rotate(p,2,-n.rotation[2]*radians),1,-n.rotation[1]*radians),0,-n.rotation[0]*radians)
 case 'repeat':return p=>p.map((x,i)=>n.period[i]<=0?x:x-n.period[i]*Math.floor(x/n.period[i]+.5)) as Vector3
 case 'mirror':return p=>add(n.centre,sub(p,n.centre).map((x,i)=>n.axes[i]>=.5?Math.abs(x):x))
 case 'polar-repeat':return p=>{const q=sub(p,n.centre),r=Math.hypot(q[0],q[1]),s=2*Math.PI/Math.max(1,n.count),angle=r===0?0:Math.atan2(q[1],q[0]),a=angle-s*Math.floor(angle/s+.5);return add(n.centre,[r*Math.cos(a),r*Math.sin(a),q[2]])}
 case 'radial-repeat':return p=>{const q=sub(p,n.centre),r=Math.hypot(q[0],q[1]),v=n.period<=0?r:mod(r,n.period);return add(n.centre,[r===0?0:q[0]*v/r,r===0?0:q[1]*v/r,q[2]])}
 case 'twist':case 'bend':return p=>{const q=sub(p,n.centre);return add(n.centre,rotate(q,2,-n.amount*q[n.type==='twist'?2:0]*radians))}
 case 'compose':{const a=compileDomain(n.first,reaction),b=compileDomain(n.second,reaction);return p=>b(a(p))}
 case 'warp':{const v=compileVector(n.field,reaction);return p=>add(p,mul(v(p),n.amount))}
 case 'layout-domain':return p=>layout(n.layout,p).local
 case 'rotate-field':{const f=compileScalar(n.angle,reaction),axis=normal(n.axis);return p=>{if(norm(n.axis)===0)return [...p] as Vector3;const q=sub(p,n.centre),a=-finite(f(p))*radians,c=Math.cos(a);return add(n.centre,add(mul(q,c),add(mul(cross(axis,q),Math.sin(a)),mul(axis,(1-c)*dot(axis,q)))))}}
 }
}
