/** Integer hashing and bounded, pruned search mirror Cellular.hs. */
export const cellularHelpers = `
uint cellMix(uint x) { x=(x^(x>>16u))*0x7feb352du; x=(x^(x>>15u))*0x846ca68bu; return x^(x>>16u); }
uint cellHash(ivec3 c,uint seed) { return cellMix(seed^(uint(c.x)*0x8da6b343u)^(uint(c.y)*0xd8163841u)^(uint(c.z)*0xcb1ab31fu)); }
vec3 cellRandom(uint h) { return vec3(float(cellMix(h^0x68bc21ebu)&65535u),float(cellMix(h^0x02e5be93u)&65535u),float(cellMix(h^0x967a889bu)&65535u))/65536.0; }
uint cellSeed(vec4 c) { return uint(c.z)|(uint(c.w)<<16u); }
vec3 cellFeature(ivec3 cell,vec4 c) { vec3 q=vec3(cell)+0.5+clamp(c.y,0.0,1.0)*(cellRandom(cellHash(cell,cellSeed(c)))-0.5); if(c.x==2.0) q.z=0.0; return q; }
float cellMetric(vec3 q,int m) { q=abs(q); return m==1 ? q.x+q.y+q.z : m==2 ? max(q.x,max(q.y,q.z)) : length(q); }
vec3 cellBox(vec3 p,ivec3 cell,bool two) { vec3 q=max(vec3(0),max(vec3(cell)-p,p-vec3(cell)-1.0)); if(two) q.z=0.0; return q; }
struct CellSample { float first; float second; ivec3 id; vec3 site; };
CellSample cellular(vec3 point,vec4 c,int metric) {
  bool two=c.x==2.0; vec3 p=point; if(two) p.z=0.0;
  ivec3 base=ivec3(floor(p)); CellSample s=CellSample(1e30,1e30,ivec3(0),vec3(0));
  for(int x=-3;x<=3;x++) for(int y=-3;y<=3;y++) for(int z=-3;z<=3;z++) {
    if(two && z!=0) continue;
    ivec3 id=base+ivec3(x,y,z);
    if(cellMetric(cellBox(p,id,two),metric)>s.second) continue;
    vec3 q=cellFeature(id,c); float d=cellMetric(q-p,metric);
    if(d<s.first) { s.second=s.first; s.first=d; s.id=id; s.site=q; }
    else if(d<s.second) s.second=d;
  }
  return s;
}
float cellEdge(vec3 point,vec4 c) {
  bool two=c.x==2.0; vec3 p=point; if(two) p.z=0.0;
  CellSample s=cellular(p,c,0); ivec3 base=ivec3(floor(p)); float best=sqrt(two ? 2.0 : 3.0);
  for(int x=-1;x<=1;x++) for(int y=-1;y<=1;y++) for(int z=-1;z<=1;z++) {
    if(two && z!=0) continue;
    ivec3 id=s.id+ivec3(x,y,z); if(all(equal(id,s.id))) continue;
    vec3 r=cellFeature(id,c),v=r-s.site; float len=length(v);
    if(len>1e-12) best=min(best,max(0.0,dot(0.5*(s.site+r)-p,v)/len));
  }
  int radius=min(6,int(ceil(s.first+2.0*best)));
  for(int x=-radius;x<=radius;x++) for(int y=-radius;y<=radius;y++) for(int z=-radius;z<=radius;z++) {
    if(two && z!=0) continue;
    ivec3 id=base+ivec3(x,y,z);
    if(all(equal(id,s.id)) || length(cellBox(p,id,two))>s.first+2.0*best) continue;
    vec3 r=cellFeature(id,c),v=r-s.site; float len=length(v);
    if(len>1e-12) best=min(best,max(0.0,dot(0.5*(s.site+r)-p,v)/len));
  }
  return best;
}
`
