/** XY layouts share ownership for every projection. */
export const layoutHelpers = `
struct LayoutSample { vec3 local; ivec3 id; float edge; };
LayoutSample layoutSample(vec3 p,int kind) {
 ivec2 id=ivec2(floor(p.xy));vec3 q;float halfY=0.5;
 if(kind==0) q=vec3(p.xy-vec2(id)-0.5,p.z);
 else if(kind==1){float shift=0.5*float((id.y%2+2)%2);id.x=int(floor(p.x-shift));q=vec3(p.xy-vec2(id)-vec2(0.5+shift,0.5),p.z);}
 else if(kind==3){int d=((id.x-id.y)%4+4)%4;if(d==3)id.y--;if(d==2)id.x--;q=(d==0||d==3)?vec3(p.xy-vec2(id)-vec2(0.5,1),p.z):vec3(p.y-float(id.y)-0.5,float(id.x)+1.0-p.x,p.z);halfY=1.0;}
 else {float h=0.8660254037844386;int j=int(floor(p.y/h+0.5));int i=int(floor(p.x-0.5*float(j)+0.5));float best=1e30;
 for(int a=-1;a<=1;a++)for(int b=-1;b<=1;b++){ivec2 c=ivec2(i+a,j+b);vec3 v=vec3(p.x-float(c.x)-0.5*float(c.y),p.y-h*float(c.y),p.z);float d=dot(v.xy,v.xy);if(d<best){best=d;id=c;q=v;}}
 return LayoutSample(q,ivec3(id,0),max(0.0,0.5-max(abs(q.x),max(abs(0.5*q.x+h*q.y),abs(0.5*q.x-h*q.y)))));}
 return LayoutSample(q,ivec3(id,0),max(0.0,min(0.5-abs(q.x),halfY-abs(q.y))));
}
`;
