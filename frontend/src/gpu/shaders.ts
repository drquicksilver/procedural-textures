import { permutation } from './permutation'

const gradients = Array.from({ length: 32 }, (_, k) => {
  const z = 1 - 2 * (k + 0.5) / 32
  const r = Math.sqrt(1 - z * z)
  const a = k * Math.PI * (3 - Math.sqrt(5)) + 0.37
  const x = r * Math.cos(a), y = r * Math.sin(a)
  const u = x * Math.cos(0.41) - z * Math.sin(0.41)
  const v = x * Math.sin(0.41) + z * Math.cos(0.41)
  return [u, y * Math.cos(0.29) - v * Math.sin(0.29), y * Math.sin(0.29) + v * Math.cos(0.29)]
})

export const noiseLookup = new Float32Array(512 * 4)
permutation.forEach((n, i) => { noiseLookup[i * 4] = n })
gradients.forEach((v, i) => noiseLookup.set(v, (256 + i) * 4))

export const vertexShader = `#version 300 es
void main() {
  vec2 p = vec2(float((gl_VertexID << 1) & 2), float(gl_VertexID & 2));
  gl_Position = vec4(p * 2.0 - 1.0, 0.0, 1.0);
}`

export const helpers = `#version 300 es
precision highp float;
precision highp int;
uniform highp sampler2D parameters;
uniform vec2 resolution;
uniform vec3 cameraOrigin,cameraForward,cameraRight,cameraDown;
uniform int sliceAxis; // -1 is scene, 0/1/2 are XY/XZ/YZ
uniform float slicePosition;
out vec4 outputColour;
vec4 data(int i) { return texelFetch(parameters, ivec2(i % 256, i / 256), 0); }
// Immutable lookup rows share the parameter texture. Dynamic constant-array
// indexing otherwise expands into large selection trees on some backends.
int perm(int i) { return int(data(i & 255).x); }
float corner(ivec3 cell, vec3 p, ivec3 d) {
  int hash = perm(perm(perm(cell.x+d.x)+cell.y+d.y)+cell.z+d.z);
  return dot(data(256+(hash & 31)).xyz, p-vec3(d));
}
float noise3(vec3 p) {
  ivec3 cell = ivec3(floor(p)) & ivec3(255);
  vec3 f = fract(p), t = f*f*f*(f*(f*6.0-15.0)+10.0);
  float a = mix(mix(corner(cell,f,ivec3(0,0,0)),corner(cell,f,ivec3(1,0,0)),t.x),
                mix(corner(cell,f,ivec3(0,1,0)),corner(cell,f,ivec3(1,1,0)),t.x),t.y);
  float b = mix(mix(corner(cell,f,ivec3(0,0,1)),corner(cell,f,ivec3(1,0,1)),t.x),
                mix(corner(cell,f,ivec3(0,1,1)),corner(cell,f,ivec3(1,1,1)),t.x),t.y);
  return clamp(0.5+0.8*mix(a,b,t.z),0.0,1.0);
}
// Matrices and normalisation are prepared in JS, matching Texture.hs.
float fractal(vec3 p, int start, bool warp) {
  vec4 config = data(start); // count, persistence, total amplitude, style
  float amplitude=1.0, value=0.0;
  for (int i=0; i<int(config.x); ++i) {
    int j=start+1+3*i;
    vec3 q=mat3(data(j).xyz,data(j+1).xyz,data(j+2).xyz)*p;
    if (!warp) q+=float(i)*vec3(31.7,17.3,11.9);
    float n=noise3(q);
    if (warp || config.w==1.0) n=abs(2.0*n-1.0);
    else if (config.w==2.0) { n=1.0-abs(2.0*n-1.0); n*=n; }
    value+=amplitude*n;
    amplitude*=config.y;
  }
  if (config.z<=0.0) return warp ? 0.0 : 0.5;
  value/=config.z;
  if (warp) return value;
  if (config.w==0.0) value=0.5+(value-0.5)*2.0;
  else if (config.w==1.0) value*=1.75;
  else value=(value-0.2)/0.72;
  return clamp(value,0.0,1.0);
}
float srgb(float c) { return c<=0.0031308 ? 12.92*c : 1.055*pow(c,1.0/2.4)-0.055; }
vec4 fromLab(vec4 lab) {
  vec3 lms=vec3(lab.x+0.3963377774*lab.y+0.2158037573*lab.z,
                lab.x-0.1055613458*lab.y-0.0638541728*lab.z,
                lab.x-0.0894841775*lab.y-1.2914855480*lab.z);
  lms=lms*lms*lms;
  vec3 rgb=vec3(dot(vec3(4.0767416621,-3.3077115913,0.2309699292),lms),
                dot(vec3(-1.2684380046,2.6097574011,-0.3413193965),lms),
                dot(vec3(-0.0041960863,-0.7034186147,1.707614701),lms));
  return clamp(vec4(srgb(rgb.r),srgb(rgb.g),srgb(rgb.b),lab.w),0.0,1.0);
}
vec4 mixLab(vec4 a,vec4 b,float t) {
  float alpha=mix(a.w,b.w,t);
  // Normalise the alpha weight before mixing: avoids cancellation of tiny
  // premultiplied components near a transparent stop on software backends.
  vec3 colour=alpha<=0.0 ? mix(a.xyz,b.xyz,t) : mix(a.xyz,b.xyz,t*b.w/alpha);
  return fromLab(vec4(colour,max(0.0,alpha)));
}
// A bounded polynomial avoids the coarse cos approximation on SwiftShader.
// sin(pi*(t-.5)) on [-pi/2,pi/2], through degree 13; analytic error < 7e-10.
float easeSinusoidal(float t) {
  float x=3.141592653589793*(t-0.5),q=x*x;
  float s=x*(1.0+q*(-0.16666666666666667+q*(0.008333333333333333+q*(-0.0001984126984126984+q*(0.0000027557319223985893+q*(-0.00000002505210838544172+q*0.00000000016059043836821615))))));
  return clamp(0.5+0.5*s,0.0,1.0);
}
float rampMode(float t,float lo,float hi,int mode) {
  if (mode==0) return clamp(t,lo,hi);
  float span=hi-lo;
  if (span<=0.0) return lo;
  float offset=t-lo;
  float period=mode==1 ? span : 2.0*span;
  offset-=floor(offset/period)*period;
  return lo+(mode==2 && offset>span ? 2.0*span-offset : offset);
}
vec4 ramp(float t,int start,int count,int mode) {
  float lo=data(start).x, hi=data(start+3*(count-1)).x;
  t=rampMode(t,lo,hi,mode);
  int lower=start;
  if (lo>t) return data(start+1);
  for (int i=1;i<count;++i) {
    int next=start+3*i;
    if (data(next).x<=t) { lower=next; continue; }
    if (data(lower).x==t) return data(lower+1);
    return mixLab(data(lower+2),data(next+2),(t-data(lower).x)/(data(next).x-data(lower).x));
  }
  return data(lower+1);
}
vec4 over(vec4 top,vec4 bottom) {
  float alpha=top.a+bottom.a*(1.0-top.a);
  float weight=alpha<=0.0 ? 0.0 : top.a/alpha;
  return vec4(mix(bottom.rgb,top.rgb,weight),alpha);
}
// Match Render.toByte: explicit ties-to-even before normalized framebuffer conversion.
vec4 quantize(vec4 c) { return roundEven(clamp(c,0.0,1.0)*255.0)/255.0; }
vec3 safeNormalise(vec3 p) { float n=length(p); return n<1e-12 ? vec3(0,0,1) : p/n; }
float solid(vec3 p);
vec3 normalAt(vec3 p) {
  vec3 h=vec3(0.0001,0.0,0.0);
  return safeNormalise(vec3(solid(p+h.xyy)-solid(p-h.xyy),solid(p+h.yxy)-solid(p-h.yxy),solid(p+h.yyx)-solid(p-h.yyx)));
}
`

export function mainShader(material: string, mode?: 'slice' | 'scene'): string {
  return `
void main() {
  vec2 uv=vec2(gl_FragCoord.x,resolution.y-gl_FragCoord.y)/resolution;
  if (${mode === 'slice' ? 'true' : mode === 'scene' ? 'false' : 'sliceAxis>=0'}) {
    vec3 p=sliceAxis==0 ? vec3(uv,slicePosition) : sliceAxis==1 ? vec3(uv.x,slicePosition,uv.y) : vec3(slicePosition,uv);
    outputColour=quantize(${material}(p)); return;
  }
  vec3 origin=cameraOrigin;
  vec3 direction=normalize(cameraForward+((2.0*uv.x-1.0)*0.36397023426620234*cameraRight+(2.0*uv.y-1.0)*0.36397023426620234*cameraDown));
  vec3 background=vec3(0.055,0.075,0.11), q=origin-vec3(0.5);
  outputColour=quantize(vec4(background,1));
  float b=dot(q,direction), discriminant=b*b-dot(q,q)+0.75*0.75;
  if (discriminant<0.0) return;
  float root=sqrt(discriminant), t=max(0.0,-b-root), end=-b+root;
  for (int i=0;i<128;++i) {
    if (t>end) break;
    vec3 p=origin+t*direction;
    float d=solid(p);
    if (abs(d)<0.0005) {
      float light=0.3+0.7*max(0.0,dot(normalAt(p),normalize(vec3(-0.6,-0.8,-1))));
      vec4 colour=${material}(p);
      outputColour=quantize(vec4(colour.rgb*light*colour.a+background*(1.0-colour.a),1)); return;
    }
    t+=max(0.0001,abs(d)*0.9);
  }
}`
}
