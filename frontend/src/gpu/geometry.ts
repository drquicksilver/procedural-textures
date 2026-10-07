import metadata from '../metadata'

export interface DistanceNode { type: string; [key: string]: unknown }
const fixtures = metadata as { shapes: { id: string; solid: DistanceNode }[] }
export const shapeDefinitions = Object.fromEntries(fixtures.shapes.map((shape) => [shape.id, shape.solid]))
export const shapeNames = fixtures.shapes.map((shape) => shape.id)

/** Compile the Haskell-exported SDF/profile data, not a second set of models. */
export function compileGeometry(solid: DistanceNode): string {
  const pending = [{ node: solid, depth: 0 }]
  let visits = 0
  while (pending.length) {
    const { node, depth } = pending.pop()!
    if (depth > 64) throw new Error('Geometry nesting exceeds 64')
    if (++visits > 4096) throw new Error('Geometry tree exceeds 4096 references')
    for (const key of ['a', 'b', 'base', 'bound', 'profile']) {
      const value = node[key]
      if (value && typeof value === 'object' && 'type' in value) pending.push({ node: value as DistanceNode, depth: depth + 1 })
    }
  }
  const functions: string[] = [], cache = new Map<string, string>()
  let count = 0
  const scalar = (value: unknown): string => {
    if (typeof value !== 'number' || !Number.isFinite(Math.fround(value))) throw new Error('Invalid geometry scalar')
    const literal = String(value)
    return `(${/[.eE]/.test(literal) ? literal : `${literal}.0`})`
  }
  const vector = (value: unknown, size: number): string => {
    if (!Array.isArray(value) || value.length !== size) throw new Error('Invalid geometry vector')
    return `vec${size}(${value.map(scalar).join(',')})`
  }
  const child = (value: unknown): DistanceNode => {
    if (!value || typeof value !== 'object' || !('type' in value)) throw new Error('Invalid geometry node')
    return value as DistanceNode
  }
  const build = (node: DistanceNode, profile = false, depth = 0): string => {
    if (depth > 64) throw new Error('Geometry nesting exceeds 64')
    const key = `${profile}:${JSON.stringify(node)}`, cached = cache.get(key)
    if (cached) return cached
    if (++count > 512) throw new Error('Geometry exceeds 512 nodes')
    const name = `geometry${count}`, dimension = profile ? 2 : 3
    const v = (field: string) => vector(node[field], dimension), f = (field: string) => scalar(node[field])
    const sub = (field: string, isProfile = profile) => build(child(node[field]), isProfile, depth + 1)
    let body: string
    switch (node.type) {
      case 'sphere': body = `return sdfSphere(p,${v('centre')},${f('radius')});`; break
      case 'box': body = `return sdfBox(p,${v('centre')},${v('half')});`; break
      case 'disc': body = `return length(p-${v('centre')})-${f('radius')};`; break
      case 'rect': {
        const radius = profile ? f('radius') : '0.0'
        body = `vec${dimension} q=abs(p-${v('centre')})-${v('half')}+${radius}; return length(max(q,0.0))+min(0.0,${profile ? 'max(q.x,q.y)' : 'max(q.x,max(q.y,q.z))'})-${radius};`
        break
      }
      case 'cylinder': body = `return sdfCylinder(p,${v('centre')},${f('radius')},${f('height')});`; break
      case 'torus': body = `return sdfTorus(p,${v('centre')},${f('major')},${f('minor')});`; break
      case 'plane': body = `return sdfPlane(p,${v('normal')},${f('offset')});`; break
      case 'union': case 'intersection': case 'difference': case 'blend': {
        const a = `${sub('a')}(p)`, b = `${sub('b')}(p)`
        body = `return ${node.type === 'union' ? `min(${a},${b})` : node.type === 'intersection' ? `max(${a},${b})` : node.type === 'difference' ? `max(${a},-${b})` : `smoothMinimum(${a},${b},${f('amount')})`};`
        break
      }
      case 'rounded': body = `return ${sub('base')}(p)-${f('amount')};`; break
      case 'revolve': body = `vec3 q=p-${v('centre')}; return ${sub('profile', true)}(vec2(length(q.xz),-q.y));`; break
      case 'extrude': body = `vec3 q=p-${v('centre')}; vec2 d=vec2(${sub('profile', true)}(vec2(q.x,-q.y)),abs(q.z)-${f('height')}); return min(0.0,max(d.x,d.y))+length(max(d,0.0));`; break
      case 'turn': f('angle'); body = `vec3 q=p-${v('centre')}; float c=${scalar(Math.cos(Number(node.angle)))},s=${scalar(Math.sin(Number(node.angle)))}; return ${sub('base')}(${v('centre')}+vec3(c*q.x+s*q.z,q.y,c*q.z-s*q.x));`; break
      case 'radial-repeat': {
        if (typeof node.count !== 'number' || !Number.isInteger(node.count) || node.count < 1) throw new Error('Invalid radial repeat count')
        body = `vec3 q=p-${v('centre')}; float sector=6.283185307179586/${f('count')},angle=length(q.xz)==0.0 ? 0.0 : atan(q.z,q.x); angle-=sector*roundEven(angle/sector); float r=length(q.xz); return ${sub('base')}(${v('centre')}+vec3(r*cos(angle),q.y,r*sin(angle)));`
        break
      }
      case 'scaled': {
        if (typeof node.amount !== 'number' || node.amount <= 0) throw new Error('Invalid geometry scale')
        body = `return ${f('amount')}*${sub('base')}(${v('centre')}+(p-${v('centre')})/${f('amount')});`
        break
      }
      case 'bounded': body = `float d=${sub('bound')}(p); if (d>0.02) return d; return ${sub('base')}(p);`; break
      case 'polygon': {
        if (!profile || !Array.isArray(node.vertices) || node.vertices.length < 3 || node.vertices.length > 128) throw new Error('Unsupported polygon size')
        const n = node.vertices.length
        body = `const vec2 vertices[${n}]=vec2[${n}](${node.vertices.map((v) => vector(v, 2)).join(',')});
          float best=1e30; bool inside=false;
          for(int i=0;i<${n};++i) {
            vec2 a=vertices[i],b=vertices[i==0 ? ${n - 1} : i-1],e=b-a,w=p-a;
            float t=clamp(dot(w,e)/dot(e,e),0.0,1.0); vec2 d=w-e*t;
            best=min(best,dot(d,d)); bvec3 conditions=bvec3(p.y>=a.y,p.y<b.y,e.x*w.y>e.y*w.x);
            if(all(conditions)||!any(conditions)) inside=!inside;
          }
          return (inside ? -1.0 : 1.0)*sqrt(best);`
        break
      }
      default: throw new Error(`Unsupported geometry node: ${node.type}`)
    }
    functions.push(`// ${profile ? 'profile' : 'SDF'} ${node.type}\nfloat ${name}(vec${dimension} p) { ${body} }`)
    cache.set(key, name)
    return name
  }
  const root = build(solid)
  return `${sdfKernels}\nfloat smoothMinimum(float a,float b,float k) { if(k<=0.0) return min(a,b); float h=max(0.0,k-abs(a-b))/k; return min(a,b)-h*h*k/4.0; }\n${functions.join('\n')}\nfloat solid(vec3 p) { return ${root}(p); }\n`
}

/** Primitive kernels shared by material fields and raymarch geometry. */
const sdfKernels = `
float sdfSphere(vec3 p,vec3 c,float r) { return length(p-c)-r; }
float sdfBox(vec3 p,vec3 c,vec3 h) { vec3 q=abs(p-c)-h; return length(max(q,0.0))+min(0.0,max(q.x,max(q.y,q.z))); }
float sdfCylinder(vec3 p,vec3 c,float r,float h) { vec3 q=p-c; vec2 d=vec2(length(q.xz)-r,abs(q.y)-h); return length(max(d,0.0))+min(0.0,max(d.x,d.y)); }
float sdfTorus(vec3 p,vec3 c,float r,float t) { vec3 q=p-c; return length(vec2(length(q.xz)-r,q.y))-t; }
float sdfPlane(vec3 p,vec3 n,float o) { return dot(safeNormalise(n),p)-o; }
`
