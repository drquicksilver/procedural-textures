import type { TextureDocument } from '../types'
import type { ViewOptions } from '../view'
import { compileMaterial, type CompiledMaterial, type Diagnostic } from './compiler'
import { vertexShader } from './shaders'

/** Independent WebGL2 spike renderer. Each instance owns its context resources. */
export class GpuRenderer {
  private readonly gl: WebGL2RenderingContext
  private texture!: WebGLTexture
  private target!: WebGLTexture
  private framebuffer!: WebGLFramebuffer
  private points!: WebGLTexture
  private vao!: WebGLVertexArrayObject
  private readonly programs = new Map<string, WebGLProgram>()
  private retainedSource: string | null = null
  private lastSource: string | null = null
  private disposed = false
  private floatingTarget = false
  private parameterHeight = 0
  private targetWidth = 0
  private targetHeight = 0
  programCompilations = 0
  lastProgramCompileMs = 0

  constructor(gl: WebGL2RenderingContext) {
    this.gl = gl
    this.allocate()
    gl.canvas.addEventListener('webglcontextlost', this.onLost)
    gl.canvas.addEventListener('webglcontextrestored', this.onRestored)
  }

  private onLost = (event: Event): void => { event.preventDefault(); this.programs.clear() }
  private onRestored = (): void => { if (!this.disposed) this.allocate() }

  private allocate(): void {
    const gl = this.gl
    this.parameterHeight = this.targetWidth = this.targetHeight = 0
    const texture = gl.createTexture(), target = gl.createTexture(), framebuffer = gl.createFramebuffer(), points = gl.createTexture(), vao = gl.createVertexArray()
    if (!texture || !target || !framebuffer || !points || !vao) throw new Error('Could not allocate WebGL2 resources')
    this.texture = texture; this.target = target; this.framebuffer = framebuffer; this.points = points; this.vao = vao
  }

  private program(source: string): WebGLProgram {
    this.lastSource = source
    const gl = this.gl, cached = this.programs.get(source)
    if (cached) { this.programs.delete(source); this.programs.set(source, cached); return cached }
    const started = performance.now()
    const compile = (type: number, text: string): WebGLShader => {
      const shader = gl.createShader(type)
      if (!shader) throw new Error('Could not allocate shader')
      gl.shaderSource(shader, text); gl.compileShader(shader)
      if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        const error = gl.getShaderInfoLog(shader)
        gl.deleteShader(shader)
        throw new Error(`${error}\n${text.split('\n').map((line, i) => `${i + 1}: ${line}`).join('\n')}`)
      }
      return shader
    }
    const vertex = compile(gl.VERTEX_SHADER, vertexShader)
    let fragment: WebGLShader | undefined
    const program = gl.createProgram()
    try {
      fragment = compile(gl.FRAGMENT_SHADER, source)
      if (!program) throw new Error('Could not allocate shader program')
      gl.attachShader(program, vertex); gl.attachShader(program, fragment); gl.linkProgram(program)
      if (!gl.getProgramParameter(program, gl.LINK_STATUS)) throw new Error(`Shader link failed: ${gl.getProgramInfoLog(program)}\n${source}`)
    } catch (error) { gl.deleteProgram(program); throw error }
    finally { gl.deleteShader(vertex); if (fragment) gl.deleteShader(fragment) }
    this.lastProgramCompileMs = performance.now() - started
    this.programCompilations++
    this.programs.set(source, program!)
    if (this.programs.size > 8) {
      const oldest = [...this.programs.keys()].find((key) => key !== this.retainedSource)!
      gl.deleteProgram(this.programs.get(oldest)!); this.programs.delete(oldest)
    }
    return program!
  }

  /** Keep the main viewer program resident while background thumbnails use the LRU. */
  retainPresentedProgram(): void { this.retainedSource = this.lastSource }

  /** Draw and present. GPU completion/readback is deliberately separate. */
  render(document: TextureDocument, view: ViewOptions, size: number): void {
    const gl = this.gl
    if (this.disposed || gl.isContextLost()) throw new Error('WebGL2 renderer unavailable')
    if (!Number.isInteger(size) || size < 1 || size > 2048 || size > gl.getParameter(gl.MAX_TEXTURE_SIZE)) throw new Error('Unsupported render size')
    if (![view.yaw, view.pitch, view.distance, view.position].every(Number.isFinite)) throw new Error('Non-finite view parameter')
    this.draw(compileMaterial(document, { shape: view.shape, renderMode: view.mode }), view, size, size, false)
  }

  /** Float diagnostic pass at fixed FP32 coordinates; no lighting or quantisation. */
  samples(document: TextureDocument, points: number[][], diagnostic: Diagnostic = 'material'): Float32Array {
    return this.sampleCompiled(compileMaterial(document, { diagnostic }), points)
  }

  /** Lower-level entry used by shader diagnostic/mutation checks. */
  sampleCompiled(compiled: CompiledMaterial, points: number[][]): Float32Array {
    const gl = this.gl
    if (this.disposed || gl.isContextLost()) throw new Error('WebGL2 renderer unavailable')
    if (!gl.getExtension('EXT_color_buffer_float')) throw new Error('Float diagnostics require EXT_color_buffer_float')
    if (!points.length || points.length > 2048 || points.some((p) => p.length !== 3 || p.some((v) => !Number.isFinite(Math.fround(v))))) throw new Error('Invalid diagnostic points')
    gl.activeTexture(gl.TEXTURE2); gl.bindTexture(gl.TEXTURE_2D, this.points)
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST); gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST)
    const values = new Float32Array(points.flatMap((p) => [...p, 1]))
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, points.length, 1, 0, gl.RGBA, gl.FLOAT, values)
    this.draw(compiled, { mode: 'slice', shape: 'bitten-cube', axis: 'xy', position: 0, yaw: 0, pitch: 0, distance: 2 }, points.length, 1, true)
    const result = new Float32Array(points.length * 4)
    gl.bindFramebuffer(gl.READ_FRAMEBUFFER, this.framebuffer)
    gl.readPixels(0, 0, points.length, 1, gl.RGBA, gl.FLOAT, result)
    if (gl.getError() !== gl.NO_ERROR) throw new Error('Float diagnostic readback failed')
    return result
  }

  private draw(compiled: CompiledMaterial, view: ViewOptions, width: number, height: number, floating: boolean): void {
    const gl = this.gl
    if (this.disposed || gl.isContextLost()) throw new Error('WebGL2 renderer unavailable')
    const limit = Math.min(gl.getParameter(gl.MAX_TEXTURE_SIZE), gl.getParameter(gl.MAX_RENDERBUFFER_SIZE))
    if (Math.max(width, height, 256, compiled.parameters.length / 1024) > limit) throw new Error('Render exceeds device limits')
    this.lastProgramCompileMs = 0
    const program = this.program(compiled.source)
    if (gl.canvas.width !== width) gl.canvas.width = width
    if (gl.canvas.height !== height) gl.canvas.height = height
    gl.bindVertexArray(this.vao); gl.useProgram(program)
    gl.disable(gl.BLEND); gl.disable(gl.DITHER); gl.disable(gl.DEPTH_TEST); gl.disable(gl.SCISSOR_TEST); gl.disable(gl.CULL_FACE)
    gl.activeTexture(gl.TEXTURE0); gl.bindTexture(gl.TEXTURE_2D, this.texture)
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST); gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST)
    const parameterHeight = compiled.parameters.length / (256 * 4)
    if (this.parameterHeight !== parameterHeight) {
      gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, 256, parameterHeight, 0, gl.RGBA, gl.FLOAT, compiled.parameters)
      this.parameterHeight = parameterHeight
    } else gl.texSubImage2D(gl.TEXTURE_2D, 0, 0, 0, 256, parameterHeight, gl.RGBA, gl.FLOAT, compiled.parameters)
    gl.uniform1i(gl.getUniformLocation(program, 'parameters'), 0)
    gl.uniform2f(gl.getUniformLocation(program, 'resolution'), width, height)
    gl.uniform1i(gl.getUniformLocation(program, 'samplePoints'), 2)
    // Camera invariants use JS Double trig, matching the reference. Some
    // software GLSL trig implementations have visibly coarser approximations.
    const pitch = Math.max(-1.45, Math.min(1.45, view.pitch)), radius = Math.max(1.1, Math.min(6, view.distance))
    const q = [radius * Math.sin(view.yaw) * Math.cos(pitch), -radius * Math.sin(pitch), -radius * Math.cos(view.yaw) * Math.cos(pitch)]
    const normalise = (v: number[]) => { const length = Math.hypot(...v); return v.map((n) => n / length) }
    const forward = normalise(q.map((n) => -n)), right = normalise([forward[2], 0, -forward[0]])
    const down = [forward[1] * right[2], forward[2] * right[0] - forward[0] * right[2], -forward[1] * right[0]]
    for (const [name, value] of [['cameraOrigin', q.map((n) => n + 0.5)], ['cameraForward', forward], ['cameraRight', right], ['cameraDown', down]] as const) gl.uniform3fv(gl.getUniformLocation(program, name), value)
    gl.uniform1i(gl.getUniformLocation(program, 'sliceAxis'), view.mode === 'scene' ? -1 : ['xy', 'xz', 'yz'].indexOf(view.axis))
    gl.uniform1f(gl.getUniformLocation(program, 'slicePosition'), view.position)
    // A second unit keeps the material data bound while allocating the target.
    gl.activeTexture(gl.TEXTURE1); gl.bindTexture(gl.TEXTURE_2D, this.target)
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST); gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST)
    if (this.targetWidth !== width || this.targetHeight !== height || this.floatingTarget !== floating) {
      gl.texImage2D(gl.TEXTURE_2D, 0, floating ? gl.RGBA32F : gl.RGBA8, width, height, 0, gl.RGBA, floating ? gl.FLOAT : gl.UNSIGNED_BYTE, null)
      this.targetWidth = width; this.targetHeight = height
    }
    gl.bindFramebuffer(gl.FRAMEBUFFER, this.framebuffer)
    gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, this.target, 0)
    if (gl.checkFramebufferStatus(gl.FRAMEBUFFER) !== gl.FRAMEBUFFER_COMPLETE) throw new Error('Incomplete render framebuffer')
    this.floatingTarget = floating
    gl.viewport(0, 0, width, height); gl.drawArrays(gl.TRIANGLES, 0, 3)
    if (!floating) {
      gl.bindFramebuffer(gl.READ_FRAMEBUFFER, this.framebuffer); gl.bindFramebuffer(gl.DRAW_FRAMEBUFFER, null)
      gl.blitFramebuffer(0, 0, width, height, 0, 0, width, height, gl.COLOR_BUFFER_BIT, gl.NEAREST)
    }
    const error = gl.getError()
    if (error !== gl.NO_ERROR) throw new Error(`WebGL2 render error: 0x${error.toString(16)}`)
  }

  /** Force completion with a synchronous one-pixel readback, not just submission. */
  complete(): void {
    this.gl.bindFramebuffer(this.gl.READ_FRAMEBUFFER, this.framebuffer)
    this.gl.readPixels(0, 0, 1, 1, this.gl.RGBA, this.floatingTarget ? this.gl.FLOAT : this.gl.UNSIGNED_BYTE, this.floatingTarget ? new Float32Array(4) : new Uint8Array(4))
    if (this.gl.getError() !== this.gl.NO_ERROR) throw new Error('GPU completion readback failed')
  }

  /** Top-to-bottom RGBA bytes, independent of the browser compositor. */
  readPixels(size: number): Uint8Array {
    if (this.disposed || this.gl.isContextLost()) throw new Error('WebGL2 renderer unavailable')
    if (this.floatingTarget || !Number.isInteger(size) || size !== this.gl.canvas.width || size !== this.gl.canvas.height) throw new Error('Readback dimensions or format differ from the last render')
    const gl = this.gl, raw = new Uint8Array(size * size * 4), pixels = new Uint8Array(raw.length)
    gl.bindFramebuffer(gl.READ_FRAMEBUFFER, this.framebuffer)
    gl.readPixels(0, 0, size, size, gl.RGBA, gl.UNSIGNED_BYTE, raw)
    if (gl.getError() !== gl.NO_ERROR) throw new Error('Pixel readback failed')
    for (let row = 0; row < size; row++) pixels.set(raw.subarray(row * size * 4, (row + 1) * size * 4), (size - 1 - row) * size * 4)
    return pixels
  }

  dispose(): void {
    this.gl.canvas.removeEventListener('webglcontextlost', this.onLost)
    this.gl.canvas.removeEventListener('webglcontextrestored', this.onRestored)
    for (const program of this.programs.values()) this.gl.deleteProgram(program)
    this.programs.clear()
    this.gl.deleteTexture(this.points); this.gl.deleteTexture(this.texture); this.gl.deleteTexture(this.target)
    this.gl.deleteFramebuffer(this.framebuffer); this.gl.deleteVertexArray(this.vao)
    this.disposed = true
  }
}
