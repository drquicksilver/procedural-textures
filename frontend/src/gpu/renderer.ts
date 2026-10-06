import type { TextureDocument } from '../types'
import type { ViewOptions } from '../view'
import { compileMaterial } from './compiler'
import { vertexShader } from './shaders'

/** Independent WebGL2 spike renderer. Each instance owns its context resources. */
export class GpuRenderer {
  private readonly gl: WebGL2RenderingContext
  private readonly texture: WebGLTexture
  private readonly target: WebGLTexture
  private readonly framebuffer: WebGLFramebuffer
  private readonly vao: WebGLVertexArrayObject
  private readonly programs = new Map<string, WebGLProgram>()
  private disposed = false
  programCompilations = 0
  lastProgramCompileMs = 0

  constructor(gl: WebGL2RenderingContext) {
    this.gl = gl
    const texture = gl.createTexture(), target = gl.createTexture(), framebuffer = gl.createFramebuffer(), vao = gl.createVertexArray()
    if (!texture || !target || !framebuffer || !vao) throw new Error('Could not allocate WebGL2 resources')
    this.texture = texture; this.target = target; this.framebuffer = framebuffer; this.vao = vao
  }

  private program(source: string): WebGLProgram {
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
      const oldest = this.programs.keys().next().value!
      gl.deleteProgram(this.programs.get(oldest)!); this.programs.delete(oldest)
    }
    return program!
  }

  /** Draw and present. GPU completion/readback is deliberately separate. */
  render(document: TextureDocument, view: ViewOptions, size: number): void {
    const gl = this.gl
    if (this.disposed || gl.isContextLost()) throw new Error('WebGL2 renderer unavailable')
    if (!Number.isInteger(size) || size < 1 || size > 2048 || size > gl.getParameter(gl.MAX_TEXTURE_SIZE)) throw new Error('Unsupported render size')
    if (![view.yaw, view.pitch, view.distance, view.position].every(Number.isFinite)) throw new Error('Non-finite view parameter')
    if (view.mode === 'scene' && view.shape !== 'bitten-cube') throw new Error('Spike scene supports bitten-cube only')
    this.lastProgramCompileMs = 0
    const compiled = compileMaterial(document), program = this.program(compiled.source)
    gl.canvas.width = size; gl.canvas.height = size
    gl.bindVertexArray(this.vao); gl.useProgram(program)
    gl.disable(gl.BLEND); gl.disable(gl.DITHER); gl.disable(gl.DEPTH_TEST); gl.disable(gl.SCISSOR_TEST)
    gl.activeTexture(gl.TEXTURE0); gl.bindTexture(gl.TEXTURE_2D, this.texture)
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST); gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST)
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, 256, compiled.parameters.length / (256 * 4), 0, gl.RGBA, gl.FLOAT, compiled.parameters)
    gl.uniform1i(gl.getUniformLocation(program, 'parameters'), 0)
    gl.uniform2f(gl.getUniformLocation(program, 'resolution'), size, size)
    gl.uniform3f(gl.getUniformLocation(program, 'camera'), view.yaw, view.pitch, view.distance)
    gl.uniform1i(gl.getUniformLocation(program, 'sliceAxis'), view.mode === 'scene' ? -1 : ['xy', 'xz', 'yz'].indexOf(view.axis))
    gl.uniform1f(gl.getUniformLocation(program, 'slicePosition'), view.position)
    // A second unit keeps the material data bound while allocating the target.
    gl.activeTexture(gl.TEXTURE1); gl.bindTexture(gl.TEXTURE_2D, this.target)
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST); gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST)
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, size, size, 0, gl.RGBA, gl.UNSIGNED_BYTE, null)
    gl.bindFramebuffer(gl.FRAMEBUFFER, this.framebuffer)
    gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, this.target, 0)
    if (gl.checkFramebufferStatus(gl.FRAMEBUFFER) !== gl.FRAMEBUFFER_COMPLETE) throw new Error('Incomplete render framebuffer')
    gl.viewport(0, 0, size, size); gl.drawArrays(gl.TRIANGLES, 0, 3)
    gl.bindFramebuffer(gl.READ_FRAMEBUFFER, this.framebuffer); gl.bindFramebuffer(gl.DRAW_FRAMEBUFFER, null)
    gl.blitFramebuffer(0, 0, size, size, 0, 0, size, size, gl.COLOR_BUFFER_BIT, gl.NEAREST)
    const error = gl.getError()
    if (error !== gl.NO_ERROR) throw new Error(`WebGL2 render error: 0x${error.toString(16)}`)
  }

  /** Force completion with a synchronous one-pixel readback, not just submission. */
  complete(): void {
    this.gl.bindFramebuffer(this.gl.READ_FRAMEBUFFER, this.framebuffer)
    this.gl.readPixels(0, 0, 1, 1, this.gl.RGBA, this.gl.UNSIGNED_BYTE, new Uint8Array(4))
  }

  /** Top-to-bottom RGBA bytes, independent of the browser compositor. */
  readPixels(size: number): Uint8Array {
    const gl = this.gl, raw = new Uint8Array(size * size * 4), pixels = new Uint8Array(raw.length)
    gl.bindFramebuffer(gl.READ_FRAMEBUFFER, this.framebuffer)
    gl.readPixels(0, 0, size, size, gl.RGBA, gl.UNSIGNED_BYTE, raw)
    for (let row = 0; row < size; row++) pixels.set(raw.subarray(row * size * 4, (row + 1) * size * 4), (size - 1 - row) * size * 4)
    return pixels
  }

  dispose(): void {
    for (const program of this.programs.values()) this.gl.deleteProgram(program)
    this.programs.clear()
    this.gl.deleteTexture(this.texture); this.gl.deleteTexture(this.target)
    this.gl.deleteFramebuffer(this.framebuffer); this.gl.deleteVertexArray(this.vao)
    this.disposed = true
  }
}
