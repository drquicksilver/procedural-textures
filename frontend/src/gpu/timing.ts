interface TimerExtension { TIME_ELAPSED_EXT: number; GPU_DISJOINT_EXT: number }
interface Sample { query: WebGLQuery; receive: (ms: number) => void }

/** Asynchronous GPU timing, bounded to four outstanding queries. Never wait for
 * completion, read pixels, or use samples invalidated by a disjoint event.
 */
export class GpuTimer {
  private readonly gl: WebGL2RenderingContext
  private readonly extension: TimerExtension | null
  private samples: Sample[] = []
  private frame: number | null = null
  constructor(gl: WebGL2RenderingContext) {
    this.gl = gl
    this.extension = gl.getExtension('EXT_disjoint_timer_query_webgl2')
  }
  get supported(): boolean { return this.extension !== null }
  begin(): WebGLQuery | null {
    if (!this.extension || this.samples.length >= 4 || this.gl.isContextLost()) return null
    // Reading this clears any pre-existing disjoint state before the measurement.
    if (this.gl.getParameter(this.extension.GPU_DISJOINT_EXT)) this.clear()
    const query = this.gl.createQuery()
    if (query) this.gl.beginQuery(this.extension.TIME_ELAPSED_EXT, query)
    return query
  }
  end(query: WebGLQuery, receive?: (ms: number) => void): void {
    this.gl.endQuery(this.extension!.TIME_ELAPSED_EXT)
    if (!receive) { this.gl.deleteQuery(query); return }
    this.samples.push({ query, receive })
    this.pollLater()
  }
  private pollLater(): void {
    if (this.frame !== null || !this.samples.length) return
    this.frame = requestAnimationFrame(() => {
      this.frame = null
      if (this.gl.isContextLost() || this.gl.getParameter(this.extension!.GPU_DISJOINT_EXT)) { this.clear(); return }
      const pending: Sample[] = []
      for (const sample of this.samples) {
        if (this.gl.getQueryParameter(sample.query, this.gl.QUERY_RESULT_AVAILABLE)) {
          const ms = Number(this.gl.getQueryParameter(sample.query, this.gl.QUERY_RESULT)) / 1e6
          this.gl.deleteQuery(sample.query)
          sample.receive(ms)
        } else pending.push(sample)
      }
      this.samples = pending
      this.pollLater()
    })
  }
  private clear(): void {
    for (const sample of this.samples) this.gl.deleteQuery(sample.query)
    this.samples = []
  }
  dispose(): void {
    if (this.frame !== null) cancelAnimationFrame(this.frame)
    this.frame = null; this.clear()
  }
}
