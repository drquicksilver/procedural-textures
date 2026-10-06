export interface PreviewOptions { previewSize: () => number; fullSize: number; interactive: boolean; settleMs: number }
export type Enqueue = (work: () => void) => () => void

/** Coalesce changes before submitting a frame, and cancel obsolete refinement.
 * The renderer is synchronous: submitted GPU work is never awaited or read back.
 */
export class CanvasPreview<State> {
  private latest: State | null = null
  private cancelFrame: (() => void) | null = null
  private timer: ReturnType<typeof setTimeout> | null = null
  private readonly render: (state: State, size: number) => void
  private readonly enqueue: Enqueue
  private readonly onError: (error: unknown) => void
  private readonly onBusy: (busy: boolean) => void
  private options: PreviewOptions
  constructor(
    render: (state: State, size: number) => void,
    enqueue: Enqueue,
    onError: (error: unknown) => void,
    onBusy: (busy: boolean) => void,
    options: PreviewOptions,
  ) { this.render = render; this.enqueue = enqueue; this.onError = onError; this.onBusy = onBusy; this.options = options }
  update(state: State): void {
    this.latest = state
    this.cancel()
    this.onBusy(true)
    this.schedule(false)
    if (!this.options.interactive) this.timer = setTimeout(() => {
      this.timer = null
      this.cancelFrame?.()
      this.schedule(true)
    }, this.options.settleMs)
  }
  setOptions(options: PreviewOptions): void {
    const changed = Object.keys(options).some((k) => options[k as keyof PreviewOptions] !== this.options[k as keyof PreviewOptions])
    this.options = options
    if (changed && this.latest !== null) this.update(this.latest)
  }
  private schedule(full: boolean): void {
    this.cancelFrame = this.enqueue(() => {
      this.cancelFrame = null
      if (this.latest === null) return
      try { this.render(this.latest, full ? this.options.fullSize : Math.min(this.options.fullSize, this.options.previewSize())) }
      catch (error) { this.onError(error) }
      this.onBusy(this.timer !== null)
    })
  }
  private cancel(): void {
    this.cancelFrame?.(); this.cancelFrame = null
    if (this.timer !== null) clearTimeout(this.timer)
    this.timer = null
  }
  dispose(): void { this.cancel(); this.latest = null; this.onBusy(false) }
}
