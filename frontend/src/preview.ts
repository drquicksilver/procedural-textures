import type { TextureDocument } from './types'

export type RenderFn = (document: TextureDocument, size: number, signal: AbortSignal) => Promise<Blob>

export interface PreviewResult {
  blob: Blob
  size: number
  document: TextureDocument
}

export interface PreviewOptions {
  lowSize: number
  fullSize: number
  /** How long edits must pause before a full-resolution render starts. */
  interactive?: boolean
  settleMs: number
}

/**
 * Keeps a preview in step with a document that may change many times a
 * second (while a slider is dragged, say).
 *
 * - Every change asks for a low-resolution render. At most one is in flight;
 *   changes made meanwhile collapse into a single follow-up render of the
 *   latest document, so the preview keeps moving without queueing stale work.
 * - Once changes pause for `settleMs`, a full-resolution render starts. Any
 *   change aborts it.
 * - Results for documents that have since changed are still shown if nothing
 *   newer has arrived, but a full-resolution image is never replaced by an
 *   older or lower-resolution one for the same document.
 */
export class PreviewScheduler {
  private readonly render: RenderFn
  private readonly onResult: (result: PreviewResult) => void
  private readonly onError: (error: unknown, document: TextureDocument) => void
  private readonly onBusy: (busy: boolean) => void
  private options: PreviewOptions

  private latest: TextureDocument | null = null
  private generation = 0
  private lowInFlight = false
  private lowPending = false
  private lowAbort: AbortController | null = null
  private fullTimer: ReturnType<typeof setTimeout> | null = null
  private fullAbort: AbortController | null = null
  private shownGeneration = -1
  private shownFull = false

  constructor(
    render: RenderFn,
    onResult: (result: PreviewResult) => void,
    onError: (error: unknown, document: TextureDocument) => void,
    onBusy: (busy: boolean) => void,
    options: PreviewOptions,
  ) {
    this.render = render
    this.onResult = onResult
    this.onError = onError
    this.onBusy = onBusy
    this.options = options
  }

  setOptions(options: PreviewOptions): void {
    const changed = options.fullSize !== this.options.fullSize || options.lowSize !== this.options.lowSize || options.interactive !== this.options.interactive
    this.options = options
    if (changed && this.latest) this.update(this.latest)
  }

  update(document: TextureDocument): void {
    this.latest = document
    this.generation += 1
    this.cancelFull()
    if (this.lowInFlight) {
      this.lowPending = true
    } else {
      void this.renderLow()
    }
    if (!this.options.interactive) this.fullTimer = setTimeout(() => void this.renderFull(), this.options.settleMs)
    this.reportBusy()
  }

  dispose(): void {
    this.cancelFull()
    this.latest = null
    this.lowPending = false
    this.lowAbort?.abort()
  }

  private cancelFull(): void {
    if (this.fullTimer !== null) clearTimeout(this.fullTimer)
    this.fullTimer = null
    this.fullAbort?.abort()
    this.fullAbort = null
  }

  private async renderLow(): Promise<void> {
    const document = this.latest
    if (!document) return
    const generation = this.generation
    this.lowInFlight = true
    this.lowPending = false
    const abort = new AbortController()
    this.lowAbort = abort
    try {
      const blob = await this.render(document, this.options.lowSize, abort.signal)
      const supersededByFull = this.shownFull && this.shownGeneration >= generation
      if (!abort.signal.aborted && this.latest && !supersededByFull && generation >= this.shownGeneration) {
        this.show({ blob, size: this.options.lowSize, document }, generation, false)
      }
    } catch (error) {
      if (!abort.signal.aborted && generation === this.generation) this.onError(error, document)
    } finally {
      this.lowAbort = null
      this.lowInFlight = false
      if (this.lowPending) {
        void this.renderLow()
      }
      this.reportBusy()
    }
  }

  private async renderFull(): Promise<void> {
    this.fullTimer = null
    const document = this.latest
    if (!document) return
    const generation = this.generation
    const abort = new AbortController()
    this.fullAbort = abort
    this.reportBusy()
    try {
      const blob = await this.render(document, this.options.fullSize, abort.signal)
      if (!abort.signal.aborted && generation === this.generation) {
        this.show({ blob, size: this.options.fullSize, document }, generation, true)
      }
    } catch (error) {
      if (!abort.signal.aborted && generation === this.generation) this.onError(error, document)
    } finally {
      if (this.fullAbort === abort) this.fullAbort = null
      this.reportBusy()
    }
  }

  private show(result: PreviewResult, generation: number, full: boolean): void {
    this.shownGeneration = generation
    this.shownFull = full
    this.onResult(result)
  }

  private reportBusy(): void {
    this.onBusy(this.lowInFlight || this.fullTimer !== null || this.fullAbort !== null)
  }
}
