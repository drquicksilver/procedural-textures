/** Pixel work is approximately quadratic in the side length. Keep headroom and
 * damp increases; react immediately when a more expensive view exceeds budget.
 */
export class AdaptiveResolution {
  private costPerPixel: number | null = null
  private sequence = -1
  readonly budgetMs: number
  constructor(budgetMs = 15) { this.budgetMs = budgetMs }
  sample(size: number, elapsedMs: number, sequence: number): void {
    if (!Number.isFinite(elapsedMs) || elapsedMs <= 0 || size <= 0 || sequence <= this.sequence) return
    this.sequence = sequence
    const cost = elapsedMs / (size * size)
    this.costPerPixel = this.costPerPixel === null || elapsedMs > this.budgetMs
      ? cost : this.costPerPixel * 0.8 + cost * 0.2
  }
  size(fullSize: number): number {
    if (this.costPerPixel === null) return fullSize
    const desired = Math.sqrt(this.budgetMs * 0.85 / this.costPerPixel)
    return Math.min(fullSize, Math.max(Math.min(64, fullSize), Math.floor(desired / 32) * 32))
  }
  reset(): void { this.costPerPixel = null; this.sequence = -1 }
}
