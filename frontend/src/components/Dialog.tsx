import type { ComponentChildren } from 'preact'
import { useEffect, useRef } from 'preact/hooks'

interface Props {
  title: string
  onClose: () => void
  children: ComponentChildren
}

/** A modal dialog, closed with Escape, the close button or a click outside. */
export function Dialog({ title, onClose, children }: Props) {
  const panel = useRef<HTMLDivElement>(null)

  useEffect(() => {
    const previous = document.activeElement as HTMLElement | null
    panel.current?.focus()
    const onKey = (e: KeyboardEvent) => {
      if (e.key === 'Escape') onClose()
    }
    window.addEventListener('keydown', onKey)
    return () => {
      window.removeEventListener('keydown', onKey)
      previous?.focus()
    }
  }, [onClose])

  return (
    <div class="dialog-backdrop" onMouseDown={(e) => e.target === e.currentTarget && onClose()}>
      <div class="dialog" role="dialog" aria-modal="true" aria-label={title} tabIndex={-1} ref={panel}>
        <header class="dialog-header">
          <h1>{title}</h1>
          <button class="icon-button" aria-label="Close" onClick={onClose}>
            ×
          </button>
        </header>
        <div class="dialog-body">{children}</div>
      </div>
    </div>
  )
}
