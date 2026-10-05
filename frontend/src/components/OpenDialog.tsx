import type { Example, TextureDocument } from '../types'
import { Dialog } from './Dialog'
import { Thumbnail } from './Thumbnail'

interface Props {
  examples: Example[]
  onOpen: (document: TextureDocument) => void
  onClose: () => void
}

export function OpenDialog({ examples, onOpen, onClose }: Props) {
  return (
    <Dialog title="Open" onClose={onClose}>
      <h2>Examples</h2>
      <ul class="document-grid">
        {examples.map((e) => (
          <li key={e.id}>
            <button class="document-card" title={e.document.description} onClick={() => onOpen(e.document)}>
              <Thumbnail texture={e.document.texture} />
              <span>{e.document.name}</span>
            </button>
          </li>
        ))}
      </ul>
    </Dialog>
  )
}
