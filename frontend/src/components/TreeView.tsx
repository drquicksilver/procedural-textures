import { flatten, categoryOf, inspectionTexture, pathKey, samePath, variantOf, type Path } from '../tree'
import type { Node, Schema } from '../types'
import { Thumbnail } from './Thumbnail'

interface Props {
  schema: Schema
  root: Node
  /** The document's named ramps, which subtrees may refer to. */
  ramps: Record<string, Node> | undefined
  selection: Path
  collapsed: Set<string>
  onSelect: (path: Path) => void
  onToggle: (path: Path) => void
}

/** The texture as an outline, each node with a live thumbnail of its subtree. */
export function TreeView({ schema, root, ramps, selection, collapsed, onSelect, onToggle }: Props) {
  const entries = flatten(schema, root, collapsed)
  const index = entries.findIndex((e) => samePath(e.path, selection))

  return (
    <ul
      class="tree"
      role="tree"
      aria-label="Texture structure"
      tabIndex={0}
      onKeyDown={(e) => {
        const move = (delta: number) => {
          const next = entries[Math.min(entries.length - 1, Math.max(0, index + delta))]
          if (next) onSelect(next.path)
          e.preventDefault()
        }
        if (e.key === 'ArrowDown') move(1)
        else if (e.key === 'ArrowUp') move(-1)
        else if ((e.key === 'ArrowLeft' || e.key === 'ArrowRight') && entries[index]?.hasChildren) {
          const isCollapsed = collapsed.has(pathKey(selection))
          if ((e.key === 'ArrowLeft') !== isCollapsed) onToggle(selection)
          e.preventDefault()
        }
      }}
    >
      {entries.map((entry) => {
        const selected = samePath(entry.path, selection)
        const isCollapsed = collapsed.has(pathKey(entry.path))
        return (
          <li
            key={pathKey(entry.path)}
            role="treeitem"
            aria-selected={selected}
            aria-expanded={entry.hasChildren ? !isCollapsed : undefined}
            class={`tree-row ${selected ? 'is-selected' : ''}`}
            style={{ paddingLeft: `${6 + entry.depth * 12}px` }}
            onClick={() => onSelect(entry.path)}
          >
            <button
              class={`disclosure ${entry.hasChildren ? '' : 'is-hidden'}`}
              tabIndex={-1}
              aria-label={isCollapsed ? 'Expand' : 'Collapse'}
              onClick={(e) => {
                e.stopPropagation()
                onToggle(entry.path)
              }}
            >
              {isCollapsed ? '▸' : '▾'}
            </button>
            <Thumbnail texture={inspectionTexture(schema, entry.node)} ramps={ramps} />
            <span class="tree-label">
              {entry.fieldLabel && <span class="tree-field">{entry.fieldLabel}</span>}
              <span>{variantOf(schema, categoryOf(schema, entry.node), entry.node.type)?.label ?? entry.node.type}</span>
            </span>
          </li>
        )
      })}
    </ul>
  )
}
