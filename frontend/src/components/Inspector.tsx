import { parseColour, formatColour } from '../colour'
import {
  changeType,
  clone,
  getAt,
  isNode,
  pathKey,
  swapChildren,
  textureFields,
  variantOf,
  wrap,
  wrapOptions,
  type Path,
} from '../tree'
import type { Example, Field, Json, Node, Schema } from '../types'
import { ColourInput, EnumSelect, FieldRow, PairField, ScalarField } from './fields'
import { RampEditor } from './RampEditor'

interface Props {
  schema: Schema
  root: Node
  path: Path
  examples: Example[]
  /** Replace the node at `path`. `key` identifies the edit for undo coalescing. */
  onReplace: (path: Path, node: Node, key: string | null) => void
  onSelect: (path: Path) => void
}

export function Inspector({ schema, root, path, examples, onReplace, onSelect }: Props) {
  const node = getAt(root, path)
  if (!node) return null
  const variant = variantOf(schema, 'texture', node.type)
  const key = pathKey(path)
  const replace = (next: Node, editKey: string | null = null) => onReplace(path, next, editKey)
  const setField = (field: string, value: Json) => replace({ ...node, [field]: value }, `${key}:${field}`)
  const children = textureFields(schema, node)

  return (
    <div class="inspector-body">
      <section class="inspector-section">
        <Breadcrumbs schema={schema} root={root} path={path} onSelect={onSelect} />
        <EnumSelect
          ariaLabel="Texture type"
          value={node.type}
          options={schema.texture.map((v) => ({ value: v.type, label: v.label }))}
          onChange={(type) => replace(changeType(schema, 'texture', node, type))}
        />
        {variant && <p class="description">{variant.description}</p>}
      </section>

      {variant && variant.fields.some((f) => f.kind !== 'texture') && (
        <section class="inspector-section">
          {variant.fields
            .filter((f) => f.kind !== 'texture')
            .map((field) => (
              <FieldEditor
                key={field.key}
                schema={schema}
                field={field}
                value={node[field.key]}
                onChange={(value) => setField(field.key, value)}
                onRampChange={(ramp, editKey) => replace({ ...node, [field.key]: ramp }, `${key}:${field.key}:${editKey}`)}
              />
            ))}
        </section>
      )}

      {children.length > 0 && (
        <section class="inspector-section">
          <h2>Contains</h2>
          {children.map((field) => {
            const child = node[field.key]
            const childVariant = isNode(child) ? variantOf(schema, 'texture', child.type) : undefined
            return (
              <button key={field.key} class="child-link" onClick={() => onSelect([...path, field.key])}>
                <span class="field-label">{field.label}</span>
                <span>{childVariant?.label ?? '?'}</span>
                <span class="chevron">›</span>
              </button>
            )
          })}
        </section>
      )}

      <section class="inspector-section">
        <h2>Structure</h2>
        <StructureActions schema={schema} node={node} examples={examples} onReplace={(next) => replace(next)} />
      </section>
    </div>
  )
}

function Breadcrumbs({ schema, root, path, onSelect }: { schema: Schema; root: Node; path: Path; onSelect: (path: Path) => void }) {
  const crumbs = path.map((_, i) => path.slice(0, i))
  if (crumbs.length === 0) return <h2>Texture</h2>
  return (
    <nav class="breadcrumbs" aria-label="Position in the texture">
      {crumbs.map((p) => {
        const n = getAt(root, p)
        return (
          <button key={pathKey(p)} class="crumb" onClick={() => onSelect(p)}>
            {n ? (variantOf(schema, 'texture', n.type)?.label ?? n.type) : '?'}
          </button>
        )
      })}
      <span class="crumb is-current">{fieldLabel(schema, root, path)}</span>
    </nav>
  )
}

function fieldLabel(schema: Schema, root: Node, path: Path): string {
  const parent = getAt(root, path.slice(0, -1))
  const last = path[path.length - 1]
  if (!parent) return last
  return textureFields(schema, parent).find((f) => f.key === last)?.label ?? last
}

interface FieldEditorProps {
  schema: Schema
  field: Field
  value: Json | undefined
  onChange: (value: Json) => void
  onRampChange: (ramp: Node, key: string) => void
}

function FieldEditor({ schema, field, value, onChange, onRampChange }: FieldEditorProps) {
  const min = field.min ?? 0
  const max = field.max ?? 1
  const step = field.step ?? 0.01
  switch (field.kind) {
    case 'scalar':
    case 'int':
      return (
        <ScalarField
          label={field.label}
          help={field.help}
          value={typeof value === 'number' ? value : 0}
          min={min}
          max={max}
          step={step}
          integer={field.kind === 'int'}
          onChange={onChange}
        />
      )
    case 'point':
    case 'vector': {
      const pair: [number, number] =
        Array.isArray(value) && typeof value[0] === 'number' && typeof value[1] === 'number' ? [value[0], value[1]] : [0, 0]
      return <PairField label={field.label} help={field.help} value={pair} min={min} max={max} step={step} onChange={onChange} />
    }
    case 'colour':
      return (
        <FieldRow label={field.label} help={field.help}>
          <ColourInput ariaLabel={field.label} value={parseColour(value)} onChange={(c) => onChange(formatColour(c))} />
        </FieldRow>
      )
    case 'enum':
      return (
        <FieldRow label={field.label} help={field.help}>
          <EnumSelect ariaLabel={field.label} value={String(value)} options={field.options ?? []} onChange={onChange} />
        </FieldRow>
      )
    case 'ramp':
      return isNode(value) ? (
        <div class="field-group">
          <span class="field-label">{field.label}</span>
          <RampEditor schema={schema} ramp={value} onChange={onRampChange} />
        </div>
      ) : null
    default:
      return null
  }
}

interface StructureProps {
  schema: Schema
  node: Node
  examples: Example[]
  onReplace: (node: Node) => void
}

/** Menus for reshaping the tree around the selected node. */
function StructureActions({ schema, node, examples, onReplace }: StructureProps) {
  const children = textureFields(schema, node).filter((f) => isNode(node[f.key]))
  const swapped = swapChildren(schema, node)
  return (
    <div class="structure-actions">
      <ActionMenu
        label="Wrap in…"
        options={wrapOptions(schema).map((o) => ({ value: `${o.type}.${o.key}`, label: o.label }))}
        onPick={(value) => {
          const [type, key] = value.split('.')
          onReplace(wrap(schema, node, type, key))
        }}
      />
      {children.length > 0 && (
        <ActionMenu
          label="Unwrap…"
          options={children.map((f) => ({ value: f.key, label: `Keep only ${f.label.toLowerCase()}` }))}
          onPick={(key) => onReplace(node[key] as Node)}
        />
      )}
      {swapped && (
        <button class="button" onClick={() => onReplace(swapped)}>
          Swap {children.map((f) => f.label.toLowerCase()).join(' and ')}
        </button>
      )}
      <ActionMenu
        label="Replace with example…"
        options={examples.map((e) => ({ value: e.id, label: e.document.name }))}
        onPick={(id) => {
          const example = examples.find((e) => e.id === id)
          if (example) onReplace(clone(example.document.texture))
        }}
      />
      <button class="button danger" title="Replace this node with a plain grey fill" onClick={() => onReplace(clone(schema.defaultTexture))}>
        Delete
      </button>
    </div>
  )
}

interface ActionMenuProps {
  label: string
  options: { value: string; label: string }[]
  onPick: (value: string) => void
}

/** A select used as a menu: it always shows its label and runs an action on choice. */
function ActionMenu({ label, options, onPick }: ActionMenuProps) {
  return (
    <select
      class="select action-menu"
      aria-label={label}
      value=""
      onChange={(e) => {
        const value = e.currentTarget.value
        e.currentTarget.value = ''
        if (value) onPick(value)
      }}
    >
      <option value="" disabled>
        {label}
      </option>
      {options.map((o) => (
        <option key={o.value} value={o.value}>
          {o.label}
        </option>
      ))}
    </select>
  )
}
