# procedural-textures

Procedural texture playground in Haskell. It defines a small algebra of
texture primitives (flat, linear, radial, circular, Perlin noise, fractal
noise, turbulence, tiled, layered) and colour ramps: multi-stop, possibly
discontinuous, blended in OKLab, and clamped, repeated or mirrored beyond
their ends wherever they are used. A
built-in library of over 40 ramps and over 40 example textures, mostly
natural materials, ships with it.

Textures are plain data (`Texture` values) interpreted to pixel functions in a
single place, and rendered to PNG with JuicyPixels. They are saved as JSON
documents; the examples live in `examples/*.json`.

Where this is heading is described in [`PLAN.md`](PLAN.md), the master plan.
[`IDEAS.md`](IDEAS.md) is a scratchpad of ideas.

## Layout

- `src/` the library:
  - `Colours` CSS colour constants and RGB helpers.
  - `ColourRamps` ramp modes and evaluation across arbitrary stops.
  - `Perlin` 2D Perlin noise.
  - `Texture` the texture ADT and its interpreter.
  - `Render` JuicyPixels adapter and image writer.
  - `HtmlOutput` the static HTML gallery.
  - `PNGCompareCore` image comparison used by `png-compare`.
  - `TextureJson` the JSON document format.
  - `Examples` loads the example documents from `examples/`.
  - `RampLibrary` loads the built-in ramps from `ramps/`; `Resolve`
    replaces ramp references with their definitions before rendering.
  - `Schema` describes the texture language for the editor (node types,
    fields, widget kinds, ranges, defaults).
  - `Server` the editor's backend (rendering API and static files).
- `app/` executables: `procedural-textures` (CLI), `texture-server` (the
  editor backend), `png-compare`.
- `examples/` the example texture documents (the source of truth).
- `ramps/` the built-in ramp library, one ramp per file.
- `golden/` expected renders for the regression suite.
- `test/` the tasty test suite.
- `frontend/` the web editor (TypeScript, Vite, Preact), with vitest unit
  tests beside the code and browser tests in `frontend/e2e/`.
- `test-vectors/` fixtures shared by the Haskell and frontend tests, written
  by the Haskell suite (the schema and sampled ramps), so the two sides
  can't drift apart.
- `bench/` benchmarks and recorded results.

## The editor

A web editor for textures: edit any texture with purpose-built controls and
see it rendered live by the Haskell backend.

```
make app    # build everything and serve the editor at http://localhost:8080/
make dev    # API plus a hot-reloading frontend at http://localhost:5173/
make test   # Haskell and frontend unit tests, and the frontend type-check
make e2e    # slower end-to-end browser tests (needs Chrome; CI runs them too)
```

Stack and Node 24 are needed. The editor runs locally only: GitHub Pages
can't host the Haskell backend.

### Using it

- **Structure** (left): the texture as a tree, each node with a live
  thumbnail of its subtree. Click a node, or move with the arrow keys, to
  select it; collapse nodes with the disclosure triangles or Left/Right.
- **Preview** (centre): renders as you edit, at low resolution while things
  are changing and at full resolution once they settle. The selected node's
  points and radius have handles you can drag (hold Shift to snap to a 0.05
  grid). The coordinate under the pointer is shown in the corner. The
  description is editable underneath.
- **Inspector** (right): the selected node's type (switching keeps whatever
  fields the two types share), its fields (sliders for the usual range,
  plus text entry that can go beyond it; arrow keys nudge, Shift for ten
  times as much), its children, and structure actions: wrap it in a
  layer, checkerboard or turbulence, unwrap it, swap a layer's top and
  bottom, replace it with an example, or delete it.
- **Ramps**: drag the markers under the gradient bar to move stops, click
  the bar to add one, select a marker and press Delete to remove it.
  "Split into hard edge" duplicates a stop in place; markers for stops
  sharing a position sit side by side. "Beyond the ends" on the node
  chooses whether values past the ramp's ends clamp, repeat or mirror, and
  the thin strip under the ramp shows the effect.
- **Ramp library**: "Choose…" on any ramp opens the picker: ramps shared
  within this texture, your saved ramps, and the built-in library (natural
  materials, sky, fire, water, scientific colour maps and more). A library
  ramp is used by reference; "Customise" makes an editable copy. "Share…"
  defines a ramp once in the texture so other nodes can use it (editing a
  shared ramp changes every use), and "Save to My ramps…" keeps a copy for
  other textures. Using a saved ramp copies it into the texture, so
  textures stay self-contained.
- **Undo/redo**: ⌘Z / ⇧⌘Z (Ctrl+Z / Ctrl+Y elsewhere). Dragging or typing
  in one field is a single undo step.
- **Library**: textures are saved automatically in the browser. Examples
  are read-only; your first edit saves a copy to your textures. The
  Library dialog opens, renames, duplicates and deletes them. Import and
  Export read and write texture JSON files; imports are validated (and
  older versions migrated) by the server.

## Running

Render the examples as 128×128 PNGs into `out/` (`--size`, `--out` and
`--examples` change the defaults):
```
stack run procedural-textures
```

Render one document:
```
stack run procedural-textures -- render examples/marble.json marble.png --size 512
```

Rewrite documents in canonical form (the test suite checks that the examples
are canonical):
```
stack run procedural-textures -- format examples/*.json
```

Render the 512×512 gallery into `site/` (published to GitHub Pages by CI):
```
stack run procedural-textures -- gallery
```

Benchmark rendering (wall-clock, all cores; see `bench/RESULTS.md`):
```
stack bench
```

Serve the rendering API on port 8080:
```
stack run texture-server
```
Its endpoints are `GET /api/schema`, `GET /api/examples`,
`POST /api/render?size=N` (document in, PNG out) and `POST /api/migrate`
(document of any version in, canonical document out).

Compare two PNGs. Prints the mean and maximum per-pixel RGBA distance, and
exits 1 if a threshold is given and exceeded:
```
stack run png-compare -- [--threshold MEAN] [--max-threshold MAX] a.png b.png
```

Run the tests:
```
stack test
```

## Texture documents

```json
{
  "version": 3,
  "name": "Checker",
  "description": "An 8 by 8 checkerboard.",
  "texture": {
    "type": "tiled",
    "columns": 8,
    "rows": 8,
    "a": {"type": "flat", "colour": "#e6e6e6ff"},
    "b": {"type": "flat", "colour": [0.1, 0.1, 0.1, 1]}
  }
}
```

Textures and ramps are objects tagged with `"type"`. A ramp is just colours;
each node that uses one also says what happens beyond the ramp's ends
(`"mode"`: `clamp`, `wrap` or `mirror`). A ramp can also be a
reference: `{"type": "named", "name": "eye"}` to a ramp defined in the
document's own `"ramps"` map, or `{"type": "builtin", "name": "viridis"}`
to a ramp in `ramps/`. Colours are
`"#rrggbbaa"` strings when exactly representable with 8-bit channels and
`[r, g, b, a]` arrays otherwise. `version` lets old documents be migrated when
the format changes.

## Golden images

`golden/textures/` holds the expected 128×128 render of every example. The
test suite renders each example and fails if it differs beyond a tight
tolerance (`defaultTolerance` in `PNGCompareCore`). This is the regression
suite for refactors and optimisations.

When a change to the images (or to `test-vectors/`) is intended, regenerate
them and say why in the commit:
```
stack test --ta --accept
```
