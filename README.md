# procedural-textures

Procedural solid-material playground in Haskell, with an interactive 3D viewer. It defines a small algebra of
texture primitives (flat, linear, radial, circular, Perlin noise, fractal
noise, turbulence, tiled, layered) and colour ramps: multi-stop, possibly
discontinuous, blended in OKLab, and clamped, repeated or mirrored beyond
their ends wherever they are used. A
built-in library of over 40 ramps and 67 example textures, mostly
natural materials, ships with it.

Textures are plain data (`Texture` values) interpreted as three-dimensional colour fields in a
single place, and rendered to PNG with JuicyPixels. They are saved as JSON
documents; the examples live in `examples/*.json`.

Where this is heading is described in [`PLAN.md`](PLAN.md), the master plan.
[`IDEAS.md`](IDEAS.md) is a scratchpad of ideas.

## Layout

- `src/` the library:
  - `Colours` CSS colour constants and RGB helpers.
  - `ColourRamps` ramp modes and evaluation across arbitrary stops.
  - `Perlin` improved 3D noise with 32 rotated gradient directions.
  - `Texture` the texture ADT and its interpreter.
  - `Render` parallel JuicyPixels adapter and image writer.
  - `Vector3`, `Geometry` reusable vector math, signed-distance solids and booleans.
  - `Scene` perspective sphere tracing, lighting, orbit camera and planar slices.
  - `Gallery` shared gallery entries, section ordering and the per-shape
    materials; `HtmlOutput` and `ContactSheet` render the HTML site and PNG galleries.
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
see it rendered live in WebGL2, entirely in the browser.

```
make app    # build the static frontend and serve the editor at http://localhost:8080/
make dev    # hot-reloading frontend at http://localhost:5173/
make test   # Haskell and frontend tests, plus the production build
make e2e    # slower end-to-end browser tests (needs Chrome; CI runs them too)
```

Node 24 is needed for the editor; Stack is needed for the reference tools and
Haskell tests. `make app`, `make dev` and `make e2e` start no Haskell process.
The production files in `frontend/dist/` can be served by any static host,
including GitHub Pages; relative asset paths support repository subdirectories.
The Pages workflow will switch from the gallery to the editor in milestone 3.7.
A browser with WebGL2 support and hardware acceleration is required. The editor
shares one GL context between the viewer and thumbnails; ordinary frames go
straight to canvases, and only explicit PNG export reads pixels back. Shader
programs use a bounded cache that keeps the main viewer program resident.

Browser tests serve the production build under `/procedural-textures/` and
reject API requests. After building, a focused run is available with
`npm --prefix frontend run e2e -- --test-name-pattern="exports"`.
CI installs pinned Chrome and explicitly selects SwiftShader with
`GPU_BACKEND=swiftshader`; local runs use the hardware GPU by default.

### Using it

- **Structure** (left): the texture as a tree, each node with a live
  thumbnail of its subtree. Click a node, or move with the arrow keys, to
  select it; collapse nodes with the disclosure triangles or Left/Right.
- **Viewer** (centre): renders the material on a 3D solid. Choose sphere,
  cube, cylinder, torus or one of three cutaways that expose the material’s
  interior. Drag to orbit and scroll to zoom. Focus the image for keyboard
  controls: arrows orbit, +/− zoom, Home resets. Camera changes do not edit
  or autosave the texture. Rendering stays low-resolution during dragging
  and refines after release.
- **2D slice**: choose XY, XZ or YZ and move the position slider through
  the material. Selected points and spherical-shell radii have projected
  handles (Shift snaps to a 0.05 grid); moving a point preserves the coordinate
  outside the slice plane. The inspector edits all three coordinates directly.
  **Download PNG** exports the chosen view at 256, 512, 1024 or 2048 square
  pixels, retaining slice transparency.
  Slices are unlit material fields; XY at z=0 is the original diagnostic view.
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
  older versions migrated) entirely in the browser.

## WebGL2 spike (Phase 3.1)

The standalone preview renders Checker, Marble and Cumulus on the bitten cube
or XY/XZ/YZ slices entirely in WebGL2. Run `npm --prefix frontend run dev` and
open `/spike.html`; it needs no Haskell server. The main editor uses the
same renderer with direct canvas previews, thumbnails and PNG export.

Render PNGs and record timings with one headless Chrome context:

```
npm --prefix frontend run gpu:spike
npm --prefix frontend run gpu:spike -- --example marble --view scene --size 512
```

For unquantised material checks and shader development:

```
npm --prefix frontend run gpu:test -- --self-test
npm --prefix frontend run gpu:watch -- --case marble
npm --prefix frontend run gpu:spike -- --goldens --repeats 0
```

`gpu:watch` retains its browser context and refreshes the renderer when shader
source or fixtures change. `gpu:browser` installs the lockfile-pinned Chrome
revision used in CI; `GPU_BACKEND=swiftshader` selects verified software rendering.

Outputs go to `out/gpu-spike/`. Set `CHROME` if the executable cannot be found.
After `stack build`, add `--compare --check` to run Haskell image comparisons,
program-cache checks and standalone preview checks. The default 128² run passes
the existing tolerance. See [spike evidence and development details](docs/decisions/PHASE-3.md)
for the measured 512² precision difference and timing methodology.

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

Render a solid or an arbitrary principal-plane slice without a server:
```
mkdir -p out
stack run procedural-textures -- render examples/malachite.json out/malachite-solid.png --shape bitten-cube --size 512
stack run procedural-textures -- render examples/malachite.json out/malachite-slice.png --axis xz --slice 0.5 --size 512
```
The shapes are `sphere`, `cube`, `cylinder`, `torus`, `bitten-cube`,
`cut-sphere` and `cut-cube`, plus a Staunton chess set: `pawn`, `rook`,
`knight`, `bishop`, `queen` and `king`. The pieces share one scale (the king is
about 1.18 units tall) and stand centred on the object centre; their round
feet are 0.38 across for the pawn, 0.43 for the rook, knight and bishop, and
0.48 for the queen and king.

Rewrite documents in canonical form (the test suite checks that the examples
are canonical):
```
stack run procedural-textures -- format examples/*.json
```

Build the site in `site/` (published to GitHub Pages by CI):
```
stack run procedural-textures -- gallery
```
`gallery.html` shows each example as a 512×512 cutaway and a 512×512 slice
(`--size`). Each of the thirteen shapes also gets a `shape-<name>.html` page with
1024×1024 renders (`--shape-size`) of the same six materials: agate, walnut,
malachite, marble, tiger-eye and lava. `index.html` links to the gallery and
the thirteen shape pages.

Write a single contact-sheet PNG to `site/gallery.png` (or use `--out DIR`):
```
stack run procedural-textures -- gallery --contact-sheet
```
It has eight columns of square 128×128 previews, in adjacent solid/slice pairs, titles and full wrapped descriptions,
grouped under the same section headings as the HTML gallery. Its height grows
to fit the captions and sections. JSON documents are omitted; `--size` applies
only to HTML previews. The bundled Open Sans font keeps text rendering portable
without needing a browser or system fonts (see `fonts/LICENSE.txt`).

Benchmark rendering (wall-clock, all cores; see `bench/RESULTS.md`):
```
stack bench --ba '-j 1'
```
The default suite renders every example at 256² and includes PNG encoding at
96², 256² and 512² for representative textures. Use
`stack bench --ba '--full-library -j 1'` to measure PNG encoding for every
example; `-j 1` keeps separate benchmarks from competing while each render
still uses all cores. The default suite also covers representative 3D scenes.
Run the thirteen-shape × three-material × two-size scene suite separately:
```
stack bench --ba '--scenes-only -j 1 --csv out/phase2-scenes.csv'
stack bench --ba '--scene-stress -j 1 --csv out/phase2-scene-stress.csv'
```
[Scene baseline and measured limits](bench/PHASE-2-RESULTS.md) include close zoom
and maximum-resolution refinement.

Serve the rendering API on port 8080:
```
stack run texture-server
```
Its endpoints are `GET /api/schema`, `GET /api/examples`, `GET /api/ramps`,
`GET /api/shapes`,
`POST /api/render?size=N` (document in, PNG out) and `POST /api/migrate`
(document of any supported version in, canonical document out).
Scene requests use `view=scene&shape=bitten-cube&yaw=0.55&pitch=0.35&distance=2.1`;
slices use `view=slice&axis=xy&position=0.5`. Camera and plane values are finite
and bounded. Existing render requests without these options remain XY at z=0.

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
  "version": 4,
  "name": "Checker",
  "description": "An 8 by 8 by 8 solid checkerboard.",
  "texture": {
    "type": "tiled",
    "columns": 8,
    "rows": 8,
    "depth": 8,
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
the format changes. Version 4 uses `[x,y,z]` points and scales, a cylinder
`axis` on radial sweeps and `depth` on checkers. Versions 1–3 migrate through
the backend while retaining named ramps and ramp modes. Material coordinates
are right-handed: x right, y down, z into the default slice; viewing uses the
unit cube, but fields continue beyond it.

See [the Phase 2 decision log](docs/decisions/PHASE-2.md) for design choices,
validation and performance evidence.

## Golden images

`golden/textures/` holds the expected 128×128 render of every example. The
test suite renders each example and fails if it differs beyond a tight
tolerance (`defaultTolerance` in `PNGCompareCore`). This is the regression
suite for refactors and optimisations. `golden/scenes/` covers all thirteen shapes
on three materials. `golden/legacy-2d/` retains the original non-noise slices
and is checked byte-for-byte; it is never regenerated by accept mode.

When a change to the images (or to `test-vectors/`) is intended, regenerate
them and say why in the commit:
```
stack test --ta --accept
```

Static editor metadata is generated from the Haskell definitions and the JSON
example/ramp libraries with `stack run procedural-textures -- assets`. The
checked-in `frontend/src/generated/metadata.json` allows frontend builds without
Haskell. `stack test` checks for drift; after an intentional metadata or document
semantics change, regenerate shared fixtures using
`stack test --ta '--accept -p "Shared test vectors"'` and run frontend tests.
