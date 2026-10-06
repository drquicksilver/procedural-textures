# Plan

Milestones are ordered and grouped into three phases. Each milestone should
land as one or more PRs that leave `stack build` and `stack test` green.

Items marked **Decision** need a call from Jules before (or early in) the
milestone. Each one has a suggested default.

## Principles

- **Textures are data.** A texture is a value of a plain data type that is
  interpreted in one place. Never a bare Haskell function. That is what makes
  JSON persistence, generated editor widgets, the gallery's `show` output, and
  later compilation to shaders possible.
- **Everything ends up 3D.** Phase 1 is deliberately 2D, but avoid choices
  that would make the move to `(x, y, z)` hard.
- **The golden suite is the safety net.** Golden images change only on
  purpose, and the PR says why. Optimisations must not change them beyond the
  tolerance.
- **Refactor freely.** The data types will change, especially in Phase 3.
  Migrate saved JSON instead of freezing the model early.
- **The editor is driven by a schema.** The frontend should learn which node
  types exist, what their fields are, and what their ranges and defaults are
  from a description the backend serves. Then adding a primitive in Phase 3 is
  mostly backend work.

---

## Phase 1: Interactive 2D editor

**Exit criteria:** a web app, run locally, that can
- edit every `Texture` and `ColourRamp` constructor using
  purpose-built widgets,
- render the result live from a Haskell backend in a 2D square, and
- save and load textures as JSON in local storage, with the current examples
  shipped as starting points.

### 1.1 Housekeeping
- Add a `library` stanza. The executables and the test suite depend on it
  instead of listing `src/` themselves. Move executable `Main`s into `app/`.
- CI: replace the archived `haskell/actions/setup` and drop the GHC 9.4.8 pin,
  since Stack uses the snapshot's GHC (9.10.x). Share the setup between the
  CI and Pages workflows. Run the tests before deploying Pages.
- Update `README.md` to cover the modules, the `--html` and `--benchmark`
  modes, `png-compare`, and this plan.
- Plain `stack run` writes to an output directory, not the repo root.

**Done when:** CI is green on the new layout, and nothing is compiled more than
once per build.

### 1.2 Golden regression suite
- Add a `golden/` directory holding expected PNGs for every example at a fixed
  size (128² is enough).
- Add a tasty test that renders each example and compares it with the expected
  PNG using `PNGCompareCore`, failing above a tolerance.
- Add an accept mode, for example `stack test --ta '--accept'`, to regenerate
  the expected images on purpose.
- Fix `png-compare` so it can be used for this:
  - Add a threshold flag and a non-zero exit code when the threshold is
    exceeded.
  - Report the maximum per-pixel error as well as the mean.
  - Treat fully transparent pixels as equal, whatever their RGB values.
- **Decision:** the tolerance policy. Suggested default: a near-zero mean and a
  small maximum for refactors. An optimisation that changes floating-point
  behaviour gets a looser, documented tolerance.
  **Decided:** mean ≤ 0.0001 and max ≤ 0.008 per pixel (about one 8-bit step
  in each channel), defined as `defaultTolerance` in `PNGCompareCore`.

**Done when:** changing a constant in any example makes `stack test` fail.

### 1.3 JSON serialisation
- Add aeson instances for `Texture`, `ColourRamp`, `RampMode` and `Colour`,
  using a readable tagged encoding, e.g.
  `{"type": "linear", "from": [0, 0.5], "to": [1, 0.5], "ramp": {...}}`.
- Use a document envelope: `{"version": 1, "name": ..., "texture": ...}`.
  Loading goes through a migration function keyed on `version` (a no-op for
  now).
- Export the examples to `examples/*.json`, which become the source of truth.
  The golden suite renders from these files.
- Add a CLI command `render <spec.json> <out.png> [--size N]`.
- Test that every example survives a JSON round trip.
- **Decision:** how colours are written. Suggested default: a hex string with
  alpha (`"#rrggbbaa"`) for readability, while also accepting `[r, g, b, a]`
  floats.
  **Decided:** colours are written as `"#rrggbbaa"` when every channel is
  exactly an 8-bit value, and as `[r, g, b, a]` otherwise, so encoding never
  loses precision. `"#rrggbb"` and 3-element arrays are accepted too.

### 1.4 Render server
- Add a small Haskell web server. Suggested: scotty on warp.
  - `POST /api/render`: takes a texture document plus size and returns a PNG.
    Malformed input gets a 400 with a JSON error that points at the bad path.
  - `GET /api/examples`: the shipped example documents.
  - `GET /api/schema`: node types, fields, field kinds (point, scalar, int,
    colour, ramp, child texture), ranges, steps and defaults.
  - Static serving of the built frontend.
- Test the endpoints in the tasty suite.
- Add a size limit on render requests, plus a timeout.

### 1.5 Architecture spike: example viewer
- Frontend skeleton: a list of examples and a square preview rendered by the
  backend. No editing yet.
- Set up the frontend build and its integration with the server (dev proxy, a
  production build served by Haskell), and add the frontend build to CI.
- **Decision:** frontend stack. Suggested default: TypeScript + Vite with a
  light component framework (Svelte or Preact). A recursive tree editor is
  painful to write without components.
  **Decided:** TypeScript + Vite + Preact, in `frontend/`, with vitest for
  unit tests. `make app` builds and serves everything; `make dev` runs the
  API with a hot-reloading frontend.

**Done when:** one command starts the app, and clicking each example renders it.

### 1.6 Interactive rendering performance
Do this before the editor so that editing feels live from the start. The golden
suite guards correctness.
- Compile ramps once: sort the stops when a ramp is built, not on every pixel.
- Render in parallel (pure per-pixel functions make this straightforward).
- Return a low-resolution preview first and a full-resolution render after it.
- Frontend: debounce requests and cancel any that have been overtaken.
- Add a proper benchmark harness (tasty-bench or criterion) to replace or
  supplement `--benchmark`, and record results so later implementations can
  be compared.

**Done when:** a 256² preview of the most expensive example (marble) comes back
fast enough to feel interactive while dragging a slider. Set a concrete target
once there are measurements.
**Target set and met:** a full 512² marble render including PNG encoding in
under 100 ms (84 ms), and a 96² low-resolution preview in under 10 ms (5 ms),
on a 10-core Apple laptop. Details in `bench/RESULTS.md`.

### 1.7 Structural editor
- A tree view of the texture with collapse and expand, and a selected node.
- A per-node inspector built from `/api/schema`:
  - a node-type dropdown; switching type keeps compatible fields and fills the
    rest with defaults
  - slider plus numeric entry for scalars, with schema-provided ranges and a
    way to go beyond them
  - steppers for integers (octaves, tile counts)
  - x/y inputs for points
- Tree operations: wrap a node in Layer, Tiled or Turbulence; unwrap; swap
  Layer children; replace a subtree with an example; delete (replacing with a
  default).
- Undo and redo, including keyboard shortcuts.
- Live preview on every change.

### 1.8 Colour and ramp widgets
- A colour picker with an alpha slider. Show transparency over a checkerboard.
- A ramp editor:
  - a gradient bar showing the ramp, with draggable stops
  - click to add a stop, delete stops
  - hard (discontinuous) stops, with two stops at the same position shown and
    editable clearly
  - a mode dropdown (Clamp, Wrap, Mirror)
  - Sinusoidal ramps as a ramp kind
- The ramp preview is evaluated on the client so it updates instantly. This
  needs the frontend to reproduce `evalRamp` exactly; the golden suite and a
  shared test vector file keep the two in step.

### 1.9 Direct manipulation on the preview
- Show handles on the preview square for the selected node's points: Linear
  from/to, Radial and Circular centres, and Circular radius. Drag to edit.
- Hover shows the coordinate under the cursor.

### 1.10 Persistence and library
- A local-storage library of documents: new (from an example or a blank
  texture), save, rename, duplicate, delete.
- Shipped examples are read-only; editing one copies it into the library.
- Import and export a JSON file.
- Loading runs the same version migration as the backend (or asks the backend
  to do it).
- Autosave the working document so that a reload loses nothing.

### 1.11 Phase 1 wrap-up
- Generate the static gallery from `examples/*.json`.
- Document how to run the app.
- **Decision:** hosting. GitHub Pages cannot run the Haskell backend.
  Suggested default: local-only for now, revisited when rendering moves into
  the browser.
  **Decided:** local-only (`make app`) for now.

### 1.12 Ramps as first-class objects
- Documents can define named ramps (a `ramps` map) and refer to them from
  any texture node (`{"type": "named", "name": …}`), so a ramp used in
  several places is defined once. This is document format version 2; the
  migration from version 1 is the first real use of the migration path.
- A built-in ramp library ships with the server (`ramps/*.json`, served by
  `GET /api/ramps`), referred to as `{"type": "builtin", "name": …}`.
  Built-in ramps are read-only. It covers scientific colour maps (viridis,
  magma, inferno, plasma, cividis), natural materials (terrain, sand,
  sandstone, woods, marble, granite, rust, moss, bark, lava, fire, ice,
  ocean, sky, clouds and more) and utilities (greyscale, fades to
  transparent, hard-edged stripes).
- In the editor, a ramp can be chosen from a picker showing gradient
  swatches: the document's own ramps, your saved ramps, and the built-in
  library. Shared and built-in ramps show where they come from; a shared
  ramp can be edited in place (changing every use), and either kind can be
  detached into a local copy. A local ramp can be shared within the
  texture or saved to your ramp library.
- Your saved ramps live in the browser beside your textures. Using one
  copies it into the document (decided: copy, not link), so documents stay
  self-contained and the server never needs the browser's storage.
- The server resolves references when rendering, with errors that name the
  path of a missing ramp. The shared ramp vectors cover the built-in
  library.

### 1.13 A broader example library, and multi-octave noise
- Bring forward from Phase 3 (decided) a multi-octave noise primitive
  (`fbm`): octaves of Perlin noise summed into a value for a ramp, with
  smooth, billowy and ridged styles. Most natural textures start from it.
- Add many more examples, especially natural-looking ones (wood, stone,
  terrain, clouds, fire, water, bark, rust and so on), each using named
  library ramps unless the ramp is a one-off special effect.
- Documents gain an optional `category` (natural, pattern, geometric,
  effect), used to group the Library dialog and the gallery.
- Keep a list of textures that still need missing primitives (cellular
  noise, transforms, blend modes): input for Phase 3.

**Done** (2026-10-05): `fbm` with smooth, billowy and ridged styles; 30 new
examples (44 in all), each a natural material, pattern, geometric or effect,
mostly on library ramps, with special effects (contour lines, camouflage,
plaid) using document-level named ramps; Library dialog and gallery grouped
by category. Clouds and Marble now use library ramps (their goldens changed
on purpose); Smiley's two eyes share one named ramp, with identical output.

**Textures that need what Phase 3 will add:**
- *Cellular / Worley noise:* leopard and giraffe spots, cracked mud,
  dry-stone walls, crocodile skin, stars, foam, and veined stone that
  breaks into cells.
- *Domain transforms (scale, rotate, repeat, offset):* bricks and tiles
  with offset rows and mortar, polka dots, herringbone, scales, rotated or
  diagonal stripes, anything that should repeat.
- *Anisotropic or noise-driven warps:* proper flames (tongues stretched
  upwards; `fire` is soft for want of this), wood grain that follows the
  rings, flowing water and hair. Turbulence warps the same amount in
  every direction at a fixed base scale.
- *Masks and blend modes:* weathering where one material shows through
  another along a noise mask, multiply for shading and dirt, screen for
  glow (the aurora wants additive light).
- *Field arithmetic:* combining two noise fields (terrain with a coastline
  falloff, ridged mountains only on high ground).

### 1.14 Noise without grid artefacts
- The 2D Perlin noise used 8 gradients along the axes and diagonals, which
  lines its features up with the lattice: ridged and billowy noise showed
  horizontal and vertical runs and right-angled turns (Caustics, Clouds,
  Ice, Embers). Use 16 unit gradients evenly spaced and turned 11.25°
  off the axes instead.
- Rotate each octave of fractal noise and turbulence by a further 0.83 rad,
  so the octaves' lattices don't line up with each other either.
- Re-measure the fractal-noise stretches for the new range. Every
  noise-based golden image changes on purpose.

### 1.15 Ramps are just colours; the mode belongs to the use
- A ramp is colour stops (or a sinusoidal ease between two colours) and
  nothing else. It is defined over its own stops' span, usually [0, 1].
- Clamp, wrap and mirror (how values beyond that span map back into it)
  become a `mode` field on each texture node that uses a ramp, so one
  library ramp can be clamped in one place and repeated in another
  (the desert dunes wanted a repeating Sand).
- Sinusoidal ramps lose their built-in back-and-forth: they ease once
  across [0, 1], and the mirror mode repeats them.
- Document format version 3. The migration moves each ramp's mode onto
  the node using it, including the modes of named ramps and of library
  ramps as they were in version 2, so old documents render as before.
  Library ramp files lose their mode; those designed to repeat say so.

### 1.16 OKLab colour blending
- Ramps interpolate between stops in the OKLab colour space instead of in
  sRGB, giving even, natural-looking gradients without the muddy or dark
  middles of sRGB blending.
- **Decided:** interpolation is premultiplied by alpha, as CSS Color 4
  specifies for gradients "in oklab", so a fade from a colour to
  transparent keeps its colour. Results are clamped to the sRGB gamut.
- Layering still composites in sRGB; blend modes are Phase 3.
- The browser's ramp previews use the same conversion, and the shared ramp
  vectors keep the two implementations in step.

**Done** (2026-10-05): 1.14–1.16. Moving the modes (1.15) left all 44 golden
images unchanged, which shows the migration preserves rendering exactly;
OKLab (1.16) changed 41 of them on purpose. OKLab costs nothing measurable,
but the noise change in 1.14 slowed noise-heavy textures; after optimising,
a 512² marble with PNG encoding takes about 103 ms, just over the 1.6 target
of 100 ms (see `bench/RESULTS.md`). A straight line in OKLab between
opposite hues passes near grey (red to blue goes through a greyish pink);
interpolating in OKLCh, round the hue circle, would be a possible option.

**Phase 1 status: complete** (2026-10-05), including 1.12–1.16, which
were added after 1.11. Beyond the milestones: `make e2e` runs end-to-end
browser tests of the editor as the slower secondary suite (also in CI), and
`test-vectors/` holds fixtures (the schema and sampled ramps) that the
Haskell suite writes and the frontend tests read.

The subsequent gallery expansion has 67 examples. Its composition studies
provide concrete requirements for the model refinement in 3.1 below.
The follow-up review's persistence gaps are fixed, and its document-version
correction is reflected in 2.2. A full 67-example performance sweep now covers
rendering and PNG encoding at preview and gallery sizes. The original latency
targets are no longer met across the library; Cumulus is the slowest example,
with Mossy Stone, Rust and Ice also expensive. See `bench/RESULTS.md` for the
measurements and the profiling follow-up.

---

## Phase 2: From 2D to 3D

**Exit criteria:** textures are 3D fields. The app shows them on several 3D
shapes, including shapes with cutouts that expose the inside of the solid, and
also as a 2D slice through the solid.

### 2.1 3D coordinate model (design)
- Choose the 3D noise with 1.14 in mind. Perlin's 2002 improved noise uses
  12 gradients towards cube edges, and its axis-aligned slices (exactly what
  the editor shows) have the same grid artefacts that 2D Perlin noise had.
  Consider more, better-spread gradients, or simplex-style noise.
- Textures are evaluated at `(x, y, z)`. A 2D image is a planar slice.
- Decide how each primitive lifts to 3D:
  - Linear: a planar gradient along a 3D direction.
  - Circular: spherical shells.
  - Perlin: 3D noise.
  - Turbulence: a 3D domain warp.
  - Tiled: a 3D checker.
  - Layer: unchanged.
  - Radial: has no natural 3D form. It needs an axis (a cylindrical angle);
    redesign it rather than port it.
- **Decision:** the coordinate conventions: unit cube or centred
  `[-1, 1]³`, handedness, and which way is up.

### 2.2 3D texture core
- Add `perlin3` (Perlin's 2002 improved noise) and 3D turbulence.
- Lift every primitive to 3D. Points become 3-vectors.
- Introduce the next unused JSON document version (currently version 4;
  versions 1–3 are existing 2D formats). Migrate documents from each supported
  version, preserving named ramps and ramp modes, adding z = 0 and sensible
  defaults for new fields. Test migrations from versions 1, 2 and 3 against
  their fixtures and the current example library.
- Golden suite: regenerate on purpose. Images whose meaning has not changed
  (the z = 0 slice of non-noise textures) should match the existing 2D goldens. The
  noise-based ones are accepted as new.
- Update the editor widgets for 3D points and directions.

**Done** (2026-10-06): 2.1–2.2. Three-dimensional fields, rotated
32-gradient improved noise, full 3D warps/checkers, cylindrical Radial axes,
and version-4 migrations. All 67 examples migrate; 11 non-noise slices retain
exact pixels. 56 noise goldens change intentionally. Decisions and evidence
are recorded in `docs/decisions/PHASE-2.md`.

### 2.3 Scene renderer
- Add a camera (orbit: yaw, pitch, distance; perspective).
- Define geometry as signed distance functions and render it by ray marching.
  This handles cutouts and boolean shape operations easily, and the SDF code is
  reused for the SDF texture primitives in Phase 3.
- Colour each hit point by sampling the texture at the hit point in object
  space, so the texture stays fixed to the object when it rotates.
- Simple shading: diffuse plus ambient, using normals from the SDF gradient.
  Background and light direction are fixed for now.
- Golden tests for a fixed set of shape × texture × camera scenes.

### 2.4 Shape library
- Sphere, cube, cylinder, torus.
- Shapes with cutouts that show the inside of the material:
  - a cube with a spherical bite taken out
  - a sphere with an octant removed
  - a cube cut by a plane
- A 2D slice view: a plane through the solid, with a slider for its position
  (and orientation).

### 2.5 3D viewer interaction
- A shape dropdown, drag to orbit, scroll to zoom.
- Render at low resolution while dragging and refine on release. Revisit
  performance: ray marching multiplies the per-pixel cost.
- The editor (tree, inspector, ramps, library) works unchanged next to the 3D
  view. Point handles work in slice view.
- Optional: an animated slice sweep through the solid.

### 2.6 Phase 2 wrap-up
- Gallery shows each example on a shape and as a slice.
- Benchmarks updated for 3D scenes.

---

## Phase 3: New primitives and a refined model

**Exit criteria:** the primitives below exist, are editable, and have golden
coverage. The data types have been reshaped into a deeply composable model we
are comfortable with: separate scalar fields, domain transforms, ramps and
compositing, combining freely. The refinement happens as the primitives are
added, not as a separate up-front redesign.

### 3.1 Model refactor: decompose the ADT
- Split today's constructors into their parts:
  - **scalar fields:** planar distance, point distance, noise
  - **domain transforms:** warps such as Turbulence
  - **ramps:** a scalar mapped to a colour
  - **compositing:** layering, masking
- Rebuild the existing primitives from these parts, ideally as JSON
  conveniences that expand into the new form, so documents stay readable.
- Golden suite: no image changes.
- Use the gallery composition work as design cases: gate mountain ridge
  detail by broad elevation; shade a contiguous union of cloud lobes without
  punching holes in its silhouette; replace knot interiors completely while
  applying a local, fading grain deflection outside; nest corrosion masks;
  and attenuate fragmented ripple crests with distance. These need reusable
  scalar masks, field arithmetic and independently composable domain transforms,
  rather than colour-layer approximations. Keep individual scalar fields and
  masks inspectable so macrostructure, detail and final composition can be
  rendered separately while tuning.
- **Decision:** the shape of the core types. Possible approaches include a
  typed GADT, separate mutually recursive types, or one untyped node graph
  checked separately. This is Jules's design work; record the outcome in a
  `DESIGN.md`.

### 3.2 Domain operators
- Translate, rotate, scale (affine transforms).
- Repeat (modulo), mirror, and polar or radial repeat.
- Twist and bend.

### 3.3 Warping
- Domain warping by any vector field, including warps applied repeatedly (fbm
  warping a fbm). `Turbulence` becomes one instance of this.
- fbm and turbulence as generic combinators over any noise source.

### 3.4 SDF primitives
- SDF shapes as scalar fields (sphere, box, torus, cylinder, plane), shared
  with the Phase 2 geometry code.
- Combine them with union, intersection and difference, in both hard and
  smooth versions.
- Contours, bands and outlines produced by passing an SDF through a ramp.

### 3.5 Worley / cellular noise
- F1, F2 and F2−F1 outputs.
- Distance metrics: Euclidean, Manhattan, Chebyshev.
- Jitter amount, and a seed.

### 3.6 Voronoi
- Cell identity used as a field: a random value or colour per cell, and the
  distance to the cell edge.
- This adds fields that are not plain scalars (cell IDs, per-cell random
  values), which tests the type design from 3.1.

### 3.7 Field combinators, masks and blend modes
- Arithmetic on fields: add, multiply, min, max, remap, threshold.
- Masks: blend between two textures using a scalar field.
- Blend modes beyond normal layering: multiply, screen, overlay, and so on.

### 3.8 Reaction–diffusion
- A Gray–Scott simulation on a 3D voxel grid, then sampled with trilinear
  interpolation.
- Architecturally new: this is a precomputed simulation, not a field
  evaluated point by point. It needs caching, explicit resolution and
  iteration-count parameters, and deterministic results so that golden tests
  work.

### 3.9 Phase 3 wrap-up
- JSON schema settled at its next version, with migrations from every earlier
  version.
- Editor widgets cover every primitive. Example library expanded to show the
  new primitives.
- Write up the design in `DESIGN.md`.

---

## Later (not yet planned)

- Moving rendering into the browser: compile textures to GLSL/WGSL, or run a
  JS/WASM implementation, with the golden suite as the conformance check.
- Volumetric rendering (density fields, clouds, hypertexture) as a side-quest.
- Hosting the app publicly.
- Comparing implementations with the benchmark harness (Haskell, browser, and
  others).
