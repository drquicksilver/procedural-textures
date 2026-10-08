# Plan

Milestones are ordered and grouped into five phases. Each milestone should
land as one or more PRs that leave `stack build` and `stack test` green.

Items marked **Decision** need a call from Jules before (or early in) the
milestone. Each one has a suggested default.

## Principles

- **Textures are data.** A texture is a value of a plain data type that is
  interpreted in one place. Never a bare Haskell function. That is what makes
  JSON persistence, generated editor widgets, the gallery's `show` output, and
  compilation to browser shaders possible.
- **Everything ends up 3D.** Phase 1 is deliberately 2D, but avoid choices
  that would make the move to `(x, y, z)` hard.
- **The golden suite is the safety net.** Golden images change only on
  purpose, and the PR says why. Optimisations must not change them beyond the
  tolerance.
- **Refactor freely.** The data types will change, especially in Phase 4.
  Migrate saved JSON instead of freezing the model early.
- **The editor is driven by a schema.** The frontend should learn which node
  types exist, what their fields are, and what their ranges and defaults are
  from a shared description exported by Haskell at build time. Phase 1 serves
  it from the backend; Phase 3 ships it as a static asset. Every Phase 4
  primitive must have both a Haskell evaluator and a browser shader implementation.

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
- Bring forward from Phase 4 (decided) a multi-octave noise primitive
  (`fbm`): octaves of Perlin noise summed into a value for a ramp, with
  smooth, billowy and ridged styles. Most natural textures start from it.
- Add many more examples, especially natural-looking ones (wood, stone,
  terrain, clouds, fire, water, bark, rust and so on), each using named
  library ramps unless the ramp is a one-off special effect.
- Documents gain an optional `category` (natural, pattern, geometric,
  effect), used to group the Library dialog and the gallery.
- Keep a list of textures that still need missing primitives (cellular
  noise, transforms, blend modes): input for Phase 4.

**Done** (2026-10-05): `fbm` with smooth, billowy and ridged styles; 30 new
examples (44 in all), each a natural material, pattern, geometric or effect,
mostly on library ramps, with special effects (contour lines, camouflage,
plaid) using document-level named ramps; Library dialog and gallery grouped
by category. Clouds and Marble now use library ramps (their goldens changed
on purpose); Smiley's two eyes share one named ramp, with identical output.

**Textures that need what Phase 4 will add:**
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
- Layering still composites in sRGB; blend modes are Phase 4.
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
provide concrete requirements for the model refinement in 4.1 below.
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
  reused for the SDF texture primitives in Phase 4.
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

**Done** (2026-10-06): 2.3–2.4. Bounded perspective SDF renderer,
seven solids/cutaways, movable XY/XZ/YZ slices, validated render controls and
CLI rendering. Analytic coverage and 21 shape/material scene goldens pass.

### 2.5 3D viewer interaction
- A shape dropdown, drag to orbit, scroll to zoom.
- Render at low resolution while dragging and refine on release. Revisit
  performance: ray marching multiplies the per-pixel cost.
- The editor (tree, inspector, ramps, library) works unchanged next to the 3D
  view. Point handles work in slice view.
- Optional: an animated slice sweep through the solid.

**Done** (2026-10-06): 2.5. Seven-shape viewer with mouse and keyboard
orbit/zoom, low-resolution interaction and refinement on release, all three
slice orientations and depth-preserving projected handles. Existing editor
and persistence workflows plus new viewer checks pass in the browser.

### 2.6 Phase 2 wrap-up
- Gallery shows each example on a shape and as a slice.
- Benchmarks updated for 3D scenes.

**Done** (2026-10-06): 2.6. All 67 materials have paired cutaway/slice
HTML previews and adjacent square previews in the contact sheet. The 42-case
scene baseline and five extreme-view cases are recorded in
`bench/PHASE-2-RESULTS.md`. Documentation and standalone CLI rendering cover
the complete workflow.

**Phase 2 status: complete** (2026-10-06). All 444 Haskell tests, 302
frontend unit tests, the production build and 16 browser tests pass. Original
non-noise slices remain byte-identical; the intentionally accepted 3D noise
and scene goldens are documented. Autonomous decisions and their evidence
are in `docs/decisions/PHASE-2.md`.

---

## Phase 3: Full client-side texture editor

**Exit criteria:** the complete editor runs from static files with no Haskell
server: every existing texture, ramp and shape renders in the browser; editing,
orbiting, slicing, thumbnails, persistence, JSON import/migration/export and PNG
export work locally; and the app is deployed on GitHub Pages alongside the
existing gallery. Browser output is checked against the Haskell reference, with
measured numerical tolerances and recorded performance on desktop and mobile.

**Decided:** WebGL2 first, with GLSL ES 3.00 generated from texture trees.
Haskell remains the reference evaluator, CLI renderer and source of build-time
metadata and fixtures. It is not required at runtime. WebGPU, a WASM port and a
second CPU browser renderer are deferred unless evidence justifies them.

### 3.1 WebGL2 architecture and rendering spike
- Keep the renderer independent of Preact: accept a WebGL2 context, resolved
  texture document, view options and output dimensions. Reuse it in the editor
  and a minimal headless test page.
- Draw a fullscreen triangle; calculate camera rays in the fragment shader,
  sphere-trace geometry, sample the material only at the surface hit, and apply
  the existing ambient/diffuse lighting and background.
- Start with Checker, Marble and Cumulus on the bitten cube, plus XY/XZ/YZ
  slices. Preserve object-space coordinates, camera conventions, bounding-sphere
  rejection and bounded tracing from the Haskell implementation.
- Generate GLSL functions from the texture tree rather than interpreting the
  tree per pixel. Share noise/colour/geometry helpers. Put editable parameters
  in uniforms or data textures so camera and numerical edits do not recompile
  shaders; cache programs by structural requirements.
- Measure image fidelity, compilation latency and rendering on the development
  laptop and a phone before completing the port. Set explicit preview,
  refinement and shader-compilation targets from those results.

**Done when:** the spike renders through the same module in a visible browser
and the CLI harness, comparisons expose any discrepancies, and measurements
support proceeding with the selected architecture.

**Completed** (2026-10-06): independent WebGL2 spike, standalone preview
and single-context CLI harness render Checker, Marble and Cumulus on the bitten
cube and all three slice planes. All 12 128² comparisons pass the existing
Haskell tolerance. Completed 512² scene medians on the M1 Pro are 0.8 ms,
3.0 ms and 8.1 ms respectively; one Marble pixel exceeds the original maximum
error at that size. Desktop evidence, provisional targets and reproduction are
in `docs/decisions/PHASE-3.md`. The focused discrepancy investigation and later conformance work are recorded
there. Final desktop and physical iPhone 16 Pro measurements in 3.6 support
the WebGL2 architecture and adaptive 15 ms preview target.

### 3.2 Command-line shader development and conformance harness
- Extend the existing Puppeteer infrastructure with a minimal render page that
  loads no editor UI. Start one headless Chromium process and reuse its page
  and WebGL2 context across cases; provide single-case and watch commands.
- Render to an explicit RGBA8 framebuffer, read pixels, flip rows for PNG
  output and compare with the existing `png-compare` tooling. Avoid page
  screenshots; fix resolution, sampling positions, colour encoding, blending
  and antialiasing independently of CSS and device pixel ratio.
- Report generated shader source with useful compile/link diagnostics and
  texture-node context. Optional `glslangValidator` checks provide fast GLSL
  validation, but browser compilation and rendering are authoritative.
- Check primitive sample vectors as well as full texture and scene images:
  negative coordinates, noise lattice boundaries, octave transforms, ramp
  clamp/wrap/mirror, duplicate stops, premultiplied OKLab interpolation and
  alpha compositing. Use diagnostic shader passes to isolate failures.
- Keep the Haskell goldens as the reference. Measure differences from GPU
  32-bit arithmetic versus Haskell `Double`, then document explicit tolerances
  for material samples and images, including discontinuities and silhouettes.
  Do not regenerate reference images simply to accommodate a porting error.
- Pin the browser and rendering backend for CI; configure and verify software
  rendering where supported, and record the actual backend. Use real hardware
  for performance measurements and additional browser/device checks.
- Keep shader-generator and document unit tests fast. GPU image comparisons
  and full editor browser tests belong in the secondary suite, run in CI and
  after renderer/editor behaviour changes.

**Done when:** one command renders and compares a chosen case without opening
an interactive browser window, and a deliberately wrong shader fails the suite.

**Done** (2026-10-06): pinned headless Chrome/SwiftShader CI, single-case and
watch commands, float material/noise/distance diagnostics, annotated shader
errors, raw-framebuffer PNG comparisons and deliberate wrong-shader checks.
One browser/context is reused across cases. Image tolerances are unchanged;
measured sample tolerances and development commands are documented in
`docs/decisions/PHASE-3.md`.

### 3.3 Complete texture, ramp and geometry shader support
- Port every current texture constructor, including nested turbulence,
  multi-octave noise styles, 3D checkers and layers. Match the current Perlin
  gradients/permutation, octave rotations, contrast mappings and wrapping.
- Resolve named and built-in ramps in the client. Preserve stable duplicate
  stop order, hard edges, alpha handling, sinusoidal easing and OKLab gamut
  clamping. Precompute invariant ramp data outside the per-pixel path.
- Port all current geometry operations and shapes: the seven original
  solids/cutaways and all six Staunton chess pieces, including their lathe
  profiles, smooth combinations and other distance-function helpers.
- Keep slice rendering and scene rendering on the same material function.
- Preserve useful optimisations such as opaque-layer skipping and reuse of
  identical noise/warp samples within the same coordinate domain.
- Bound shader/resource complexity and report unsupported or excessive
  documents clearly. Check WebGL2 availability, device limits and context loss;
  restore renderer state after context restoration without losing edits.

**Done when:** every shipped example and all existing scene golden cases pass
browser comparisons, with targeted coverage of all shapes and constructors.

**Done** (2026-10-06): every texture/ramp constructor, named/built-in ramps and
all 13 shapes compile in the standalone browser preview. Haskell exports the
actual shape trees and shared reference samples. All 67 texture and 39 scene
goldens pass at their native sizes on hardware and SwiftShader, with no golden
image changes. Coverage includes 107 material cases, raw noise, 1,125 geometry
samples, exact warp sharing/domain isolation, numerical-edit program reuse,
resource limits and forced context loss/restoration. Camera/projection
precomputation and accurate sinusoidal easing remove software-backend
approximation errors. Editor integration remains 3.5; mobile/device measurements
remain outstanding in 3.1/3.6.

### 3.4 Static metadata and client document processing — complete
- Export schema, examples, built-in ramps and shape metadata as versioned
  build-time assets from the existing sources of truth. Preserve canonical
  ordering and verify generated assets against the Haskell definitions.
- Replace `/api/schema`, `/api/examples`, `/api/ramps` and `/api/shapes` with
  bundled data or base-path-aware static loads.
- Replace `/api/migrate` with client validation, canonicalisation and migrations
  from every supported document version. Preserve path-specific errors, ramp
  reference checks and the historical version-2 built-in ramp modes.
- Share migration/validation fixtures with Haskell and compare canonical output;
  cover malformed inputs and old autosaved/library documents as well as files.
- Keep the current document format unless a real representation change needs a
  new version. Moving the renderer alone does not change document semantics.

**Done when:** the editor loads its full library and imports/migrates supported
documents using only static assets, with conformance fixtures passing.

**Implemented:** one versioned Haskell export bundles the schema, all 67 examples,
built-in ramps, shape labels/models and historical v2 ramp modes. Regenerate it
with `stack run procedural-textures -- assets`; the Haskell suite detects drift.
Shared fixtures check 279 historical/invalid documents against reference parsing,
resolution and canonical output. Current autosaves and library loads are validated
as well as imports, retaining identity for canonical documents. JavaScript rejects
unsafe integers and non-finite scalar inputs explicitly rather than silently losing
precision. Document format remains v4. Validation: 473 Haskell tests, 592 frontend
tests, all 16 editor browser tests and all 123 GPU conformance cases passed.
New metadata/migration fixtures were accepted deliberately; render goldens are
unchanged.

### 3.5 Integrate the browser renderer with the complete editor — complete
- Replace rendering API calls for the main viewer and subtree thumbnails.
  Display directly on canvases; reserve PNG encoding/readback for export and
  test tooling rather than every interactive frame.
- Preserve tree/inspector editing, ramp widgets, projected slice handles,
  undo/redo, autosave, library operations and JSON import/export.
- Schedule the latest state without queuing obsolete work. Adapt interaction
  resolution to a 15 ms frame budget and retain settled full-resolution refinement.
  Reuse programs and resources across views and thumbnails; dispose obsolete
  resources and avoid recompilation during ordinary slider or camera edits.
- Provide PNG download for the chosen view and resolution, preserving slice
  transparency and scene compositing.
- Adapt `make app`, `make dev` and browser tests to serve the static editor
  without starting `texture-server`. Retain the Haskell reference tools for
  development and comparisons.

**Done when:** the full editor browser suite passes against a static server,
with no `/api/*` requests and no Haskell process running.

**Implemented:** the complete editor now runs from static files. Viewer and
subtree/library thumbnails share one WebGL2 context and copy frames directly
into display canvases; interactive rendering performs no pixel readback or PNG
encoding. A cancellable frame queue prioritises the viewer and export, coalesces
edits to the latest state, adapts interaction resolution to a 15 ms budget and
refines after 180 ms to
the frame's device-pixel size (capped at 1024²). GPU allocations are reused; the
bounded eight-program cache retains the current viewer program, and a 300-entry
canvas cache serves revisited thumbnails. Context restoration redraws the latest
material, and component/page cleanup cancels work and releases resources.
PNG download offers 256–2048² for the selected solid or slice, retaining slice
alpha and scene compositing. `make app`, `make dev` and `make e2e` start only
Node; relative production asset paths work beneath a repository prefix. CI's
editor job uses pinned Chrome/SwiftShader and no Haskell setup. Reference CLI,
server and comparison tools remain available.

**Validation:** `stack build` and all 473 Haskell tests; 592 frontend tests and
production build; all 19 editor browser tests on both Apple M1 Pro/ANGLE and
SwiftShader, served under `/procedural-textures/`. The browser suite asserts zero
API requests and zero readbacks/PNG encodes except explicit export, and checks
camera/numeric shader reuse, export size/alpha and context restoration. All 123
GPU conformance cases (including harness self-checks) and 106 unchanged golden
image comparisons passed on the hardware backend. Desktop layout was inspected.
No document-format change or render-golden regeneration was needed. Broader
browser/mobile measurements and tuning remain 3.6; publishing remains 3.7.

**Adaptive resolution follow-up:** full-size previews are now the starting point,
not a fixed 96² pass. Warm GPU timer queries measure draw work asynchronously,
combined with CPU preparation/presentation cost; CPU elapsed time is used when
timer queries are unavailable. The controller estimates cost per pixel, reserves
15% headroom, reduces resolution promptly over budget and damps increases.
Interaction sizes range from 64² to the viewer's full size; settled renders still
use full resolution. Cold shader compilation is excluded, delayed/invalid timing
results are ignored, outstanding queries are bounded, and context loss resets the
estimate. This is a target rather than a hard deadline: compilation, sudden cost
changes and the minimum resolution can still exceed 15 ms. Controller/query
lifecycle tests and the complete hardware editor suite pass (602 frontend tests,
19 browser cases), with no interactive readback. On M1 Pro, warm Gradient and
Marble motion retained the full 704² view at roughly 0.7–0.8 and 7–12 ms respectively.
At high DPI, Gradient stayed at 1024² while Marble adapted around 672–704².
The focused adaptive-motion and context-restoration cases also passed on SwiftShader.

### 3.6 Performance and browser compatibility
- Record browser/GPU/backend, shader compilation, first render, steady-state
  interaction, refinement and PNG-export timings. Compare against
  `bench/PHASE-2-RESULTS.md` with equivalent materials, cameras and sizes;
  distinguish shader execution from compilation, readback and encoding.
- Include noise-heavy examples, close zoom, 1024² refinement, thumbnails and
  chess pieces. Check repeated structural edits for memory/resource growth.
- Test Chrome, Firefox and Safari on desktop, plus representative mobile
  devices. Verify WebGL2 context creation and actual render paths; retain
  hardware checks even when software rendering makes CI repeatable.
- Tune resolution and work scheduling to meet the 3.1 targets. Document the
  supported-device policy and show a useful message if WebGL2 cannot run.
  Add a CPU fallback only if measured compatibility needs justify its cost.

**Done when:** conformance and interaction targets are met on the recorded
supported devices, and limitations and measurements are documented.

**Completed for the recorded release devices** (2026-10-06): Chrome 154, Firefox 156
and Safari 26.6 on M1 Pro hardware pass 123 sample cases, 106 reference PNGs
and editor workflow smoke checks. The 15 ms adaptive renderer handles warm
interaction; resource stress retains at most eight programs and disposal
releases all objects. Narrow layouts and unavailable-WebGL2 behavior have
browser coverage. Timings, CPU comparisons and the supported-device policy
are recorded in `bench/PHASE-3-RESULTS.md` and `docs/decisions/PHASE-3.md`.
Firefox's 1.22-second cold first render misses the provisional one-second
goal and remains a documented limitation. **Physical mobile validation:**
the iPhone 16 Pro in Mobile Safari completed nine render cases, with default
512² warm medians of 1/4/10 ms and cold first renders below 0.5 seconds. Its
Checker slice matches the reference exactly; the user reports silky smooth
interaction. Close 1024² Cumulus takes 32 ms, supporting adaptive resolution.
This closes the phone measurement requirement in 3.1. Android and other mobile
devices remain best effort until measured; the focused phone check does not
claim the full desktop conformance suite.

### 3.7 GitHub Pages deployment and Phase 3 wrap-up
- Extend the existing Pages workflow to build and publish the complete editor
  alongside the gallery, with clear links between them. Use the project Pages
  base path for every script, static asset and navigation URL.
- Run Haskell checks, frontend unit tests/build, GPU conformance and static-app
  browser tests before deployment. Haskell may generate assets and reference
  renders in CI; the published site contains only static browser assets.
- Smoke-test the built site under the repository subpath, including direct
  loading/reloading, examples, shapes, editing, import/export and persistence.
  Explain that browser storage is local to each origin/device; existing
  localhost libraries can move to the hosted app through JSON export/import.
- Document local development, shader watch/render commands, golden comparison,
  PNG export, compatibility, hosting and the role of the Haskell reference.
- Record the shader architecture and measured tolerance/performance policies
  in `docs/decisions/PHASE-3.md` for future primitive additions.

**Done when:** the GitHub Pages editor supports the complete workflow without a
render server, and a clean checkout can build, test and publish that static app.

**Completed** (2026-10-06): the Pages workflow
builds and checks the static editor plus reference-generated gallery; the
editor is the site root, the gallery is under `gallery/`, and old gallery HTML
URLs redirect. Browser tests exercise the assembled artifact beneath the
repository prefix. Local commands, storage migration, compatibility,
architecture and measurements are documented. The editor is deployed at
[the public Pages URL](https://drquicksilver.github.io/procedural-textures/).
The final release passes all CI jobs, 473 Haskell tests, 603 frontend tests,
123 GPU sample cases, 106 unchanged reference PNGs and all 22 browser tests
against both the assembled Pages artifact and the actual public site. The
physical iPhone measurements and remaining device/cold-start limits are
recorded in 3.6; Phase 3 is complete for these recorded release devices.

---

## Review follow-up before Phase 4 — 2026-10-06

Work through the Phase 2/3 review findings before adding language primitives:

- Correct constant-ramp interior alpha and fBm fallback style mapping; extend
  reference-generated material fixtures without changing image goldens.
- Make warp sharing lazy, retaining reuse only within the same coordinate domain.
- Support custom and empty gallery libraries, with valid preview links.
- Consolidate renderer-readiness checks after navigation/reload; exercise delayed frames.
- Name and centralize GPU parameter packing/decoding; resolve generic editor nodes
  into a validated, typed compiler representation without redesigning the format.
- Derive structural field validation from reference metadata while keeping slider
  hints separate. Improve shader readability and separate build paths from browser tools.
- Measure cold library selection and structural-edit stalls, make busy feedback
  paint before rendering, and evaluate asynchronous compilation and warming.
  Keep resource bounds, reference comparisons and existing device-support limits.

**Completed** (2026-10-07): all five findings are fixed with targeted regressions.
The compiler consumes validated concrete material types; parameter packing and
GLSL layout constants are shared, structural field rules come from Haskell
metadata, and Pages assembly uses a paths-only module. Cold structural changes
show preparation feedback before blocking work; warm frames keep one-frame
scheduling and offscreen thumbnail warming is deferred. Link-first compilation
retains annotated error diagnostics. The bounded parallel-compilation experiment
and driver-cache limitations are recorded in `bench/PHASE-3-REVIEW.md`.
Production asynchronous compilation and a worker rewrite were not selected.
Validation: 475 Haskell tests, 612 frontend tests/build, 135 Chrome GPU cases
including mutation/lifecycle/warp instrumentation, native Firefox and Safari
135-case/106-image/resource checks, and all 25 browser tests against the rebuilt
Pages artifact. Only shared semantic/schema fixtures were intentionally extended;
image goldens and tolerances are unchanged. Phase 4.1's core model design remains
Jules's work; this strengthens the current boundary without choosing that model.

---

## Phase 4: New primitives and a refined model

**Exit criteria:** the primitives below exist, are editable, and have golden
coverage in both the Haskell reference and browser renderer. The data types
have been reshaped into a deeply composable model we
are comfortable with: separate scalar fields, domain transforms, ramps and
compositing, combining freely. The refinement happens as the primitives are
added, not as a separate up-front redesign.

### 4.1 Model refactor: decompose the ADT
- Split today's constructors into their parts:
  - **scalar fields:** planar distance, point distance, noise
  - **domain transforms:** warps such as Turbulence
  - **ramps:** a scalar mapped to a colour
  - **compositing:** layering, masking
- Rebuild the existing primitives from these parts, ideally as JSON
  conveniences that expand into the new form, so documents stay readable.
- Golden suite: no image changes in either implementation. Update shader
  generation, static schema and client validation/migrations with the model.
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

**Completed** (2026-10-07): Jules chose separate mutually recursive scalar,
vector, domain and colour types, retaining ordered `Compose`. Reference
evaluation lowers legacy conveniences into the decomposed core. Version 5,
reference-derived typed schema/validation, browser compilation and typed tree
actions are in place. Basic scalar arithmetic/remap/threshold and scalar mixing
cover the composition design cases; fields can be inspected and exported
separately. Existing image goldens are unchanged. See `DESIGN.md` and
`docs/PHASE-4.md` for conventions and examples.

### 4.2 Domain operators
- Translate, rotate, scale (affine transforms).
- Repeat (modulo), mirror, and polar or radial repeat.
- Twist and bend.

**Completed** (2026-10-07): all operators work independently on scalar, vector
and colour fields, with explicit composition order, inverse affine conventions
and defined origin/degenerate cases. New example materials demonstrate every
operator, including medallions, rosettes, mirrored veins and bent/twisted ribbons.

### 4.3 Warping
- Domain warping by any vector field, including warps applied repeatedly (fbm
  warping a fbm). `Turbulence` becomes one instance of this.
- fbm and turbulence as generic combinators over any noise source.

**Completed** (2026-10-07): arbitrary scalar-component/analytic vector fields,
scalar attenuation, addition and transformed vector sampling drive domain warps.
Nested/ordered warps can displace an fBm field repeatedly. Fractal and absolute-
fractal sums accept any scalar source while preserving legacy octave and fallback
semantics. Nested work is bounded at the GPU boundary. All 21 new materials
have reference image goldens; a coverage test requires every new expression
constructor to appear in an example. `docs/PHASE-4.md` records verification.

**CI/Pages follow-up** (2026-10-07): consolidate builds and deployment into one
CI dependency graph, run both GPU suites in parallel, reuse tested CLI/frontend
artifacts, cache gallery images by render inputs, and load the editor once per
isolated browser test. Keep the full numerical/image and 27-workflow static-site
coverage. See `docs/CI-PIPELINE.md`; measure cold and warm job timings against the
recorded pre-change runs.

### 4.4 SDF primitives — complete
- SDF shapes as scalar fields (sphere, box, torus, cylinder, plane), shared
  with the Phase 2 geometry code.
- Combine them with union, intersection and difference, in both hard and
  smooth versions.
- Contours, bands and outlines produced by passing an SDF through a ramp.

Implemented shared CPU/GPU distance kernels and typed editor nodes, including
hard/smooth union, intersection and difference. Added 13 examples with signed
contours, paired hard/soft joins, noise weathering and twisted SDFs. Added
analytical tests, shared GPU fixtures and intentional new example goldens;
existing image goldens are unchanged.

### 4.5 Worley / cellular noise — complete
- F1, F2 and F2−F1 outputs.
- Distance metrics: Euclidean, Manhattan, Chebyshev.
- Jitter amount, and a seed.

Implemented deterministic CPU/WebGL integer hashing with full unsigned 32-bit
seeds, clamped jitter, 2D/extruded and 3D modes, all three metrics and outputs.
Scalar nodes use existing transforms, ramps and generic fractals. Search bounds
and cell-box pruning retain exact F1/F2 with conservative browser work limits.
Examples compare the metrics, F2/gap, regular grids, volumetric fractures and
cellular fractals.

### 4.6 Voronoi — complete
- Cell identity used as a field: a random value or colour per cell, and the
  distance to the cell edge.
- This adds fields that are not plain scalars (cell IDs, per-cell random
  values), which tests the type design from 4.1.

Implemented scalar cell values and true Euclidean bisector edge distance,
plus vector cell identity and seeded RGB projections. Matching configurations
select the same cells; identity feeds existing vector operations/warps without
being collapsed into a scalar. Added mosaics, terrazzo, warped grout, volumetric
opal and 3D grouted marble; 28 new examples across 4.4–4.6, 116 total. Visually
checked all additions in XY and on the cube, refining subtraction examples to
show the cut clearly. Shared analytical/exhaustive-search tests, numerical GPU
fixtures, intentional new goldens and headless editor checks pass. The client
remains static and uses the existing Pages workflow. See `docs/PHASE-4.md` and
`bench/phase4/fields-cells-validation.json` for the guide and validation evidence.

### 4.7 Field combinators, masks and blend modes — complete
- Arithmetic on fields: add, multiply, min, max, remap, threshold.
- Masks: blend between two textures using a scalar field.
- Blend modes beyond normal layering: multiply, screen, overlay, and so on.

Arithmetic and masks are complete from 4.1. Added nine colour blend modes with
source-over alpha and opacity, eleven examples, analytic alpha/formula tests and
shared GPU fixtures. Translucent exports now encode RGBA directly as lossless
PNG, avoiding Canvas2D premultiplication rounding. Existing layer behaviour and
pre-existing image goldens are unchanged.

### 4.8 Reaction–diffusion — complete
- A Gray–Scott simulation on a 3D voxel grid, then sampled with trilinear
  interpolation.
- Architecturally new: this is a precomputed simulation, not a field
  evaluated point by point. It needs caching, explicit resolution and
  iteration-count parameters, and deterministic results so that golden tests
  work. Decide how to compute and cache the simulation entirely in the browser
  while keeping it aligned with the Haskell reference.

Implemented deterministic Float32 Gray–Scott volumes, periodic trilinear U/V
sampling, worker-based browser preparation, bounded shared caches and GPU volume
reuse. The editor handles cancellation, undo, context restoration and export.
Eight examples cover all initial states and both concentrations, including
combinations with warps, fractals, SDF masks, cellular fields and blend modes.
Shared fixtures compare every CPU/browser voxel; headless browser checks retain
existing numerical and image tolerances. Static Pages deployment is preserved.

### 4.9 Phase 4 wrap-up — complete

#### Texture library tidy-up — complete (2026-10-07)

Jules approved the [full 135-texture review](docs/reviews/2026-10-07-texture-library-review.md).
Work through these checkpoints in order, retaining controlled comparison fixtures:

- [x] Correct names/descriptions and introduce the seven shared browsing categories.
- [x] Add capability tags, study roles, family grouping, teaching hints and representative slice previews.
- [x] Repair weak visual showcases, especially clipping, reaction morphology and faint ripple crests; intentionally update affected goldens.
- [x] Fill fundamental/comparison gaps and add a small set of genuinely new visual subjects.
- [x] Verify the complete reference/frontend/browser libraries and document the resulting guide.

The library now contains 175 examples, seven categories and 22 families. See
[the guide](docs/TEXTURE-LIBRARY.md) for coverage and validation. All original IDs
remain available; eleven image updates and forty additions are intentional.

- [x] JSON schema settled at its next version, with migrations from every earlier
  version.
- [x] Editor widgets and browser shaders cover every primitive. Client and Haskell
  migrations agree, and both renderers pass conformance checks. Expand the
  example library to show the new primitives; retain static Pages deployment.
- [x] Write up the design in `DESIGN.md`.

### 4.10 Structured texture compositions — complete (2026-10-07)

Implement the two current-primitive batches approved after the
[coverage audit response](docs/reviews/2026-10-07-coverage-audit-response.md).

- [x] Batch 1: connected Truchet paths, plain weave, twill, overlapping fish scales,
  leopard rosettes, bamboo, travertine, hierarchical crackle, combed marbled paper,
  variable-radius halftone, digital camouflage and scute growth lines.
- [x] Batch 2: ray flecks, multi-eye burl, sparse spotted coat, octagon-and-dot,
  honeycomb, knitted loops, multicoat chips, boundary wear, sea foam, salami,
  porous volume, Greek key, peacock eye, fixed herringbone, finite stain trails
  and Bayer coverage. Select one subject per optional group, rather than palette variants.
- [x] Keep all existing example IDs and images; add intentional reference goldens,
  descriptive capability guides, related families and sampling-contract tests.
- [x] Verify CPU/frontend/GPU rendering and static Pages output for all 203 examples.

The following milestones extend scalar, vector and colour sampling only. The
project remains RGB-valued 3D textures: surface normals, lighting, reflectance,
geometry displacement and mesh-dependent inputs are outside this expansion.
Each milestone must include typed JSON/editor support, migration where needed,
resource accounting, deterministic CPU/browser agreement and focused examples.
Existing goldens must stay unchanged unless an intentional correction is agreed.

### 4.11 Scalar mathematics and coordinates (audit N1) — complete

- [x] Add sin/cos, absolute value, floor/fract, clamp, safe division, power and
  scalar interpolation. Define angle units, negative inputs, zero denominators,
  invalid powers and non-finite handling consistently on CPU and GPU.
- [x] Add a true atan2/azimuth coordinate; retain the existing angular cosine fan
  unchanged. Consider vector component accessors alongside this typed interface.
- [x] Simplify the explicit quantisation, periodic crossing and comb recipes;
  compare rendered results and add examples that justify the new operators.

Implemented additive typed V5 nodes, analytical/singularity fixtures and three
new compositions; simplified existing derived maths without changing their images.
See [decisions and validation](docs/PHASE-4-EXTENSIONS.md).

### 4.12 Native periodic noise (audit N13)

- [ ] Provide gradient noise with explicit integer periods on all three axes.
- [ ] Specify periodic fractal sampling: octave frequencies and orientations must
  preserve the requested world-space period. Existing rotated octaves cannot be
  assumed to do so. Document period-preserving warp composition.
- [ ] Test values and first derivatives at every axis seam, negative coordinates
  and multiple periods; compare native cost with the existing crossfade tile.

### 4.13 Bounded seeded scatter (audit N3)

- [ ] Scatter bounded colour/mask motifs with deterministic placement, density,
  rotation and scale variation, sampled in local motif coordinates.
- [ ] Define support bounds, neighbourhood search, overlap ownership and seed
  semantics before implementation. Enforce CPU/GPU work limits for nested motifs.
- [ ] Demonstrate independent marks, variable inclusion size and overlapping
  motifs. Keep scattering in texture space; no mesh or surface sampling.

### 4.14 Focused periodic layouts (audit N2)

- [ ] Introduce shared layout configurations with typed local coordinates,
  identity/random value and boundary-distance projections.
- [ ] Start with grid/running bond, regular hexagons and herringbone; establish
  ownership, negative-coordinate and boundary rules before more elaborate layouts.
- [ ] Replace the fixed gallery prototypes with simpler equivalent documents and
  compare joins and grout. Defer arbitrary parquet/Versailles and every bond variant.

### 4.15 Field-driven orientation (audit N5)

- [ ] Rotate local sampling around a fixed axis by a scalar angle field, with
  explicit pivot and angle units. Prototype directional motif/stripe compositions.
- [ ] Account for evaluating the angle field and retain transform-order semantics.
  Consider general 3D frames only after the fixed-axis feature proves useful.
  Gabor noise and geometry-derived directions are separate future proposals.

### 4.16 One bounded branching field (audit N7)

- [ ] Choose one deterministic growth model and expose distance/density to a
  bounded branching network, including hierarchy and taper, for colour mapping.
- [ ] Design a bounded segment/volume representation and reuse worker preparation,
  caching and cancellation where appropriate. Avoid unbounded per-pixel traversal.
- [ ] Demonstrate convincing vein/root/network structure before expanding to other
  growth models; do not promise DLA, L-systems and vascular simulation together.

### 4.17 Selective reaction–diffusion extensions (audit N10)

- [ ] Add scalar-field initial seeding, then spatially varying feed/kill fields.
- [ ] Define sampling, periodicity, cache identity, worker dependencies, stability
  and memory limits for arbitrary field inputs; preserve existing simulations.
- [ ] Evaluate a higher-resolution 2D simulation extruded through Z as a subsequent
  subset. Defer anisotropic diffusion and time-as-coordinate shell models.

### 4.18 Conditional gradient/curl fields (nonlighting part of audit N4)

- [ ] First prototype a compelling RGB flow composition and measure its cost.
  Proceed only if it adds enough beyond existing warps within browser budgets.
- [ ] If justified, expose gradient/curl for domain displacement, with a specified
  difference step, units and zero-gradient handling. Count the repeated source
  evaluations (typically four forward or six central samples in 3D).
- [ ] Treat gradient-normalised isovalue distance as a local approximation, not an
  exact SDF. No normal maps, relief shading or lighting operators in this milestone.

Deferred rather than scheduled: general nonlocal filters (N6), historical
fracture simulation (N8), Penrose tiling (N12), surface attributes (N9), and lighting
response (N11). The last two and N4's relief/shading branch are explicitly outside
this project's present RGB scope. Keep this sequence; reassess conditional 4.18
against the gallery and performance evidence available after 4.17.

---

## Phase 5: Portable procedural texture compiler and shader libraries (draft)

Revised 2026-10-08 after design discussion. Work through Phase 4 before this
phase. Build an independent Metal backend in the style of the existing
TypeScript-to-WebGL compiler first. Establish agreement on shared numerical
fixtures and the entire then-current golden texture library, with measured
fast-maths deviations and explicit acceptance criteria, before refactoring around
an IR. The WebGL and Metal generators remain independent until then; do not
introduce an interim dialect layer. Compiler technology remains an open decision;
Haskell code generation with a small Swift execution harness is the current
preference, not a settled implementation choice.

**Goal:** ship the texture library for use inside other people's GPU projects,
starting with Metal. Other platform backends remain future possibilities. Preserve
continuous 3D scalar, vector and colour sampling; application geometry, lighting
and material response remain the caller's concern.

Build-time export is the first product; runtime compilation is a later decision:

- **Build-time export:** texture JSON becomes generated shader source, exposed
  parameter definitions and required resource assets/metadata. The application
  compiles the source alongside its own shaders. Texture structure is fixed at
  build time; explicitly exposed parameters can change at runtime.
- **Runtime compilation (if selected):** a host library loads texture JSON,
  generates shader source and prepares pipelines/resources. New structures can be loaded without
  rebuilding the application. A Swift-facing Metal library is a later milestone,
  separate from the small Swift harness used to validate the first backend.

**Release gates:** each is independently useful and includes clean consumer
builds, documented capabilities, numerical/image conformance and packaging.

1. **Metal source export:** independent backend and consuming application,
   exposed parameters and existing prepared-field assets. No IR prerequisite.
2. **IR-based export compiler and Metal host integration:** proven refactor,
   parameter/resource lifecycles and an independent consuming application.
   Arbitrary runtime JSON loading is conditional on a demonstrated need.

Browser compiler migration and additional platform backends are optional future
work, not release gates. The editor retains its direct TypeScript/WebGL path;
shared fixtures and goldens keep editor and exported textures aligned.

Metal tests run locally on Apple hardware for now. CI continues checking the
Haskell and JavaScript/WebGL paths; hosted Metal execution or a Metal CI probe is
not a prerequisite. Record local hardware/toolchain details and validation results
at each gate. Finish Phase 4 before starting Phase 5; name the initial feature
baseline, and require matching Metal support/fixtures for subsequent new primitives
once the backend exists. Selective field baking is deferred to Later.

### 5.1 Sampling, colour and parameter contracts

- [ ] Default Metal compilation to fast maths and measure its effect rather than
  requiring safe maths up front. Run representative numerical/image comparisons
  and animated views; record error distributions, visible differences and timings.
  Use safe-maths builds as diagnostic comparisons. Add targeted precise operations
  only where evidence justifies them; do not assume they constrain all surrounding
  arithmetic. Document consumer compilation settings and the measured guarantees.
  Keep reference goldens unchanged; assess backend deviations rather than rewriting
  references to match fast maths. Retain exact integer/hash and categorical checks.
- [ ] Define coordinates, scalar/vector/colour outputs, colour encoding, alpha,
  prepared-field sampling and numerical tolerances. Keep geometry, preview
  cameras, application entry points and lighting separate from sampling functions.
- [ ] Preserve existing document semantics for the initial Metal backend:
  sRGB colour literals, alpha-weighted OKLab ramp interpolation, current straight
  RGB/alpha mask mixing and existing layering/blend behaviour. Specify gamut
  clamping, hard ramp stops, transparent endpoints and alpha representation.
- [ ] **Proposed default:** retain OKLab for artistic ramps. Treat ramp
  interpolation and exported output encoding as separate choices; provide an
  explicitly documented linear-RGB output adapter for consumers that need it.
  Decide whether to add explicit encoded-sRGB/linear-RGB ramp options later.
  Do not change existing internal mixing semantics while porting the renderer.
- [ ] Classify exposed parameters by their update requirements: sampling data
  updates, prepared-resource regeneration, or structural/specialisation changes
  requiring shader/pipeline regeneration. Numeric type alone does not determine
  the class. Describe dependencies and which edits invalidate prepared resources.
- [ ] Define stable public parameter names, types, defaults and constraints, and
  decide where declarations live. Prefer document-owned declarations; include
  schema/versioning, migrations where needed, round-trip preservation and editor
  UI in gate 1. Document paths remain diagnostics rather than the permanent API.
- [ ] Hand-port three representative textures into a tiny Swift consumer before
  fixing the sampling API: a procedural field, cellular composition and prepared
  reaction volume. Use them together in the application's shader to establish the
  context argument/resource bindings, ownership and namespacing.
- [ ] Document the current portable baseline: 200 nodes, nesting depth 64, 128
  stops per ramp, four distinct reaction volumes and the 4096 expanded sampling-work
  budget, with precise counting rules. These are current implementation limits,
  not presumed Metal hardware limits; record any measured backend differences.
- [ ] **Decision:** initial compiler technology, minimum Metal OS/API/GPU support,
  feature/resource limits and harness interface. Evaluate Haskell generation plus
  a small Swift runner first; defer the shared compiler/distribution choice until
  both direct backends have demonstrated correctness. The editor does not need
  to embed or adopt the new compiler.

### 5.2 Independent Metal backend and conformance harness

- [ ] Generate Metal sampling functions directly from the resolved texture model,
  using the existing TypeScript-to-WebGL implementation as a behavioural guide.
  Keep reusable field functions separate from test fragment/compute wrappers.
- [ ] Build a small Swift harness for device creation, shader compilation,
  parameter/resource uploads, execution, numerical readback and image output.
  Report compiler errors and unsupported features with useful document locations.
- [ ] Start with a vertical slice: transformed noise, an alpha ramp, matching
  cellular projections and a prepared reaction volume. Exercise multiple textures
  in one shader and parameter updates, then cover every supported primitive.
- [ ] Reuse shared fixtures for integer hashes, negative coordinates, transforms,
  ramps/alpha, noise, cellular boundaries and prepared-volume interpolation.
  Compare against the independent Haskell evaluator and existing WebGL backend.
- [ ] Compare the entire then-current golden library and numerical fixtures
  before the IR refactor. Investigate deviations, establish evidence-based Metal
  acceptance criteria for fast maths, and record compile and GPU sampling costs.
  Small floating-point/image differences may be acceptable; wrong cell ownership,
  hash results, broken boundaries or non-finite results need investigation.
  Existing reference goldens change only for an explicitly agreed correction.
- [ ] Run the harness locally on real Apple GPU hardware and document a repeatable
  command and reports. Keep focused checks fast and GPU/image suites separate.
  Metal CI is deferred; CI retains Haskell and JavaScript/WebGL checks.
- [ ] Match existing prepared-volume semantics with explicit texel reads and
  manual trilinear interpolation, including voxel-centre offsets and periodic
  wrapping. Do not depend on hardware float32 filtering for initial conformance.

### 5.3 Metal build-time export — release gate 1

- [ ] Export fixed JSON documents as namespaced Metal functions, reusable
  primitive source/headers, exposed parameter definitions and explicit host
  layouts/alignment. Carry over slot-based parameter packing into a
  `constant float4*` buffer, with names mapped to slots/components in the manifest.
  Emit unexposed fixed values as literals for platform constant folding; values
  derived from exposed parameters must remain updateable. Document bindings for
  noise tables, ramps and prepared fields, and preserve FP32 packing semantics.
- [ ] Supply existing prepared-field assets, including reaction concentrations,
  with formats and sampling metadata. Demonstrate consuming exported assets
  without running their preparation algorithms inside the application.
  Start with a concrete binding manifest; defer general dependency planning to 5.6.
- [ ] Validate an independent application that compiles exports alongside its own
  shaders, samples scalar/vector/colour fields and updates exposed sampling
  parameters without recompilation. Exercise resource-changing edits separately.
- [ ] Package usable source exports, integration samples and installation/licence
  guidance at this gate. Optional image/volume output kernels evaluate the defined
  fields; they do not introduce implicit approximation of procedural subfields.

### 5.4 Shared typed intermediate representation

- [ ] After both direct backends agree, design an IR informed by their working
  implementations. Resolve the export compiler language, host bindings
  and distribution strategy at this point; avoid duplicated optimisation semantics.
- [ ] Separate JSON validation/migration and reference resolution, typed field
  graph, lowering and backend source emission. Preserve domains, ramps, fractals,
  coordinate inputs, prepared-field dependencies and public parameter classes.
- [ ] Introduce expression/block IR only where needed for operations, loops,
  temporaries, buffer reads and texture samples. Optimise the representation,
  rather than manipulating emitted source text.
- [ ] Emit Metal and a test-only GLSL target from the new compiler. Execute its
  GLSL output through the existing headless Linux numerical/image harness so IR
  and optimiser checks can run in CI without Metal. Keep the editor's generator
  independent, and compare all paths with shared fixtures and reference goldens.
  Retain direct Metal generation until refactor equivalence is demonstrated;
  keep the Haskell evaluator independent of optimisation as an oracle.
- [ ] **Decision:** IR boundaries, serialisation/versioning and any intermediate
  artifact shared with host bindings, including Slang as a candidate emission
  target. Include source locations and resource limits.

### 5.5 Measured optimisation and correctness

- [ ] Add constant folding, dead-code removal, redundant-transform simplification,
  ramp specialisation and shared computations only where measured and validated.
  Common-subexpression identity includes coordinates and relevant resources;
  sampling the same field at two warped positions is not one evaluation.
- [ ] Preserve exposed parameters and preparation dependencies. Measure rewrites
  under default fast maths against unoptimised generation and diagnostic safe
  builds; document numerical effects separately from compiler-option effects.
  Classify specialisation and its pipeline invalidation explicitly.
- [ ] Compare optimised/unoptimised Metal and GLSL with the Haskell reference and
  shared fixtures. Measure generation/compilation time, source size and GPU cost;
  leave instruction selection and scheduling to platform compilers.
- [ ] Keep sampled-field baking outside semantics-preserving optimisation passes;
  its approximation contract and experiments are deferred to Later.

### 5.6 Resource planning and Metal host integration — release gate 2

- [ ] Introduce general resource planning as a compiler output alongside sampling
  code and the parameter/binding manifest. Describe preparation inputs and
  dependencies, resource identities, formats, bounds, sampling rules and budgets.
- [ ] Make invalidation dependency-driven: sampling edits update data; preparation
  edits rebuild affected resources; structural/specialisation edits rebuild code
  or pipelines. Reuse preparation results where their full dependencies match.
- [ ] Expose exported-function parameter updates, resource preparation and
  diagnostics through a Swift-facing API with explicit ownership/lifetimes.
  Support caching, asynchronous preparation, cancellation and memory limits.
- [ ] **Decision:** do we need arbitrary runtime JSON loading? If so, decide how
  the compiler is embedded before promising that API. A Haskell build-time
  generator and Swift harness do not by themselves solve in-app JSON compilation.
  If it can wait, ship build-time exports and their host support first.
- [ ] **Decision:** source composition versus dynamic shader linking, host API and
  cache policy, CPU versus GPU preparation. Validate numerical behaviour rather
  than assuming preparation implementations are interchangeable.
- [ ] Validate the IR-based exporter and host package in an independent app, with
  parameter/resource updates and local Metal conformance/performance evidence.
  If runtime JSON loading is selected, also demonstrate loading a new structure
  without rebuilding. Publish only the capabilities actually implemented.

### 5.7 Packaging and release maintenance

- [ ] At each release gate, publish versioned artifacts, API/installation docs,
  licences, capability limits and complete independent consuming examples.
- [ ] Document build-time/runtime use, output colour encoding/alpha, parameter
  classes, resource bindings/preparation, procedural sampling and its fast-maths
  error contract, compatibility policies and benchmark/conformance evidence. Update `DESIGN.md`
  and the library guide at the corresponding gates.
- [ ] **Decision:** source exports, ordinary compiled libraries for ready-made
  kernels, Metal dynamic libraries for linked functions, and binary host frameworks
  are distinct packaging choices. An ordinary `.metallib` alone does not provide
  arbitrary helper calls from a user's shader; choose additional products only
  when a consuming example demonstrates their need.
- [ ] Maintain Haskell/JavaScript checks and test-only generated GLSL in CI,
  with separate local Metal GPU/image suites. Benchmark compile/startup time,
  parameter updates, repeated sampling, output baking and resource preparation time/memory as each capability lands.

---

## Later (not yet planned)

- Alternative browser renderers (WebGPU/WGSL, JS/WASM) if compatibility or
  future workloads justify them, using the same conformance harness.
- Volumetric rendering (density fields, clouds, hypertexture) as a side-quest.
- Comparing implementations with the benchmark harness (Haskell, browser, and
  others).
- Optional migration of the editor to IR-generated GLSL, only if it offers a
  demonstrated benefit while preserving editor behaviour and static deployment.
- Selective scalar/vector field baking: benchmark preparation/reuse costs, memory,
  filtering and error under zoom/warps before scheduling support. Keep bounds,
  resolution, outside-domain/periodicity rules and approximation explicit; never
  interpolate categorical cell identities. Existing prepared reaction volumes
  remain part of Phase 5 and are distinct from arbitrary procedural-field baking.
- Low-priority additional export targets: consider one HLSL emitter with DXC
  outputs for both Vulkan/SPIR-V and DirectX/DXIL, with separate integration and
  execution checks. Neither is required to complete the Metal-focused phase.
- Keep future PS5 AGC and Nintendo NVN/NVN2 integrations possible without adding
  platform-specific assumptions to the IR. Revisit when a consumer and platform
  tools/hardware justify the work; neither is a current milestone or release gate.
- Hosted/self-hosted Metal CI if it later becomes useful; local Metal testing is
  the agreed initial workflow.
