# Phase 3: browser rendering decisions and release evidence

## Desktop release — 2026-10-06

The complete editor now uses the WebGL2 renderer without a Haskell server.
Chrome, Firefox and Safari hardware paths pass all 123 sample cases and 106
reference PNGs, plus editor workflow smoke checks. The Pages site publishes
the editor at the root and the build-time Haskell gallery under `gallery/`.
See [release measurements](../../bench/PHASE-3-RESULTS.md) for raw evidence and
support policy. The user reports silky smooth interaction on a physical iPhone 16 Pro in
Mobile Safari; its nine-case benchmark records default 512² medians of
1/4/10 ms and cold first renders below half a second. Android remains unverified.
Phone-sized layout tests alone do not establish mobile GPU performance. Firefox's worst cold
first render is 1.22 seconds, exceeding the provisional one-second goal.

The release at [GitHub Pages](https://drquicksilver.github.io/procedural-textures/)
passed all 22 browser workflow tests against the public URL after deployment,
as well as the complete pre-deployment Haskell/frontend/GPU gates. Release
commit `21d1b4d` and [Pages run 37522055629](https://github.com/drquicksilver/procedural-textures/actions/runs/37522055629)
record the deployed artifact. All four jobs in
[CI run 37522055814](https://github.com/drquicksilver/procedural-textures/actions/runs/37522055814)
passed. No golden image or comparison tolerance changed.

## Historical 3.1 spike status — 2026-10-06

The desktop spike supports proceeding with WebGL2. It renders Checker, Marble
and Cumulus on the bitten cube, plus XY/XZ/YZ slices, through an independent
renderer used by both a standalone preview and a headless CLI harness.
The later nine-case iPhone 16 Pro benchmark closes the phone measurement
requirement; see the release measurements for final cross-device evidence.
The full editor, other shapes, library assets and document migrations remain
later Phase 3 work.

## Architecture

`frontend/src/gpu/compiler.ts` compiles the texture tree into named GLSL ES
3.00 functions. It supports flat, linear, circular, Perlin, fbm, turbulence,
checker and layer nodes, concrete ramps and document-local named ramps. This
spike consumes current-version valid documents; it is not the complete document
validator or built-in ramp resolver planned for 3.4.

Numerical values live in an RGBA32F data texture. Ramp stops have their original
sRGB and precomputed OKLab values; octave matrices are also prepared outside
the per-pixel path. Fixed-capacity octave storage makes octave-count changes
parameter updates. The spike caps octaves at 32, stops at 128 and nodes at 200,
and rejects non-finite/float-overflow parameters. Structural source changes
compile a new program; a renderer keeps up to eight programs with LRU eviction.
The compiler is tested for stable source under numerical edits, and the GPU
checks verify changed pixels without compilation and compilation after a new
branch is introduced.

A fullscreen triangle runs the existing bounded perspective sphere tracer,
then evaluates the material only at the surface hit. Object-space coordinates,
finite-difference normals, lighting and scene compositing match `Scene.hs`.
Slices use the same material function. Rendering targets an explicit RGBA8
framebuffer and blits to the preview canvas; export reads this framebuffer and
flips its rows. Explicit ties-to-even byte quantisation matches `Render.toByte`
and fixes the checker slice's initial half-byte rounding discrepancy.

This is a functional spike: parameter packing and framebuffer allocation still
happen each render. Resource reuse, shared warp evaluation, context restoration,
complexity policies and editor scheduling need attention in 3.3–3.6.

## Development and comparisons

For a visible preview, run `npm --prefix frontend run dev` and open
`http://localhost:5173/spike.html`. Material/view/size controls, slice position,
drag-to-orbit and scroll-to-zoom all use the independent renderer. Its UI timing
is submission time, not completed GPU time.

The CLI starts a minimal Vite server and one headless Chrome process/page/context
for all selected cases. It loads no editor and calls no rendering API:

```
npm --prefix frontend run gpu:spike
npm --prefix frontend run gpu:spike -- --example marble --view scene --size 512
stack build
npm --prefix frontend run gpu:spike -- --compare --check
```

`CHROME` can select the Chrome/Chromium executable; `--out` selects an output
directory and `--repeats` sets the warm sample count. Outputs default to
`out/gpu-spike/`: raw-framebuffer PNGs and a measurement JSON. `--check` also
checks parameter/program reuse and the standalone preview controls, and saves a
preview screenshot for inspection. Browser tests remain a secondary suite.
A watch command, shader linting and the broader conformance harness are 3.2 work.

`--compare` uses installed Haskell CLI tools, existing goldens at their native
sizes (texture slices 128²; scene goldens 96²), and freshly rendered references
in the output directory for other views/sizes. It changes no golden files.
All 12 material/view cases at 128² pass mean ≤ 0.0001 and max ≤ 0.008.
Checker's three slice images are pixel-identical; the largest scene difference
is 0.006792. Named-ramp and source-stability checks also pass in unit tests.

At 512² the bitten-cube scene errors are:

| Material | Mean RGBA error | Maximum RGBA error |
|---|---:|---:|
| Checker | 0.000006 | 0.006792 |
| Marble | 0.000018 | 0.011765 |
| Cumulus | 0.000014 | 0.006792 |

Marble has one pixel above 0.008: at (296,227), Haskell gives
[145,147,147,255] and the shader gives [147,149,148,255]. The focused investigation below isolates this to a float-sensitive raymarch
stopping decision, rather than a material evaluator discrepancy.
The default comparison correctly fails this case. For the exploratory 512² run
only, an explicit maximum of 0.016 was used, retaining the original mean limit:

```
npm --prefix frontend run gpu:spike -- --size 512 --view scene --repeats 20 --compare --max-threshold 0.016
```

This is measured spike evidence, not a settled cross-device tolerance policy.
Keep the stricter 128² checks and use these findings when designing the broader
conformance checks in 3.2. Reference images and their acceptance policy are unchanged.

## Desktop timing evidence

Apple M1 Pro, Chrome 153.0.8010.36, ANGLE Metal hardware backend. A synchronous
one-pixel framebuffer readback forces completion after each draw; `gl.finish()`
alone gave misleadingly low times in the initial harness. Those initial times
are not the recorded completed-render baseline. Timings include JS material
preparation, parameter upload, drawing, presentation and the completion readback;
they exclude browser startup, full-image readback and PNG encoding.

Twenty warm samples, 512² bitten cube, default camera:

| Material | Median completed render | Full readback | PNG encoding |
|---|---:|---:|---:|
| Checker | 0.8 ms | 1.1 ms | 4.0 ms |
| Marble | 3.0 ms | 0.8 ms | 5.0 ms |
| Cumulus | 8.1 ms | 0.7 ms | 3.7 ms |

Full readback and encoding columns are single observations, not stable latency
estimates. The matching Haskell scene-plus-PNG baseline is 21.4/73.9/95.4 ms
respectively (`bench/PHASE-2-RESULTS.md`); it uses a different timing harness.
These observations support the GPU direction, not precise speedup claims.
Raw samples and image reports are in `bench/phase3-spike/desktop-512.json`;
`desktop-128.json` records the conformance/cache/preview check run. The actual
96² scene preview medians are 0.6/2.9/4.7 ms; all three pass the original
comparison limits (`desktop-96.json`).

First renders vary with browser/driver caches. Earlier new-shader runs took
roughly 0.3–0.6 seconds for noise-heavy materials; the recorded 512² run took
17–26 ms with driver caches potentially warm. Recorded synchronous program
compile/link times exclude any deferred driver work captured by first-render
completion. Treat cold compilation separately from warm interaction.

Provisional desktop targets: warm preview ≤ 10 ms, warm 512² refinement ≤ 16 ms,
no program compilation on parameter/camera edits, and a first render within
one second for these spike cases. The measured 96² editor preview size meets the preview
target. These are spike targets, not guarantees for all materials or devices.
Measure cold compilation and mobile hardware before
settling cross-device targets and declaring 3.1 complete.

## Verification

`stack build`, `stack test`, frontend unit tests/production build, the existing
16 editor browser tests, raw image comparisons, GPU cache/parameter checks and
standalone preview controls are the checks for this change. No golden images or
shared vectors were accepted/regenerated.


## Focused Marble pixel investigation — 2026-10-06

A bounded, approximately twelve-minute investigation confirms the FP32
explanation, specifically a branch at the sphere tracer's hit threshold. At
pixel (296,227) in the 512² default bitten-cube view, the fourth step (index 4)
has these signed distances:

| Implementation | Distance | `abs(d) < 0.0005` |
|---|---:|---|
| Haskell Double | 0.0005000243070739097 | false |
| GPU FP32 | 0.0004999885568395257 | true |

GPU stops at step 4; Haskell advances once more and stops at step 5, where its
distance is 0.0000548661227200431. The final points differ by about 0.000450
object units. This tiny change in sample position alters Marble's warped narrow
veins enough to produce the two-byte red/green and one-byte blue difference.
The pre-branch distance discrepancy is only about 3.6e-8, but the stopping rule
turns it into a discrete change of trace path.

The decisive control is evaluating the Haskell material at the GPU's actual
hit point. Haskell then produces **[147,149,148,255]**, exactly the GPU pixel,
instead of **[145,147,147,255]** at its own later hit. At a fixed hit point, GPU
and Haskell material RGB differ by at most about 5.2e-6; the remaining lighting
and shaded-colour differences do not change the rounded bytes. This separates
sample-location sensitivity from accumulated error inside the texture evaluator.

Temporary Haskell probes called `cameraRay`, `traceRay`, `distance`, `normalAt`
and `textureToField` directly, and continued the trace through the next step.
Temporary GPU probes used the same generated GLSL and parameters with RGBA32F
framebuffers. A combined four-output shader captured shaded colour, the hit
point/step, the step-4 distance and the material colour from the same trace.
Separate simplified shaders sometimes took a different path at this threshold,
which is why the combined capture matters. Fixed-coordinate samples then
isolated material evaluation. Recorded numeric evidence is in
`bench/phase3-spike/marble-pixel.json`; the production renderer was unchanged.

**Decision:** no pixel-specific fix, golden regeneration or tolerance change.
The discrepancy is an expected sensitivity of FP32 sphere tracing compared with
a Double reference. In 3.2, keep material-at-fixed-coordinate checks separate
from scene comparisons, and account explicitly for geometry hit tolerance and
high-gradient materials when settling cross-device scene tolerances. There is
no reason from this pixel to change the selected WebGL2 architecture.

## 3.2 conformance tooling

`npm --prefix frontend run gpu:test` compares unquantised RGBA32F output at
fixed coordinates against `test-vectors/gpu-materials.json`. Haskell generates
these vectors under the existing accept policy. Cases cover the ramp edge
cases in every mode, premultiplied alpha, lattice neighbours and negative
coordinates, all fbm styles, nested/shared warps and layers. Raw Perlin samples
are checked separately. Material channels use an absolute tolerance of 5e-5;
raw noise uses 1e-5. Composed example tolerances are described below. Geometry vectors are also exported from Haskell for 3.3,
including shape definitions, so the chess models retain one source of truth.

```
npm --prefix frontend run gpu:test -- --case marble
npm --prefix frontend run gpu:test -- --self-test
npm --prefix frontend run gpu:watch -- --case marble
npm --prefix frontend run gpu:test -- --case marble --mutate
```

The final command deliberately substitutes a wrong shader and must exit with a
failure. `--self-test` checks that a wrong shader is distinguishable and that an
invalid shader reports numbered source including its texture-node path. Watch
mode keeps one browser/page/context, invalidates the Vite module graph and
replaces renderer resources to pick up changed shader dependencies. It watches
GPU source, shared fixtures, examples and ramps. An actual shader mutation and
restoration were checked to produce FAIL then PASS without relaunching Chrome.

Both the image and sample commands use `gpu-session.mjs`. CI installs the
Chrome revision declared by the lockfile-pinned Puppeteer package through
`npm --prefix frontend run gpu:browser`; it verifies that exact version and the
SwiftShader backend. `GPU_BACKEND=swiftshader` selects and verifies software
rendering locally as well. Hardware timings remain separate from software
conformance results. Full GPU checks remain outside the fast unit suite and
run in a separate CI job. No optional GLSL linter is required.

Software conformance exposed cancellation near transparent ramp endpoints.
Normalising the interpolated alpha weight before mixing OKLab channels avoids
dividing tiny, cancellation-damaged premultiplied components. This is
mathematically equivalent to the Haskell interpolation and preserves the
zero-alpha fallback. The targeted case improved from about 3.86e-4 to 8.6e-8 on
SwiftShader. The largest software sample error in this initial set is about
2.6e-5 (sinusoidal easing); the same 5e-5 limit applies to both backends. This
is an evaluator conformance limit, not the scene image tolerance.

## 3.3 complete shader coverage

The standalone preview now loads all 67 example documents and offers all 13
shapes. The material compiler covers every current constructor, named and
built-in ramps, all noise styles, nested warps, three-dimensional checkers and
alpha layers. Geometry compilation covers every SDF and profile constructor.
`test-vectors/gpu-geometry.json` includes the actual Haskell shape trees as well
as independently evaluated distances; there is no separately maintained set of
browser chess models. Moving these models into lean versioned metadata assets
belongs to 3.4.

Perlin permutation and gradient tables occupy the first two rows of the data
texture. This avoids large dynamic constant-array selection trees in software
shader compilers. Octave and ramp loops use their validated bounds directly.
Slices and scenes have separately cached pipelines so a slice need not compile
the unused raymarching path. Scalar, colour, ramp-mode, octave and camera edits
still update data without recompilation; structural edits select another
program in the eight-entry LRU cache. Linear projection coefficients, camera
axes and fixed geometry rotations are also computed outside the pixel path in
JavaScript Double arithmetic. This removes software GLSL trig approximations
from the camera: sphere/Malachite max error improved from 0.114 to 0.003922,
with mean about 1.06e-5. Precomputing the linear gradient fixes the exact
wrapped-stripe seam without adding a material-specific exception.

Layer branches share a warp sample in their current coordinate domain. The
compiler passes the sample and its configuration explicitly to child functions;
exact host configuration identities allow numerical edits to break or
re-establish sharing without changing shader structure. Distinct Double
configurations remain distinct even if their FP32 values round alike. A turbulence child starts a fresh
domain. This retains opaque-layer skipping and avoids mutable fragment arrays,
which caused excessive compilation work on SwiftShader.

Texture trees are limited to 200 nodes, depth 64, 32 octaves and 128 stops per
ramp. Geometry is limited to 512 nodes, depth 64 and 128 polygon vertices.
Non-finite/FP32-overflowing parameters, unsupported nodes, missing references,
unknown shapes and excessive framebuffer/data sizes report errors. The renderer
handles context loss, recreates resources after restoration and leaves the
caller's document intact. The preview schedules a fresh render on restoration.
The lifecycle test forces a loss/restoration and checks identical output.

### Numerical policy and coverage

`gpu:test` covers all shipped materials plus targeted constructor/ramp cases,
raw noise and 1,125 distances across all shapes and additional operations.
Primitive material channels retain the 5e-5 limit; composed shipped materials
use 2.5e-4 (under 0.064 of an 8-bit colour step). This accounts for amplification
through sharp ramps and nested warps: hardware Malachite measured about 1.44e-4.
Raw noise and geometry use 1e-5. Points near lattice/hard-stop boundaries use
binary-exact coordinates. Tilted radial tests avoid the undefined angular
direction exactly on their axis; axis-aligned poles and zero axes are tested
explicitly. GLSL's undefined `atan(0,0)` in radial repetition is guarded, and
normalisation matches Haskell's `(0,0,1)` fallback below 1e-12.

```
npm --prefix frontend run gpu:spike -- --goldens --repeats 0
GPU_BACKEND=swiftshader npm --prefix frontend run gpu:test -- --self-test
npm --prefix frontend run gpu:spike -- --example marble --shape knight --view scene --size 96 --compare
```

The golden command compares all 67 XY texture goldens at 128² and all 39 scene
goldens at 96². Both slices and scenes retain the Haskell mean ≤ 0.0001,
max ≤ 0.008 policy, including its treatment of fully transparent
RGB. Camera/projection precomputation removed the apparent need for a looser
scene tolerance: native scene goldens agree within about 0.0068 on both backends.
Raw comparisons remain authoritative; no neighbour matching or edge exemptions
are used. The prior 512² Marble investigation still illustrates why other
resolutions and cameras may cross a raymarch/discontinuous-material boundary;
`--max-threshold` is an explicit experimental override, not the CI policy.

SwiftShader's cosine approximation also produced a 128² Red-green sine mean
error of 0.000123 despite only one-byte differences. Sinusoidal easing now uses
a degree-13 polynomial for sin(pi*(t-0.5)), on its bounded interval. Its analytic
approximation error is below 7e-10 before FP32 rounding. That texture now matches
the reference PNG exactly on software rendering, and the primitive vectors
check its endpoints, interior samples and wrapping modes.

Reference PNGs and the Haskell comparison policy are unchanged. Only new GPU
sample/shape fixtures were deliberately generated with the accept command.
Browser/backend, raw comparisons, shader compilation and readback
measurements are written below `out/`; GPU conformance remains a secondary suite.
Desktop software compilation measurements are not interactive GPU timings.
Phone measurements, other browsers and performance tuning remain in 3.1/3.6;
editor integration and static metadata replacement remain in 3.4/3.5.

## Final editor architecture and hosting (3.4–3.7)

Haskell exports checked-in static metadata for the schema, all 67 examples,
built-in ramps, shape distance trees and historical migration semantics.
`stack test` detects metadata/fixture drift. The browser validates and migrates
v1–v4 documents locally; the shared 279 processing cases check that behavior
against the reference. Haskell remains authoritative for new primitives,
fixtures, CLI/galleries and golden images, without any runtime dependency.

The renderer reuses framebuffer/data allocations, bounds its program cache at
eight entries and keeps the main viewer program resident. One hidden WebGL2
canvas serves the main viewer and subtree thumbnails; rendered frames are copied
to presentation canvases with `drawImage`, without readback or PNG encoding.
A prioritized queue runs one render per animation frame, with export and main
viewer jobs preceding thumbnail work. Thumbnail jobs are deferred and cancelled
when obsolete. The 300-entry thumbnail canvas cache is bounded.

Interaction uses an adaptive 15 ms render budget with 15% headroom and a
64–1024px range. Asynchronous disjoint timer queries measure GPU time where
available; CPU submission time is the less accurate fallback. Cold compilation
is excluded, reductions are immediate, increases damped, and idle refinement
returns to full resolution after 180 ms. This is a render budget, not a promise
that all browser UI work fits in a 15 ms frame. Export requests render the chosen
view at 256/512/1024/2048px, read back once, flip rows and encode PNG. Slices keep
alpha. Context loss preserves documents and restoration re-renders current state.

The Pages artifact uses relative script/asset/navigation paths so the repository
prefix works for direct loads and reloads. `make pages` builds the frontend and
assembles `out/pages/`, including the reference-generated gallery and old HTML
URL redirects. The workflow gates deployment on Haskell and frontend checks,
GPU sample/mutation tests, native-size golden comparisons and browser tests of
the assembled artifact served beneath `/procedural-textures/`. Only static files
are uploaded. No `/api` calls are permitted by browser tests.

The editor library is browser-local storage, scoped to origin/device. Moving
from localhost to Pages requires JSON export/import. The shipped desktop paths
support hardware-accelerated WebGL2; software Chrome is a CI correctness backend.
There is no measured reason yet to maintain a parallel CPU browser renderer.
The physical iPhone 16 Pro smoke check reports smooth interaction and its
hardware benchmark supports the adaptive 15 ms preview budget: close Cumulus
costs 16 ms at 512² and 32 ms at 1024². Its Checker slice matches the Haskell
reference exactly. Other mobile devices have no measured performance guarantee.
A 390px layout has a vertically arranged viewer/tree/inspector with wrapping
controls, so the complete workflow remains accessible on narrow screens.

### Linux software backend

Chrome 154's Linux SwiftShader GPU process segfaults (exit 139) when compiling
or first executing Cumulus, even in a fresh Cumulus-only context. Increasing
the thread stack from 8 to 64 MiB and disabling the GPU watchdog did not fix
it. Native diagnostics showed a process crash, not an image-tolerance failure
or an out-of-memory kill. The Mac ARM SwiftShader path passes the full sample
and mutation/lifecycle suite, as do all three tested desktop hardware browsers.

Linux CI therefore uses ANGLE OpenGL with Mesa llvmpipe under Xvfb, forces
`LIBGL_ALWAYS_SOFTWARE=1`, and verifies the reported backend before tests.
Sample, scene and slice tolerances remain unchanged. `GPU_BACKEND=swiftshader`
stays available for additional checks; ordinary local tests use hardware.
Software-only launches disable the GPU watchdog for slow software JIT; protocol
and job timeouts still bound execution, and forced loss/restoration is tested.
See the upstream [ANGLE debugging guidance](https://android.googlesource.com/platform/external/angle/+/refs/tags/android-15.0.0_r26/doc/DebuggingTips.md)
for that switch's role; it was not the fix for the native Linux crash.

### Constant ramp spans at byte boundaries

Mesa's complete image suite initially passed 104/106 goldens. Swirly Stripes
and Wobbly Stripes differed by one byte on their pale constant bands, yielding
mean errors around 0.00195 even though all float samples passed. The reference
blends identical ramp endpoints through OKLab: its Double round-trip puts
green just below 229.5/255 (byte 229), whereas Mesa FP32 rounded to byte 230.

The compiler precomputes identical-endpoint interiors through the same Double
OKLab conversion and premultiplied-alpha blend. When FP32 would land on the
opposite side of a half-byte, it selects the neighboring float (at most one
ULP) that preserves reference byte rounding. Constant-span identity uses exact
host colours; nearby distinct colours that round alike remain distinct. Cache
RGB and the identity flag use spare parameter lanes, so numerical edits need
no recompilation. Exact stops still return the original colour (byte 230 in
this example), while general spans still interpolate in GLSL. A GPU regression
checks endpoints and the interior; a fast test checks identity changes without
source changes. Golden images and tolerances are unchanged.
