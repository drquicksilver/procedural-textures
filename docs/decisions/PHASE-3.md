# Phase 3: browser rendering decisions and spike evidence

## 3.1 status — 2026-10-06

The desktop spike supports proceeding with WebGL2. It renders Checker, Marble
and Cumulus on the bitten cube, plus XY/XZ/YZ slices, through an independent
renderer used by both a standalone preview and a headless CLI harness.
Phone measurements remain outstanding, so milestone 3.1 is still in progress.
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
[145,147,147,255] and the shader gives [147,149,148,255]. This is consistent with
amplified float precision differences, but the exact cause is not isolated.
The default comparison correctly fails this case. For the exploratory 512² run
only, an explicit maximum of 0.016 was used, retaining the original mean limit:

```
npm --prefix frontend run gpu:spike -- --size 512 --view scene --repeats 20 --compare --max-threshold 0.016
```

This is measured spike evidence, not a settled cross-device tolerance policy.
Keep the stricter 128² checks and investigate high-resolution outliers as part
of 3.2. Reference images and their acceptance policy are unchanged.

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
