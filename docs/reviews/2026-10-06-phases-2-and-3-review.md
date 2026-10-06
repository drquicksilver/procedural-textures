# Review of Phase 2 and Phase 3 — 2026-10-06

Reviewed all 27 of the day's commits from `f62239a` through `5a2ba6d`,
including three merge commits. The initial review covered 21 commits through
`2056eee`; the extended review covered the six subsequent commits and revisited
the earlier findings. Line references below describe the file tree at `5a2ba6d`.
Focused reproductions also used temporary snapshots of the reviewed revisions.

The latest commits substantially strengthen the release. The browser renderer
now has credible cross-browser evidence, deployment tests exercise the actual
Pages artifact, and device-support claims are appropriately limited. The
architecture is sound and shipped workflows are well tested, but four concrete
implementation issues and one remaining test race need attention. Before adding
many more primitives, make the compiler's types and parameter layout easier to
reason about.

## Findings

### 1. P2 — Constant ramp interiors do not preserve reference alpha semantics

**New in `b7e7615`.** The shortcut at `frontend/src/gpu/shaders.ts:114` returns
cached RGB together with the original stop alpha. Haskell's interpolated colour
passes through `fromLab`, which clamps alpha to `[0,1]`.

Both document processors accept finite colour arrays outside that range. A
focused GPU diagnostic confirmed that identical stops containing
`[0.2, 0.3, 0.4, 2]` produce interior alpha **1 in Haskell and 2 in the GPU**.
With alpha `-0.5`, the results are **0 and -0.5**. The ordinary alpha-0.5 case
agrees.

The discrepancy has a visible compositing consequence. Layering the alpha-2
constant ramp over opaque red produces approximately `[-0.6, 0.6, 0.8]` on the
GPU, whereas the reference produces `[0.2, 0.3, 0.4]`. Final framebuffer clamping
cannot repair that composition. This is an imported-document edge case;
ordinary picker-generated colours are unaffected.

**Recommended fix:** preserve the reference's interpolated alpha in the
shortcut. Keep exact-stop behaviour separate, since Haskell returns the original
colour at an exact stop. Add tests for endpoints, interiors, and composition
using out-of-range alpha. Do not silently redefine document semantics by
globally clamping input colours.

### 2. P2 — The fBm fallback skips style remapping

**Still open; introduced in `f6742a9`.** At
`frontend/src/gpu/shaders.ts:63`, a nonpositive amplitude total returns `0.5`
immediately. Haskell applies the style's contrast mapping after choosing that
fallback.

With two octaves and persistence `-1`, the scalar results before ramp evaluation
are:

| Style | Haskell | GPU |
| --- | ---: | ---: |
| Smooth | 0.5 | 0.5 |
| Billowy | 0.875 | 0.5 |
| Ridged | 0.416667 | 0.5 |

These settings pass validation and can be entered through the numeric editor.
The Haskell expression and GPU material diagnostics reproduced the difference;
the extended review confirmed it remains present.

**Recommended fix:** apply the fallback before the normal style remapping.
Extend shared GPU fixtures to cover zero and negative amplitude totals.

### 3. P2 — Custom gallery builds require repository showcase materials

**Still open; introduced in `049cee1`.** `app/Main.hs:134` unconditionally looks
up six fixed material IDs when generating shape pages. The preview image is also
hard-coded to Agate.

A valid custom examples directory containing only Checker was reproduced
writing the material gallery and then failing with:

```text
agate: shape page material is not an example
```

The command leaves the site incomplete. The relevant implementation remains
unchanged in the extended review.

**Recommended fix:** select showcase materials and preview images from supplied
examples, or make the additional shape pages optional. Cover a small custom
library and an empty library.

### 4. P2 — Shared warp preparation defeats some opaque-layer skipping

**Still open; introduced in `8bd8feb`.** At
`frontend/src/gpu/compiler.ts:167`, the generated sharing preamble evaluates
turbulence before testing the top layer's alpha.

An opaque Flat above a lower layer containing two matching turbulence branches
still invokes `rawWarp`. An instrumented GPU shader confirmed one invocation
where none is needed, both in the initial review and at the final revision.

The rendered image remains correct, but expensive hidden work survives despite
the opacity optimisation. Its cost grows with octave count and the number of
shaded pixels. The reproduction verifies evaluation behaviour; it is not a
measurement of the resulting frame-time penalty.

**Recommended fix:** populate shared displacement values when a visible branch
first needs them, preserving reuse across visible branches. Add a regression
combining sharing with a fully opaque covering layer.

### 5. P3 — Renderer readiness still races after reload in browser tests

**Incomplete fix in `21d1b4d`.** The new wait correctly handles the shared
canvas in `beforeEach`, but backend checks following reload still access it
immediately after `networkidle0`. This occurs at
`frontend/e2e/smoke.test.mjs:151`, `:241`, and `:333`.

Network idle does not guarantee that the scheduled render has created the
canvas. With deliberately delayed frame scheduling, a focused reproduction
confirmed a null-canvas access followed by successful application mounting.

**Recommended fix:** extract one helper that waits for the renderer and verifies
its backend, then use it after every relevant navigation or reload. This is a
test-reliability issue, not evidence of a broken application.

All five findings are bounded fixes. No release-blocking defect was found in
the standard shipped-example workflows.

## Assessment of the latest commits

| Commit | Assessment |
| --- | --- |
| `0c76239` — compatibility and Pages | A substantial improvement. Native-browser checks, resource accounting, narrow layouts, and artifact-level deployment tests cover meaningful release risks. |
| `7b74040` — software compilation allowance | Reasonable. Disabling the watchdog is restricted to software test backends, with protocol/job timeouts retained. |
| `b7d9606` — llvmpipe CI | A justified response to the native SwiftShader crash. Verifying the actual backend is essential and is implemented. |
| `b7e7615` — constant-span rounding | Carefully targeted and well explained. It preserves goldens without weakening tolerances, but needs the alpha correction above. |
| `21d1b4d` — iPhone evidence and readiness | Physical-device evidence closes an important gap. Apply the readiness fix consistently. |
| `5a2ba6d` — Phase 3 closure | The documented release scope is defensible. It explicitly retains cold-start and untested-device limitations. |

## Assessment of the earlier work

The profiling and optimisation commits use matched controls, retain raw
measurements, and check exact pixels. Separating rendering, PNG encoding, and
combined cost makes the conclusions more useful. Combining the successful
optimisations with fresh controls avoids assuming that their independent gains
multiply. The golden and exact-image checks provide meaningful protection for
the production evaluator changes.

The 3D transition deliberately preserves original non-noise slices and records
intentional changes to noise-based renders. Keeping legacy images separately
prevents a later accept run from silently weakening that compatibility check.
Version-4 migration and shared fixtures cover old documents rather than simply
replacing the examples with new data.

The scene renderer samples materials at surface hits, shares object-space
coordinates with slices, and uses bounded tracing. Geometry, camera, cutaway,
and chess-piece tests cover analytic properties as well as reference images.

The shader diagnostics and focused Marble investigation are particularly
strong. The latter identifies a specific FP32 hit-threshold branch and checks
the material at the same hit point, rather than attributing an unexplained image
difference vaguely to floating point.

## Architecture

The strongest decision is keeping **Haskell as the semantic reference and
build-time source of data while removing it from the runtime application**.
Static hosting becomes straightforward without abandoning an independent
evaluator, migration reference, or golden suite.

Shader generation, GPU execution, scheduling/timing, and Preact presentation
have distinct responsibilities. One shared WebGL context serves the viewer and
thumbnails, avoiding browser context limits. Direct canvas presentation avoids
unnecessary encoding and readback. Explicit PNG export follows a separate path.
Bounded caches and context restoration address real lifecycle problems.

Exporting the actual Haskell SDF trees is another good choice. The browser
implements distance operations without maintaining a second independently
authored set of chess models, reducing model drift.

Geometry JSON is currently an **internal exported representation**. It is not
yet a complete user-facing model-document system: there is no corresponding
Haskell model parser, versioned model envelope, or model editor. Keep that
boundary explicit when Phase 4 starts.

## Code quality and structure

Four improvements would make future changes safer:

1. **Introduce stronger types at the compiler boundary.** Generic JSON nodes
   make sense for a schema-driven editor. They are less helpful inside an
   evaluator/compiler whose constructors have precise requirements. A validated,
   resolved discriminated union would let TypeScript check fields and constructor
   coverage while retaining the generic editing representation.

2. **Make the GPU parameter layout explicit.** Ramp stops and noise
   configurations depend on offsets, strides, and reused vector lanes. Converting
   a Lab alpha lane into a constant-span flag illustrates how tightly host packing
   and GLSL decoding are coupled. Named packing helpers and shared layout
   constants would make changes safer and easier to review.

3. **Separate validation rules from UI hints more clearly.**
   `frontend/src/document.ts:120` maintains another constructor/field inventory
   alongside the exported schema and Haskell parser. Shared fixtures mitigate
   drift, but new primitives still require several coordinated edits. Exporting
   structural validation metadata, or generating appropriate types and field
   descriptions, would reduce duplication. Slider limits should remain distinct
   from document restrictions.

4. **Reduce compressed code and incidental dependencies.** Long generated GLSL
   expressions and densely packed TypeScript make mathematical and lifecycle
   changes harder to audit. Small named helpers and readable shader formatting
   would help. The Pages builder also imports `gpu-session.mjs` just for a
   directory path, unnecessarily pulling browser-harness dependencies into site
   assembly. A small shared paths module would be cleaner.

These recommendations do not require a wholesale rewrite. The implementation
is relatively small and its component boundaries are useful. Strengthen those
boundaries incrementally, particularly before expanding the language.

## Functionality and performance

The release covers considerably more than rendering on the development laptop.
Tests exercise persistence failures, undo/redo, import/export, slice
manipulation, shader reuse, context restoration, unavailable WebGL2, narrow
layouts, and editor/gallery navigation beneath the repository prefix. Testing
the assembled Pages output before deployment is a strong protection against
asset-path and packaging errors.

The main remaining user-experience concern is **cold shader compilation**.
Warm rendering is fast, but compilation and first execution can block the main
thread; the recorded Firefox first render reaches 1.22 seconds. Adaptive
resolution cannot solve this because its estimator deliberately excludes cold
compilation. Thumbnail prioritisation cannot interrupt a compilation already
running either.

Make cold-start responsiveness the next performance investigation: measure
library-opening and structural-edit stalls, ensure loading feedback appears
before blocking work, and consider asynchronous compilation where supported.

The support policy is appropriately cautious. The physical iPhone result is
evidence for that device, not a general mobile guarantee. Android remains
unverified. Avoiding a second CPU browser renderer is reasonable given current
evidence.

## Validation

Fresh verification during the extended review passed:

- `stack build` and all **473 Haskell tests**.
- All **603 frontend tests** and the production build.
- All **22 browser tests against the rebuilt Pages artifact**, served beneath
  the repository prefix.
- Chrome GPU conformance, mutation/lifecycle checks, and **106 golden-image
  comparisons** on the M1 Pro hardware backend.
- Native Firefox and Safari: **123 sample cases and 106 golden-image comparisons
  each**, plus bounded resource counts across repeated structural edits and
  complete disposal of tracked renderer objects.
- Focused GPU diagnostics reproduced the alpha discrepancy, fBm fallback
  discrepancy, and hidden warp evaluation. A delayed-frame browser check
  reproduced the remaining renderer-readiness race.

The earlier review also reproduced the custom-library gallery failure. Its
relevant implementation is unchanged at the final revision.

Linux llvmpipe and physical iPhone checks were not rerun during this review;
those conclusions rely on committed evidence. The live Pages deployment was
not independently rechecked during the extended review; the assembled artifact
was rebuilt and tested locally. New compatibility timings were not collected
as a controlled performance study: correctness and resource assertions are the
fresh verification results, while performance conclusions refer to the recorded
release measurements.

No production source, reference goldens, or shared fixtures were changed by the
review. Passing supplied suites does not negate the targeted findings above.

## Recommended order of follow-up work

Fix the five findings first, then clarify parameter packing and validated
compiler types. This leaves a stronger foundation for Phase 4, where richer
composition will put substantially more pressure on validation, optimisation,
and shader generation. Investigate cold-start responsiveness alongside that
work without weakening reference comparisons or expanding device claims beyond
the available evidence.
