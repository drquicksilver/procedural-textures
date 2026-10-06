# Phase 2 decision log

2026-10-06. The user authorised completing Phase 2 without questions, including
making the design calls formerly reserved for Jules in PLAN.md.

## 2.1 — Coordinate and noise design

- Use a right-handed unit-cube material space: +x right, +y down, +z into the
  default slice. Shapes are centred at (0.5, 0.5, 0.5). Fields are unbounded;
  the unit cube is a viewing convention, not a clamp. This preserves existing
  x/y points, scale and preview handles. The default legacy slice is z=0.
- Keep texture values as data and compile them to a three-argument field.
  Keep `textureToImageFn` as the z=0 convenience adapter for CLI compatibility.
- Use improved Perlin's quintic interpolation and permutation hashing with
  32 approximately evenly distributed, rotated unit gradients rather than
  the 12 cube-edge gradients. The plan explicitly identifies axis-slice grid
  artefacts; increased directional coverage avoids repeating the old choice.
  Reference: [Ken Perlin’s 2002 improved-noise implementation](https://cs.nyu.edu/~perlin/noise/).
  Octaves rotate about all three axes, with transforms compiled once.
- Lift Linear to planar projection, Circular to spherical distance, and Tiled
  to 3D parity with a depth count. Radial gains an explicit cylinder axis and
  retains its mirrored cosine-angle ramp so the z=0 non-noise slice is unchanged.
  A zero-length axis uses +z. No new Phase 3 primitives are introduced.
- Version 4 migrates v1→v2→v3→v4, preserving named ramps and use-site modes.
  Points receive z=0, scales z=their geometric-mean x/y scale, Radial axis +z,
  and checker depth=1. Zero z noise scale would make old examples planar forever,
  so scale gets a useful nonzero default instead of point semantics.

Implementation and measurement evidence is recorded below per milestone.

## 2.2 — Core implementation evidence

- All 67 original version-3 documents are retained together in a fixture and
  compared against their canonical migrated version-4 examples. Existing
  version-1/2 tests retain their named/built-in ramp and mode cases.
- The 11 noise-free examples are byte-identical to their original 128² renders;
  their original goldens are retained separately, so later accepts cannot
  silently weaken this migration guarantee. All 56 noise-based goldens change
  intentionally because noise and warps now sample a 3D field.
- 13,005 deterministic off-lattice samples give noise 1st/50th/99th percentiles
  0.212/0.474/0.785. Central-difference squared energies on x/y/z are
  3.47e-7/4.92e-7/4.56e-7 (largest/smallest 1.42). This is a distribution sanity
  check, not proof of isotropy. Values are clamped to [0,1]; fBm retains its
  explicit style remapping. Octave matrices are compiled once as 3x3 matrices
  and the proven INLINE optimisation is retained.
- No client-side migration is duplicated: local storage/import continues to
  call the backend's canonical migration endpoint, avoiding two migration rules.

Core milestone validation: `stack build`, 411 Haskell tests, 295 frontend
unit tests, production frontend build, and all 13 real-browser editor tests pass.

## 2.3–2.4 — Geometry and scene decisions

- Use reusable SDF data constructors (sphere, box, capped cylinder, torus,
  plane, union, intersection, difference). The seven shape presets are data
  compositions of these primitives, including a spherical bite, removed
  octant and planar cut. No mesh/UV mapping is involved.
- Sphere tracing first intersects the common 0.75-radius bounding sphere,
  advances conservatively by 90% of signed-distance magnitude and stops at
  128 steps or the far bound. Surface tolerance is 0.0005 object units.
  The bounds fit every shipped solid and make misses inexpensive.
- Orbit the camera around a fixed material-space centre. Perspective uses a
  40-degree FOV; normals are central differences of the same SDF. Fixed
  upper-left frontal lighting uses 30% ambient plus 70% diffuse. No expensive
  shadow rays or speculative acceleration backend are needed for Phase 2.
- A material is sampled once per surface hit, after tracing, not at every
  ray step. This is important given the preceding texture performance study.
  Surface alpha composites over the fixed background; this is a surface
  viewer, not volumetric transparency.
- Slices inspect the unlit field across the unit square, with XY/XZ/YZ
  orientations and a movable plane. They deliberately include the complete
  material cross-section rather than clipping to a particular shape, retaining
  the useful original diagnostic 2D view.
- The existing render endpoint accepts view/camera/slice query parameters;
  omitting them preserves the original z=0 PNG API. Values are finite and
  bounded and reuse the existing size/body/timeout limits. `/api/shapes`
  provides viewer choices. CLI `render --shape` and `--axis/--slice` also work.
- Analytic tests cover distances, boolean interiors, hit/miss tracing, camera
  aim, outward/cut-wall normals and slice mappings. The 21 intentionally added
  96² scene goldens cover all seven shapes on checker, marble and malachite.
  Cutaway goldens were visually inspected to verify the actual material volume.

Scene milestone validation: `stack build` and all 441 Haskell tests pass.
The new scene goldens are accepted intentionally; texture goldens do not change.


## 2.5 — Viewer interaction decisions

- Start on the bitten cube: the opening view immediately demonstrates that
  these are solid fields. The shape dropdown is populated by the backend.
  Orbit and zoom have both pointer and keyboard controls, bounded pitch and
  distance, and an explicit camera reset.
- Camera/slice state is separate from texture documents and undo/autosave.
  Camera movement must not create copies of read-only examples or overwrite
  saved materials. It lasts for the open editor session rather than being
  embedded in the material format.
- Keep the proven coalescing preview scheduler. Add an explicit interaction
  flag that suppresses full renders while dragging/scrolling/scrubbing and
  schedules refinement on release. Disposal now cancels previews and revokes
  image URLs. Actual browser requests verify the low/full distinction.
- Project point handles into each principal slice plane. Dragging changes its
  two displayed components and preserves the third. Shell-radius handles use
  the actual spherical cross-section, including offset from the slice plane;
  no radius handle is shown when the sphere does not intersect that plane.
- Optional animated sweeping is omitted: the manual depth slider supplies the
  required exploration without an ongoing render loop or unsolicited motion.
- Production solid and XZ-slice screenshots were inspected at 1440×960. The
  controls, image, caption and inspector remain visible without clipping.
  Validation: 302 frontend unit tests, production build, all 16 browser tests
  and the 442-test Haskell suite pass.


## 2.6 — Gallery, performance and completion

- HTML cards pair a cutaway with its original diagnostic XY/z=0 slice and
  label both views. The contact sheet uses adjacent square 128² previews,
  rather than squeezing two views into one distorted square. All 67 materials
  are present (134 preview images), with existing category order and wrapped
  descriptions. Local-file HTML and contact-sheet layouts were inspected in
  Chrome; no server is needed to review the generated gallery.
- Record a new 3D baseline rather than treating older 2D timings as comparable.
  Seven shapes × checker/marble/cumulus × 96/512 pixels gives 42 cases. The
  source/core noise model is intentionally different, so old benchmark CSVs
  are retained as historical evidence. The default harness now includes 3D
  scenes and has separate scene-only and extreme-view modes.
- Default-camera render+PNG estimates are 2.2–5.0 ms for 96² and 14.5–95.4 ms
  for 512². A close Cumulus preview is 8.2 ms; close 512² is 188 ms, and 1024²
  refinement reaches 712 ms. Keep 96² while interacting and retain responsive
  frame-sized final images up to 1024², rendered only after release/settling.
  This balances large-screen detail with movement latency. Means are not
  worst-case guarantees; raw spreads and sample scope are in the report.
- Reject nonfinite three-coordinate inputs with their JSON path. Legacy
  migration avoids overflowing the derived depth scale before the parser can
  report an invalid original coordinate. Very large finite scale products use
  a stable geometric-mean fallback; ordinary fixture coordinates retain their
  exact previous migration arithmetic. Tests cover versions 1–4.
- Extend the displacement-sharing regressions to three entry depths and a
  genuinely three-dimensional gradient. Add explicit fractal-depth and z-warp
  checks. All existing texture/scene goldens remain unchanged by this hardening.
- Adapt the isolated profiler to the new vector dependency. Its old inlining/
  opacity experiments are refused on an evaluator that already contains those
  changes; historical worktree patches remain the reproducible source for that
  earlier comparison. Its current 3D export, isolated profiled compilation and
  cost-centre smoke run pass. `bench/NoiseStudy.hs` reproduces the gradient sanity check.
- Final checks: `stack build`, all **444 Haskell tests**, **302 frontend unit
  tests**, production frontend build and all **16 real-browser tests** pass.
  Tests remain fast (about 2.1 seconds for Haskell, about one second for frontend
  units); browser checks remain in the secondary suite. The default-camera and
  stress benchmark files record 47 cases. The generated gallery has 136 files
  (134 previews, HTML and contact sheet). Temporary QA servers are stopped.

Phase 2’s exit criteria are met. Work is committed in sequential milestones
on main, with intentional golden/schema changes identified in commit messages.
