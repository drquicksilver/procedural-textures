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
  Reference: https://cs.nyu.edu/~perlin/noise/ (Ken Perlin, 2002 algorithm).
  Octaves rotate about all three axes, with transforms compiled once.
- Lift Linear to planar projection, Circular to spherical distance, and Tiled
  to 3D parity with a depth count. Radial gains an explicit cylinder axis and
  retains its mirrored cosine-angle ramp so the z=0 non-noise slice is unchanged.
  A zero-length axis uses +z. No new Phase 3 primitives are introduced.
- Version 4 migrates v1→v2→v3→v4, preserving named ramps and use-site modes.
  Points receive z=0, scales z=their geometric-mean x/y scale, Radial axis +z,
  and checker depth=1. Zero z noise scale would make old examples planar forever,
  so scale gets a useful nonzero default instead of point semantics.

Implementation and measurement evidence will be appended per milestone.

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
