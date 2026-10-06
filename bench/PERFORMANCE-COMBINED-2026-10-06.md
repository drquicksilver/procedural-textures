# Combined performance optimisations — 2026-10-06

The three successful approaches work together. The combined evaluator improves
512² rendering plus PNG by **1.97×**, rendering alone by **2.41×**, and 96²
previews plus PNG by **1.60×** across the ten expensive examples. Cumulative
allocation falls **67.2%**. Against a freshly measured inlining-only control,
combining opacity and displacement sharing adds another **1.28×** full-resolution
render-and-PNG speedup. These are geometric means of per-example median ratios.

The combined changes are integrated into `src/Texture.hs`. Normal GHC `-O1`,
four-row parallel tasks, ten RTS capabilities and PNG settings are unchanged.
The unsuccessful `-O2`, buffered-backend and larger-chunk experiments are excluded.

## Matched results

| Treatment | 512² render + PNG | 512² render only | 96² render + PNG | Allocation change |
|---|---:|---:|---:|---:|
| Inlining alone | 1.54× | 1.87× | 1.40× | -61.1% |
| All three combined | **1.97×** | **2.41×** | **1.60×** | **-67.2%** |

The baseline and inlining controls were rerun alongside the combined candidate;
comparisons do not reuse the earlier study’s elapsed times. Small differences
for individual examples, such as Jupiter, are inconclusive with this sample size.

Full-resolution render-and-PNG medians, in milliseconds:

| Texture | Baseline | Inlining alone | Combined | Combined speedup | Combined 96² preview |
|---|---:|---:|---:|---:|---:|
| Cumulus | 272.8 | 157.1 | 80.0 | 3.41× | 4.05 |
| Mossy Stone | 205.0 | 136.7 | 121.6 | 1.69× | 4.72 |
| Rust | 211.8 | 123.7 | 103.0 | 2.06× | 5.37 |
| Ice | 154.9 | 116.2 | 69.8 | 2.22× | 4.06 |
| Water Ripples | 145.3 | 89.9 | 52.0 | 2.80× | 3.03 |
| Tiger Fur | 161.6 | 92.8 | 87.3 | 1.85× | 4.51 |
| Jupiter | 132.7 | 82.7 | 84.6 | 1.57× | 5.12 |
| Marble | 144.2 | 88.6 | 87.1 | 1.65× | 4.20 |
| Wood Knot | 131.9 | 113.6 | 89.7 | 1.47× | 4.26 |
| Mountain Ridges | 144.7 | 97.7 | 84.7 | 1.71× | 4.85 |

Eight of these ten examples now have 512² medians below 100 ms. Mossy Stone
(121.6 ms) and Rust (103.0 ms) remain above that interactive target. All ten
96² preview medians are below 10 ms. This study is the selected expensive
cohort, not a new full-library benchmark sweep; `bench/baseline.csv` retains
its historical measurements and is not a baseline for the newly integrated code.

## What was combined

1. **Inline octave-coordinate transforms.** `INLINE transformOctave` allows
   the octave loops to eliminate temporary coordinate pairs. This supplies
   most of the general allocation reduction.
2. **Skip fully hidden layers.** When top alpha is exactly one, return the
   top colour without evaluating the bottom colour. All other blend arithmetic
   remains unchanged. This also applies inside shared layer compositions.
3. **Share repeated displacement samples within a coordinate domain.** Sibling
   warps with matching octave count, persistence and lacunarity reuse their
   two turbulence values for a pixel. Each warp applies its own amplitude.
   Collection stops at warp boundaries, so nested coordinates remain distinct.
   The sample list and its values remain lazy: opacity can still avoid hidden work.

These gains overlap, so multiplying the earlier independent speedups would
overstate the combined benefit. Composition determines the incremental gain:
Cumulus benefits strongly from repeated lobe warps, while Ice and Water Ripples
benefit from skipping hidden layers. The fresh inlining control makes this
interaction measurable rather than assuming the improvements add.

## Validation and measurement

- Both candidates match all 67 baseline renders byte-for-byte at five square
  sizes: 1, 3, 13, 96 and 512. That is **335 exact-image checks for the combined
  evaluator**. Goldens and test vectors are unchanged.
- Five focused regression tests cover ordinary and shared-path opacity skipping,
  different warp amplitudes, nested coordinate domains and distinct warp parameters.
- `stack build` followed by `stack test` passes **404 tests** on the integrated code.
- Apple M1 Pro, ten cores, GHC 9.10.3; ordinary `-O1` library and probe builds.
- Three separate worktrees: baseline, inlining only, and combined. Each case
  interleaves all three variants in a deterministically shuffled order.
- 512² render-and-PNG uses five samples of five operations per example/variant;
  render-only uses three samples of five; 96² uses three samples of forty.
  In total: **330 timing samples**. Startup, document resolution and warm-up
  are excluded; final collection is included. No builds/tests run during timing.
- Allocation is cumulative bytes, not resident RAM. Aggregates use geometric
  means of median ratios; the raw samples retain timing variation.

[Raw evidence and reproduction instructions](profiling/2026-10-06/combined/README.md)
include measurements, exact-pixel results, the combined patch, source revisions
and median summaries. The [independent study](PERFORMANCE-EXPERIMENTS-2026-10-06.md)
remains available as the record of individual treatments.

The next useful investigation is the remaining Moss/Rust evaluation cost after
inlining, not PNG compression. The current sharing implementation recognises
only repeated sibling turbulence fields within one layer domain; it does not
eliminate arbitrary repeated subexpressions. Broader sharing would require
careful domain identity and another matched benchmark.
