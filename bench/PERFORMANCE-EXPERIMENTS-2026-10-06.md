# Six independent performance experiments — 2026-10-06

The strongest broadly useful change is explicit octave-coordinate inlining.
Same-domain field sharing and opaque-layer skipping also help, with benefits
that depend on composition. `-O2` alone does not address the main allocation
cost. The buffered tile prototype supplies no convincing latency improvement;
coarser task chunks generally hurt.

All six experiments have separate detached worktrees based on `f62239a`.
None incorporates another treatment. A seventh worktree supplies the unchanged
baseline. **No treatment is merged into the production evaluator, and PNG
compression/settings are unchanged.** The cohort is the same ten expensive
examples selected by the previous full-gallery baseline.

## Comparative results

Figures below are geometric means of per-example speedups across the ten
textures, using each example's median batch latency. **Above 1 means faster;
below 1 means slower.** Allocation is the geometric mean percentage change in
cumulative allocated bytes on the full-resolution render-and-PNG path, not
resident memory. Small single-digit timing differences are inconclusive with
this sample size and scheduling variability.

| Independent experiment | 512² render + PNG | 512² render only | 96² render + PNG | Allocation change |
|---|---:|---:|---:|---:|
| Project `-O2` | 1.04× | 0.97× | 1.00× | -1.0% |
| Inline octave coordinates | 1.59× | 1.78× | 1.41× | -61.1% |
| Opaque-layer skipping | 1.16× | 1.21× | 1.12× | -15.8% |
| Same-domain displacement sharing | 1.18× | 1.13× | 1.11× | -14.2% |
| Stage-wise unboxed tiles | 1.03× | 1.01× | 1.00× | +7.2% |
| 16-row task chunks | 0.97× | 0.87× | 0.86× | 0.0% |

The primary outcome uses five interleaved timing samples per example/treatment.
Render-only and preview outcomes use three. Every full-resolution sample is
five operations; previews use forty. The measurements exclude startup,
document/ramp resolution and warm-up, and include the final collection in the interval.
Order is deterministically shuffled, and no builds/tests run concurrently with
the measurement processes. The final two primary rounds independently confirm
the first three rather than comparing against a previous day's run.

Full-resolution render-and-PNG medians, in milliseconds:

| Texture | Baseline | `-O2` | Inline | Opaque | Shared fields | Tiled kernels | Chunk 16 |
|---|---:|---:|---:|---:|---:|---:|---:|
| Cumulus | 266.8 | 278.4 | 133.7 | 237.4 | 160.2 | 259.8 | 301.9 |
| Mossy Stone | 211.9 | 218.3 | 136.9 | 192.1 | 173.2 | 203.2 | 224.2 |
| Rust | 210.8 | 198.3 | 120.5 | 213.2 | 175.0 | 198.7 | 227.6 |
| Ice | 155.4 | 141.3 | 92.5 | 113.0 | 149.4 | 149.5 | 160.0 |
| Water Ripples | 156.1 | 142.7 | 125.3 | 95.2 | 109.4 | 150.5 | 153.9 |
| Tiger Fur | 143.5 | 138.8 | 99.3 | 143.1 | 137.9 | 167.1 | 155.4 |
| Jupiter | 131.0 | 138.9 | 84.3 | 146.3 | 131.5 | 126.2 | 143.6 |
| Marble | 155.9 | 149.2 | 87.0 | 131.8 | 132.0 | 142.6 | 141.1 |
| Wood Knot | 156.4 | 134.6 | 94.8 | 119.1 | 131.0 | 135.3 | 141.8 |
| Mountain Ridges | 135.0 | 133.0 | 95.5 | 119.8 | 139.4 | 136.2 | 150.8 |

## What each experiment tested

1. **Project optimisation level (`o2`).** Add `-O2` to the common Cabal options,
   compiling all local library/executable/test code at that level. Compile the
   probe at `-O2` too. Saved library compiler options were checked and contain
   `-O2`. Cached third-party dependencies are unchanged. The roughly 4% primary
   timing gain is not convincing: render-only is roughly 3% slower, previews
   are unchanged, CPU cost improves only about 2%, and allocation changes by
   just 1%. This does not substitute for the explicit representation fix.

2. **Coordinate representation (`inline`).** At `-O1`, add only
   `{-# INLINE transformOctave #-}`. The octave loops can eliminate their
   temporary coordinate pairs. It improves every primary per-texture median,
   cuts allocation by 61% across the cohort, and improves render-only by 1.78×.
   Cumulus falls from 267 to 134 ms and 7.06 to 2.28 GB allocated. This is the
   clearest first optimisation to adopt.

3. **Demand-driven composition (`opaque`).** At `-O1`, return the top colour
   when alpha is exactly one, without forcing the bottom layer. Leave all
   other blending arithmetic unchanged. It helps Water Ripples (156 to 95 ms)
   and Ice (155 to 113 ms), while other compositions gain little or have a
   noisy regression. Allocation falls 16% overall. The benefit tracks how much
   expensive material is completely occluded, so it is not a uniform multiplier.

4. **Repeated-field elimination (`shared`).** At `-O1`, collect sibling warp
   parameters within one layer domain. Share the two raw turbulence displacement
   values when octave count, persistence and lacunarity match, applying each
   child's amplitude afterwards. Stop at a warp boundary: nested domains must
   remain distinct. This is per-pixel value reuse, not merely reusing functions,
   and there is no global floating-coordinate cache. Cumulus's seven matching
   lobe warps benefit strongly (267 to 160 ms; allocated bytes halve). Moss,
   Rust and Water Ripples also have eligible repeated fields. Six cohort
   examples have no eligible sharing; apparent changes there are mainly noise,
   not evidence of a general speedup from the cache. The cohort's allocation
   reduction is 14%, with much larger reductions in eligible compositions.

5. **Different execution model (`batch`).** At `-O1`, compile Layer and
   Turbulence into vector kernels over unboxed Double coordinate/colour
   buffers; use the existing scalar leaf evaluator for other nodes. Render
   four-row tiles, preserving the baseline's four-row task boundaries, and
   convert to bytes only at the end. Route the experimental server/probe through
   this backend. This is a stage-wise CPU buffer prototype, not explicit SIMD
   or a GPU implementation. Latency is essentially unchanged, while allocation
   increases 7%. Buffering by itself leaves the octave allocation problem intact
   and adds intermediate data. Do not adopt this prototype. A future batched
   backend needs fused/specialised or genuinely vectorised kernels to justify it;
   this result does not rule out those approaches.

6. **Scheduling (`schedule`).** At `-O1`, change `rowsPerSpark` from 4 to 16,
   without changing pixel evaluation. The primary outcome gets slightly slower;
   render-only latency is about 15% higher and preview latency about 16% higher.
   Allocation is unchanged, consistent with fewer larger tasks reducing
   balancing opportunities, especially for small images. Retain four-row chunks
   on this evidence.

## Scheduling controls

A separate factorial measurement varies capabilities `-N4`, `-N8` and `-N10`
for **both** the original four-row and experimental sixteen-row chunks, on
Cumulus, Moss and Wood Knot at 512² and 96². It retains three samples per case,
108 samples total. This prevents attributing a thread-count effect to the
chunk-size code change.

Four capabilities are consistently slower than eight/ten in these cases.
Eight capabilities look promising for some previews (Moss: 8.1 ms versus
11.0 ms at ten with the original chunks), but do not consistently win at full
resolution. Cumulus's 512² original-chunk medians are 457/291/287 ms at 4/8/10
capabilities; its sixteen-row medians are 454/344/304 ms. Do not change the
application's global capability count based on this small hardware-specific
matrix. Tuning preview and final-render scheduling separately is a possible
follow-up after the evaluator fixes.

## Recommendation

Adopt and remeasure **inlining first**. Next test a combined build containing
inlining, opaque skipping and domain-correct field sharing; their independent
speedups must not be multiplied, because they eliminate overlapping work.
Keep the artwork and exact-pixel checks. A production field-sharing compiler
should use statically assigned slots/bindings instead of per-pixel list/key
lookups in this prototype. Preserve domains and arithmetic order.

Keep `-O2`, stage-buffered evaluation and larger task chunks as negative or
inconclusive results, rather than landing them simply because they sound
plausible. The CPU/allocation measurements support the three targeted changes
more strongly than the small timing differences in the others. PNG is unchanged
throughout and is not a proposed optimisation in this experiment set.

## Validation, isolation and reproduction

Every worktree passes `stack build` and all 399 Haskell tests. Across the six
treatments, **2,010 exact RGBA image comparisons pass**: all 67 examples
at 1², 3², 13², 96² and 512², six treatments. The tiny/odd dimensions exercise
partial final tiles. No golden files or test vectors were regenerated.
The tests and exact-pixel checks are separate from timed runs.

Environment: Apple M1 Pro, ten cores, GHC 9.10.3, the same Stack snapshot and
cached dependency versions. Default experiment/probe compilation is `-O1`;
only the `o2` treatment changes optimisation level. Runtime is `-N10` except
for the labelled factorial controls. Small differences remain noisy; the large
allocation and inlining gains are much stronger evidence than a changed rank.

| Worktree | Revision |
|---|---|
| `baseline` | `6158fdd` |
| `o2` | `7888284` |
| `inline` | `068e66c` |
| `opaque` | `2175d7f` |
| `shared` | `22e70c4` |
| `batch` | `9f0440d` |
| `schedule` | `cdb6bde` |

The worktrees remain under `/private/tmp/procedural-texture-experiments-20261006/`.
They are committed and clean. [Revisions and paths](profiling/2026-10-06/worktrees/revisions.json)
are recorded, and the portable patches allow reconstruction if temporary
worktrees are later removed.

- [Primary and secondary samples](profiling/2026-10-06/worktrees/measurements.csv): 770.
- [Scheduling controls](profiling/2026-10-06/worktrees/scheduling.csv): 108.
- [Exact image checks](profiling/2026-10-06/worktrees/pixel-verification.csv): 30 groups of 67.
- [Aggregate scores](profiling/2026-10-06/worktrees/summary.json).
- [Reproduction instructions and patches](profiling/2026-10-06/worktrees/README.md).

The main checkout contains the report, evidence and measurement runner only;
its production library, server and examples are unchanged.
