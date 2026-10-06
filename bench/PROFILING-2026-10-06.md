# Expensive-texture profiling — 2026-10-06

Profiled the ten slowest 512² PNG examples from the 67-example baseline at
`33aa569`, on the Apple M1 Pro (10 cores), GHC 9.10.3. The main bottleneck is
allocation-heavy coordinate evaluation, not the Perlin arithmetic or PNG
encoding. Two small, isolated evaluator experiments give 1.49–2.46× faster
full render-and-PNG results with identical sampled pixels.

## Normal renderer: where time goes

All times below are milliseconds, reported as medians of five batches. These
measurements use the existing production library with ten capabilities. Each
512² batch contains three operations, except the noisy Wood Knot render-only
case, refined with twenty. Each 96² batch contains twenty operations. Document
loading/resolution and warm-up are excluded. Encode-only measurements force the
image before timing. The final GC is included in the interval to capture all
allocated bytes. Independent stages have scheduling variance and their medians
should not be added or subtracted as if they were one trace.

| Texture | Render 512² | Encode only 512² | Combined 512² | Combined 96² | Render allocation |
|---|---:|---:|---:|---:|---:|
| Cumulus | 255.1 | 11.2 | 271.3 | 11.0 | 7.06 GB |
| Mossy Stone | 167.0 | 42.3 | 200.6 | 9.0 | 4.21 GB |
| Rust | 174.9 | 28.1 | 206.1 | 9.3 | 4.66 GB |
| Ice | 133.6 | 15.2 | 146.8 | 7.1 | 3.47 GB |
| Water Ripples | 144.6 | 13.3 | 146.6 | 6.7 | 3.41 GB |
| Tiger Fur | 117.0 | 26.2 | 140.4 | 6.3 | 2.93 GB |
| Jupiter | 111.5 | 27.3 | 129.9 | 5.7 | 2.69 GB |
| Marble | 93.2 | 39.5 | 130.2 | 5.7 | 2.42 GB |
| Wood Knot | 103.2 | 34.7 | 129.3 | 5.5 | 2.30 GB |
| Mountain Ridges | 104.4 | 32.7 | 134.3 | 5.7 | 2.57 GB |

Allocation means **cumulative bytes allocated per render**, not peak/resident
memory. GC accounts for approximately 0.9–1.0% of render CPU time at 512². Most
objects die quickly; allocating them still costs evaluator CPU time. Larger
nurseries are therefore a lower priority than removing the objects themselves.
PNG encoding alone takes 11–42 ms: worth revisiting after evaluator fixes, but
not the principal cost in the worst cases. The small-preview measurements
remain close to the old 10 ms target for Cumulus and some other cases; this
probe includes cleanup and is not a replacement for the tasty-bench baseline.

## Focused evaluator profiles

GHC cost centres added with `-fprof-late` after optimisation identify:

- `transformOctave`: **27–40% of sampled time and 46–61% of allocation** across
  all ten examples. It returns a pair of transformed coordinates inside every
  noise octave. The unprofiled inlining experiment below confirms that these
  temporary values are a real production cost, not solely profiling overhead.
- The closures attributed to `textureToImageFn`: **24–35% of time and 23–32% of
  allocation**. These include coordinate warping and composition; this label
  does not mean the texture tree is recompiled every pixel.
- `perlin2`: **7–11% of time and no measurable allocation at the reporting
  precision**. Optimising the gradient/permutation arithmetic first would miss
  the larger problem.
- OKLab conversion and ramp evaluation are secondary costs. Stop sorting,
  stop conversion and octave matrix preparation already happen once per node;
  the profiles do not suggest moving those preparations again.

The focused driver forces all four colour channels on three 512² grids, on
one CPU. It excludes the image buffer, byte conversion and encoder. Its own
checksum loop accounts for 5–13% of time; the listed percentages are individual
GHC cost-centre figures including that driver, not percentages of normal PNG
wall time. Cost-centre instrumentation changes timing and should only be used
for attribution. Raw summaries are retained in
[core-costs.csv](profiling/2026-10-06/core-costs.csv).

Cumulus does 76 Perlin evaluations per pixel in the original evaluator: about
19.9 million at 512². The measured entry count agrees with its nine turbulence
nodes. Other examples' potential budgets are Moss 44, Rust 50, Ice 33,
Water Ripples 32, Tiger Fur 29, Jupiter 28, Marble 25, Wood Knot 22 and
Mountain Ridges 24; these include direct Perlin nodes as well as FBM octaves.
Their different colour and compositing costs explain why counts alone do not
predict every ranking.

## Isolated optimisation experiments

These are temporary builds, **not production changes**. All three evaluator
builds use `-O1`, the same image renderer/encoder, and identical resolved
texture constructors exported from JSON. They are measured sequentially with
five batches of three 512² render-and-PNG operations. The matched baseline is
measured again rather than compared directly to an older day's numbers.
Timings are milliseconds; allocation reduction includes the PNG path.

1. Add `{-# INLINE transformOctave #-}` so the pair and intermediate coordinate
   work can be eliminated in the octave loops.
2. On top of that, let `blend` return the top colour immediately when its alpha
   is exactly one, **without forcing the bottom colour**. Other alpha values
   still use the original formula. The current pattern-matching implementation
   evaluates both layers even when the bottom layer is fully occluded.

| Texture | Matched baseline | Inline transform | Inline + opaque layer | Total speedup | Allocation reduction |
|---|---:|---:|---:|---:|---:|
| Cumulus | 268.8 | 135.2 | 114.3 | 2.35× | 76% |
| Mossy Stone | 214.3 | 126.2 | 123.4 | 1.74× | 64% |
| Rust | 227.0 | 127.9 | 108.9 | 2.08× | 69% |
| Ice | 147.3 | 88.2 | 70.0 | 2.11× | 69% |
| Water Ripples | 142.0 | 87.9 | 57.7 | 2.46× | 76% |
| Tiger Fur | 138.7 | 89.6 | 82.8 | 1.67× | 64% |
| Jupiter | 129.8 | 80.6 | 80.5 | 1.61× | 65% |
| Marble | 131.0 | 89.8 | 86.7 | 1.51× | 65% |
| Wood Knot | 127.9 | 94.7 | 86.1 | 1.49× | 61% |
| Mountain Ridges | 132.9 | 93.6 | 84.5 | 1.57× | 63% |

Both experimental variants produced byte-identical full 512² RGBA buffers for
all ten examples against the production evaluator. This check does not cover
all possible documents or blend edge cases. Seven of the ten measured combined
medians fall below 100 ms; Cumulus, Mossy Stone and Rust still exceed it. No
example documents, goldens or original baseline files were changed.

## Recommended order

1. **Adopt octave-transform inlining first.** It supplies the largest measured
   improvement and changes no arithmetic. Inspect the optimised worker to
   confirm pair elimination; retain full golden coverage, then refresh the
   normal full-library baseline. If this becomes fragile under future changes,
   use a strict/unboxed coordinate worker or inline the transform at the octave
   call sites, with the same arithmetic order.
2. **Add the opaque-layer fast path next.** Its additional gain is especially
   useful for Water Ripples, Ice, Rust and Cumulus. Keep the bottom argument
   lazy until the opacity decision. Add tests for opaque, transparent and
   partially transparent compositing, including both-transparent colour
   behaviour, and run all goldens. A transparent-top shortcut needs separate
   care: blindly returning a fully transparent bottom can change hidden RGB
   compared with the current canonical zero result.
3. **Profile again before another evaluator rewrite.** The remaining closures
   and colour tuples are candidates for stricter/unboxed workers, but their
   importance will change after the two measured fixes. Preserve arithmetic
   ordering where byte identity matters. No additional speedup is claimed for
   these untested candidates.
4. **Use Phase 3's field decomposition to share repeated evaluations.** The
   seven Cumulus lobe warps all use four octaves, persistence 0.35 and lacunarity
   2 at the same incoming coordinates; only the displacement amplitude differs.
   Their two raw displacement fields can be shared before applying each
   amplitude. That could reduce those seven warps' 56 noise evaluations to eight
   where all are needed. This is a structural opportunity, not a measured
   speedup. Share values per pixel/domain, not just function definitions; avoid
   a global cache keyed by floating-point coordinates, and preserve distinct
   transformed domains.
5. **Revisit PNG encoding after evaluator work.** Mossy Stone, Marble and Wood
   Knot have the largest isolated encoder times. Measure the encoder/filter/
   compression trade-offs separately, retaining lossless decoded pixels, before
   replacing the codec or changing output size. Runtime thread-count/chunk-size
   tuning is also a later experiment; this study does not establish a better
   setting than the existing renderer's ten-core execution.

The first two changes are worthwhile before Phase 3 and do not require a new
primitive or altered artwork. The larger reuse/representation work fits the
planned scalar-field/domain-transform refactor.

## Reproduction and evidence

The [probe instructions](profiling/README.md) describe the normal, focused and
experimental builds. The runner uses temporary source copies only for focused
profiling and experiments; the production library remains unchanged.

- [Normal stage samples](profiling/2026-10-06/stages-n10.csv): 300 batch records.
- [Matched experiment samples](profiling/2026-10-06/experiments.csv): 150 records.
- [Focused cost centres](profiling/2026-10-06/core-costs.csv).
- Pixel equality logs: [inlining](profiling/2026-10-06/pixels-inline.txt) and
  [inlining plus opacity](profiling/2026-10-06/pixels-opaque.txt).

The existing benchmark baseline is intentionally retained: production has not
adopted these experimental optimisations yet. Measurements are local and
noisy, especially at short preview sizes; treat the large, consistent changes
and allocation reductions as stronger evidence than small differences in rank.
