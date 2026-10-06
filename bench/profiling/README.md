# Focused texture profiling

The [2026-10-06 analysis](../PROFILING-2026-10-06.md) profiles the ten slowest
512² PNG cases in the 67-example baseline. This developer probe is separate
from the fast tests and the default benchmark suite.

From the repository root:

```
stack build
python3 bench/profiling/run.py --out out/profiling
```

Run on an otherwise idle machine. The recorded study uses an Apple M1 Pro
with ten cores; the normal measurements explicitly use `-N10`. Change that
argument in the runner for other machines and record the new environment.
The current command collects normal-stage measurements and focused
cost-centre profiles. No production source, examples, goldens or original
baseline files are modified. Experiment files and binaries live in a temporary
directory; measured results go to `--out`.

The normal probe links the existing built library. It measures rendering,
encoding a prepared image, and their combination, at 512² and 96². Each result
is an average per operation in one batch; use the median of five batches.
Images are forced before encode-only measurement. Arguments are read from an
IORef each iteration to prevent sharing the pure operation's result. Warm-up
and document loading/resolution are outside the interval. The final collection
is included in wall/CPU time, so allocation and GC statistics cover the same
interval. Twenty iterations per batch at 96² amortize cleanup and scheduling
overheads; the 512² default is three, with twenty for the more variable Wood
Knot render-only case. The CSV records actual counts.

The focused profiler exports **resolved constructors from the current JSON**,
then compiles the evaluator modules (including the 3D vector helpers) with `-O1 -prof -fprof-late`. These
cost centres are added after optimisation. Its only source adaptation replaces
Texture's import of the `ImageFn` synonym with the identical local synonym,
avoiding a profiling rebuild of image/server dependencies. It sums all RGBA
channels over three 512² grids on one CPU. This excludes quantisation, image
assembly and PNG encoding; instrumented execution time is not a latency
benchmark. The generated checksum driver contributes its own reported time
and allocation. Raw `.prof` files are retained under the output directory.

The historical optional experiments (before their adoption) rebuilt the evaluator at `-O1` without profiling:

- `base`: no evaluator changes.
- `inline`: add `{-# INLINE transformOctave #-}`.
- `opaque`: also return the top colour immediately when its alpha is exactly
  one, without evaluating the bottom texture. Other cases use the original
  blending implementation.

The same normal renderer and PNG encoder are used in all three builds. Each
variant is checked against the production evaluator's complete 512² RGBA
buffer for all ten examples before timing. This is evidence for the sampled
examples, not a replacement for full golden and compositing edge-case tests
when adopting an optimisation. The original baseline is not overwritten.

For an individual normal measurement, build the probe independently:

```
mkdir -p /tmp/texture-profile
stack exec ghc -- -O1 -Wall -threaded -rtsopts -package procedural-textures \
  -outputdir /tmp/texture-profile bench/profiling/Profile.hs \
  -o /tmp/texture-profile/probe
/tmp/texture-profile/probe cumulus combined 512 3 5 +RTS -T -N10 -RTS
```

Output columns are example, stage, size, batch, iterations, wall milliseconds,
CPU milliseconds, GC CPU milliseconds, allocated decimal MB, and collections
per iteration. Allocation is cumulative temporary allocation, not resident
memory. CPU time sums across cores. The focused profile CSV contains GHC's
reported *individual* time/allocation percentages for its main cost centres;
minor centres below GHC's reporting threshold are omitted.


Phase 2: normal measurements/profile exports now use the current 3D evaluator’s
z=0 slice and include `Vector3` in the isolated build. The `--experiments` flag
is intentionally refused on the already optimised evaluator; reconstruct the
recorded independent worktrees to reproduce that historical comparison.
Scene latency is covered separately by `stack bench --ba '--scenes-only -j 1'`.
