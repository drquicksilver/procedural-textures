# Benchmark results

Wall-clock times from `stack bench` (tasty-bench), in milliseconds.

Machine: Apple M1 Pro, 10 cores, GHC 9.10.3, 2026-10-05.

"Before" is the renderer at the start of milestone 1.6 (sequential; the
numbers are CPU time, which equals wall-clock time for sequential code).
"After" adds three changes, each verified byte-identical against the golden
images:

1. Ramps are compiled once per texture node instead of re-sorting their stops
   for every pixel, and the stop lookup is a single allocation-free scan.
2. Perlin noise uses an unboxed permutation table with unchecked indexing,
   and turbulence computes its normalisation once per node.
3. Rows are rendered in parallel (on all cores).

| Benchmark | Before | After |
|---|---:|---:|
| render 256.checker | 2.4 | 1.8 |
| render 256.clouds | 69.7 | 6.4 |
| render 256.gradient | 18.7 | 1.5 |
| render 256.layered | 43.0 | 3.7 |
| render 256.marble | 190.1 | 16.4 |
| render 256.radial | 19.0 | 1.7 |
| render 256.redgreensaw | 20.4 | 1.8 |
| render 256.redgreensine | 12.6 | 2.0 |
| render 256.rings | 20.0 | 1.8 |
| render 256.smiley | 179.3 | 8.6 |
| render 256.stripes | 27.5 | 1.8 |
| render 256.sunset | 82.7 | 4.6 |
| render 256.swirly-stripes | 127.2 | 12.5 |
| render 256.wobbly-stripes | 59.7 | 5.9 |
| render+png.marble 96 | 29.6 | 5.0 |
| render+png.marble 256 | 190.3 | 27.0 |
| render+png.marble 512 | 702.7 | 84.0 |

Target for interactive editing (set in milestone 1.6, met at that milestone): on this machine a
full-resolution 512² render of the most expensive example (marble), including
PNG encoding, under 100 ms, and a 96² low-resolution preview under 10 ms.

`bench/baseline.csv` holds the latest full-library numbers (see the final section).
Compare the default representative suite against them
with:
```
stack bench --ba '-j 1 --baseline bench/baseline.csv'
```

To regenerate all render and PNG measurements, rather than replacing the full
baseline with the smaller default suite:
```
stack bench --ba '--full-library -j 1 --stdev 15 --csv bench/baseline.csv'
```

## After milestones 1.12–1.16 (2026-10-05)

Same machine. Many more examples, plus three changes that affect speed:

- **1.14, noise without grid artefacts.** Unit gradients from a table and a
  rotation per octave made noise-heavy textures about 20–40% slower; marble
  at 512² went from 84 ms to about 128 ms. Precomputing each octave's
  rotation and frequency as one matrix, and taking `floor` once per
  coordinate, recovered most of that. (`-O2` was tried and made no
  measurable difference.)
- **1.16, OKLab blending.** Measured separately: no significant cost,
  because stops are converted to OKLab once per ramp.
- PNG encoding of noisy images is a large share at 512² (about 28 ms of
  marble's total).

**Target status:** marble at 512² including PNG encoding takes about 103 ms
(between 103 and 118 ms over several runs), just over the 100 ms target.
The 96² low-resolution preview used while editing is unaffected at about
5 ms.

| Benchmark | Now |
|---|---:|
| render 256.agate | 10.3 |
| render 256.aurora | 13.7 |
| render 256.autumn | 6.7 |
| render 256.bark | 11.5 |
| render 256.brushed-gold | 4.4 |
| render 256.camouflage | 6.0 |
| render 256.caustics | 10.8 |
| render 256.checker | 2.0 |
| render 256.clouds | 9.8 |
| render 256.contours | 14.0 |
| render 256.cumulus | 9.7 |
| render 256.dunes | 12.4 |
| render 256.embers | 8.4 |
| render 256.fire | 9.1 |
| render 256.gradient | 2.6 |
| render 256.granite | 6.4 |
| render 256.heatmap | 8.1 |
| render 256.ice | 12.0 |
| render 256.lava | 14.4 |
| render 256.lawn | 5.9 |
| render 256.layered | 4.3 |
| render 256.marble | 19.2 |
| render 256.moss | 13.7 |
| render 256.mountains | 8.9 |
| render 256.ocean | 7.3 |
| render 256.pine | 11.6 |
| render 256.plaid | 4.6 |
| render 256.plasma | 16.8 |
| render 256.radial | 2.3 |
| render 256.redgreensaw | 2.6 |
| render 256.redgreensine | 2.6 |
| render 256.rings | 3.2 |
| render 256.rust | 10.0 |
| render 256.sandstone | 10.9 |
| render 256.smiley | 9.7 |
| render 256.storm | 16.6 |
| render 256.stripes | 2.3 |
| render 256.sunset-clouds | 10.0 |
| render 256.sunset | 4.4 |
| render 256.swirly-stripes | 15.4 |
| render 256.terrain | 9.8 |
| render 256.verdigris | 9.4 |
| render 256.walnut | 10.5 |
| render 256.wobbly-stripes | 7.0 |
| render+png.marble 96 | 5.0 |
| render+png.marble 256 | 28.9 |
| render+png.marble 512 | 103.3 |

## After the 67-example gallery expansion (2026-10-05)

Same machine and GHC version. All 67 examples were measured at 256² without
encoding, and at 96², 256² and 512² through `Server.renderPng`: 268 cases in
total. The sweep passed and took about six minutes. No renderer optimisations
or texture changes were made for this measurement.

The command above uses wall-clock timing, all cores within each render, and
one benchmark at a time (`-j 1`). Browser tests and builds were finished before
measurement. The full sweep uses `--stdev 15` for a practical survey; the CSV
records both the mean and twice the measured standard deviation, in
picoseconds (divide by 1e9 for milliseconds). These are local measurements,
not precise regression thresholds. The PNG timings include rendering and
encoding, not HTTP transport or browser display.

| Texture | PNG 96² | PNG 256² | PNG 512² | 512² ± twice standard deviation |
|---|---:|---:|---:|---:|
| Checker | 0.6 | 4.0 | 7.3 | 1.2 |
| Marble | 6.8 | 37.0 | 136.9 | 39.8 |
| Cumulus | 11.9 | 70.8 | 295.9 | 19.4 |
| Mossy Stone | 8.4 | 56.0 | 209.9 | 33.8 |
| Rust | 9.5 | 54.8 | 209.8 | 34.2 |
| Ice | 7.3 | 40.7 | 180.4 | 45.7 |

All table values are milliseconds. Cumulus is now the slowest 512² PNG case,
followed by Mossy Stone and Rust (effectively tied), then Ice. Water Ripples
and Tiger Fur follow at about 153 ms and 149 ms. The default benchmark suite
retains Checker as a simple control and Marble for historical comparison,
and adds the four slowest full-resolution cases. It still measures all 67
render-only cases; `--full-library` expands the PNG portion from 18 to 201
cases.

**Target status:** the original library-wide goals of under 10 ms at 96² and
under 100 ms at 512², including PNG encoding, are no longer met. In this
sweep, Cumulus was the only 96² mean above 10 ms (and remains close to that
boundary), while 27 of 67 full-resolution means exceeded 100 ms. The larger
gallery compositions increased evaluation cost; the historical Marble
measurements above do not describe the current examples.

The next performance investigation should profile Cumulus, Mossy Stone,
Rust and Ice, separating field evaluation from PNG encoding and checking
whether repeated domain/field work can be shared. Preserve the intentional
macrostructure and goldens while doing so. This measurement establishes the
priority; it does not claim an optimisation has already been implemented.

The [2026-10-06 focused profiling study](PROFILING-2026-10-06.md) now separates
rendering and encoding for the ten most expensive examples. Temporary,
pixel-verified octave-transform inlining and opaque-layer experiments improve
their 512² PNG path by 1.49–2.46×. Those evaluator changes have not been adopted;
the baseline above still describes production.

The subsequent [six-worktree experiment study](PERFORMANCE-EXPERIMENTS-2026-10-06.md)
tests `-O2`, inlining, opacity, same-domain sharing, tiled evaluation and
scheduling independently. Inlining is the strongest general result; sharing
and opacity help eligible compositions. The buffered backend and coarser task
chunks do not justify adoption. Production and this baseline remain unchanged.

The [combined optimisation benchmark](PERFORMANCE-COMBINED-2026-10-06.md)
now adopts inlining, opaque-layer skipping and same-domain displacement sharing
in production. The ten-example cohort improves by 1.97× for 512² render plus
PNG and 2.41× for rendering alone, with 67.2% less cumulative allocation.
Eight cohort medians are below 100 ms; Mossy Stone and Rust remain above it.
All ten preview medians are below 10 ms. The preserved full-library CSV above
is historical and does not measure the newly optimised evaluator.


The [Phase 2 scene baseline](PHASE-2-RESULTS.md) records the new 3D evaluator
and renderer across 42 default-camera cases plus five viewer stress cases.
Default-camera scene+PNG estimates range from 2.2–5.0 ms at 96² and
14.5–95.4 ms at 512². Close zoom and 1024² refinement are more expensive;
the report quantifies them and explains the interactive/refinement policy.
Historical 2D measurements above are preserved, not reused as 3D comparisons.
