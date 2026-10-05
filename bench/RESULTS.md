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

Target for interactive editing (set in milestone 1.6, met): on this machine a
full-resolution 512² render of the most expensive example (marble), including
PNG encoding, under 100 ms, and a 96² low-resolution preview under 10 ms.

`bench/baseline.csv` holds the latest numbers (below). Compare a change against them
with:
```
stack bench --ba '--baseline bench/baseline.csv'
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
