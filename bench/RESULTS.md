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

`bench/baseline.csv` holds the "after" numbers. Compare a change against them
with:
```
stack bench --ba '--baseline bench/baseline.csv'
```
