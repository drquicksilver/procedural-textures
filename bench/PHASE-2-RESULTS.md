# Phase 2 scene performance — 2026-10-06

The new scene renderer meets the original preview/full-resolution targets at
its default camera for this 42-case suite: all 96² mean estimates are below
10 ms and all 512² estimates below 100 ms, including PNG encoding. This is
not a guarantee for every material or camera: close zoom increases surface
coverage, and maximum-resolution refinement can take hundreds of milliseconds.

## Default camera

Apple M1 Pro, ten cores, GHC 9.10.3, normal `-O1`; threaded runtime using all
cores. `tasty-bench` measures wall time with benchmarks run sequentially.
Each operation compiles/evaluates the material, traces and shades the image,
and encodes PNG. Startup, HTTP/query parsing and document/ramp resolution are
excluded. No gallery rendering, browser tests or builds run during timing.
Camera: yaw 0.55 rad, pitch 0.35 rad, distance 2.1; fixed 40° FOV.

512² scene plus PNG estimates, in milliseconds:

| Shape | Checker | Marble | Cumulus |
|---|---:|---:|---:|
| sphere | 15.9 | 58.4 | 57.9 |
| cube | 19.4 | 77.5 | 80.5 |
| cylinder | 17.5 | 52.3 | 61.1 |
| torus | 14.5 | 31.6 | 48.3 |
| bitten-cube | 21.4 | 73.9 | 95.4 |
| cut-sphere | 18.3 | 53.5 | 56.2 |
| cut-cube | 21.9 | 75.7 | 81.4 |

Across these 21 full-resolution cases: **14.5–95.4 ms**. Across the 21 preview
cases: **2.2–5.0 ms**. The slowest default case is Cumulus on the bitten cube:
95.4 ms with a reported two-standard-deviation spread of 9.8 ms. Its proximity
to 100 ms means individual operations can exceed the target.

## Viewer extremes

The stress suite uses the bitten cube and includes the closest allowed
camera (distance 1.1) and largest allowed image (1024²). Separate fresh
measurements, in milliseconds; ± is the CSV’s **two standard deviations**,
not a confidence interval or worst-case bound.

| Material / camera | Size | Render + PNG |
|---|---:|---:|
| Cumulus / close | 96² | 8.2 ± 2.0 |
| Cumulus / default | 1024² | 335.3 ± 84.9 |
| Cumulus / close | 512² | 188.3 ± 36.1 |
| Cumulus / close | 1024² | 712.0 ± 85.1 |
| Marble / default | 1024² | 289.8 ± 47.1 |

The close-camera preview remains around 8 ms. Full-resolution close views
exceed the historical 100 ms target, and 1024² refinement reaches about
0.7 seconds. Keep the 96² interactive pass and allow 1024² only as settled
refinement: the browser suite verifies that full renders do not start during
orbit/scroll/slice interaction. This preserves image detail on large/retina
frames while movement remains responsive.

Trace geometry first and sample the material only at the surface hit. This
avoids multiplying expensive turbulence evaluations by every ray step.
Bounding-sphere rejection and bounded tracing keep background rays cheap.
The established coordinate inlining, opaque-layer skipping and same-domain
field sharing remain in the 3D evaluator. No `-O2`, PNG compression change,
buffered backend or larger task chunks were adopted.

## Reproduction and limits

```sh
stack bench --ba '--scenes-only -j 1 --stdev 15 --csv out/phase2-scenes.csv'
stack bench --ba '--scene-stress -j 1 --stdev 15 --csv out/phase2-scene-stress.csv'
```

Raw evidence: [default-camera CSV](phase2-scenes.csv) and
[stress CSV](phase2-scene-stress.csv). Columns use picoseconds; divide by 1e9
for milliseconds. The normal default benchmark suite also includes a smaller
selection of 3D scenes. These are new 3D baselines, not ratios against the
old 2D renderer: noise, sampled coordinates, surface coverage and lighting
have changed. The earlier full-library and optimisation evidence is retained.

Coverage is seven shapes × three materials × two sizes, plus five stress
cases. It is not a full 67-material scene sweep and excludes network/browser
latency. Future changes should compare against these files on the same
machine/settings and retain the scene and material golden suites.
