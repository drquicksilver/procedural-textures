# Phase 3 desktop release measurements — 2026-10-06

Apple M1 Pro, macOS; one browser at a time, hardware GPU. Chrome
154.0.8037.57 uses ANGLE Metal, Firefox 156.0.1 reports “Apple M1, or
similar”, Safari 26.6 (21624.4.5.11.5) reports “Apple GPU”. Firefox and
Safari clocks have roughly millisecond precision; zeros are below that
resolution, not free operations. Raw samples, backend/user agent,
conformance results and comparisons are in [phase3-release/](phase3-release/).

Each browser passed 123 material/noise/distance sample cases and all 106
Haskell golden images (67 slices, 39 scenes). PNG policy remains mean
≤0.0001 and maximum ≤0.008; golden images are unchanged.

## Comparable 512² scenes

Default bitten cube, camera distance 2.1, seven warm samples; values in ms.
Completed render time includes submission and a synchronous one-pixel fence.
Readback and PNG encoding are measured separately; normal editor frames
perform neither operation. These are benchmark timings, not asynchronous
GPU timer-query measurements or whole-page frame latency.

| Browser | Material | Warm median | Full readback | PNG encoding |
| --- | --- | ---: | ---: | ---: |
| Chrome | Checker | 0.8 | 0.7 | 2.6 |
| Chrome | Marble | 6.0 | 0.8 | 4.7 |
| Chrome | Cumulus | 7.0 | 0.9 | 3.7 |
| Firefox | Checker | 1.0 | 0.0 | 1.0 |
| Firefox | Marble | 4.0 | 1.0 | 4.0 |
| Firefox | Cumulus | 5.0 | 0.0 | 2.0 |
| Safari | Checker | 1.0 | 0.0 | 4.0 |
| Safari | Marble | 5.0 | 1.0 | 6.0 |
| Safari | Cumulus | 6.0 | 1.0 | 6.0 |

[Phase 2 CPU results](PHASE-2-RESULTS.md) measured render plus PNG at 21.4 ms
Checker, 73.9 ms Marble and 95.4 ms Cumulus. Chrome's corresponding warm
render + readback + encode sums are 4.1, 11.5 and 11.6 ms, approximately
5×, 6× and 8× faster. These sums estimate a warm export pipeline; they
exclude first compilation, scheduling, downloads and cold encoder startup.

## Stress, refinement and cold programs

Chrome's default 1024² Marble and Cumulus medians are 10.0 and 13.8 ms;
Firefox 8 and 10 ms; Safari 9 and 13 ms. Close-camera Cumulus at 1024²
costs 26.2/21/26 ms respectively, so full resolution cannot always meet a
15 ms interaction budget. At 512² the same close view costs 9.5/6/9 ms.
The adaptive controller therefore chooses resolution from measured warm
cost, with 15% headroom, immediate reductions and gradual increases.
It starts at full resolution and settles to 1024² after interaction stops.

Knight/Marble at 512² costs 7/4/8 ms on Chrome/Firefox/Safari. Across the
33 recorded cases, maximum first render is 556/1222/533 ms; maximum explicit
compile/link time is 203.5/13/138 ms. First render includes driver work
that can be deferred past linking; Firefox's 1.22-second noise-heavy first
render misses the original provisional one-second goal. This remains a
known limitation, rather than being hidden by a looser claimed result.
Cold compilation is excluded from the resolution estimator and may block
interaction when a new structure is selected.

160 structural edits, cycling through 16 different nested-layer structures,
retain at most eight programs. At the end: eight programs, zero shaders,
three textures, one framebuffer and one vertex array. Disposal releases all
tracked objects in all three browsers. The shared editor prioritizes the
viewer/export over thumbnails; thumbnails use bounded caching and deferred
work. Editor browser tests check numeric/camera program reuse, queued work,
context restoration, explicit-only readback/encoding and transparent export.

## Support policy and remaining coverage

The desktop release supports WebGL2 with hardware acceleration in the
recorded Chrome, Firefox and Safari versions. Software Chrome/SwiftShader
is a correctness target in CI, not an interaction-performance promise.
Without WebGL2 the editor explains the problem and keeps document editing,
saving and JSON export available; PNG rendering needs WebGL2. A second
browser CPU renderer is not justified by the desktop evidence.

Actual editor smoke checks cover examples, all 13 shape options, editing,
undo/redo, reload/persistence, JSON import/export, transparent 256² PNG export
and a 390px layout in Chrome, Firefox and Safari. The full Chrome secondary
suite additionally exercises all shapes, slices/handles, libraries, migrations,
storage failure, GPU recovery and export. Phone-sized Chrome emulation checks
layout/editing; it is **not a physical-device performance measurement**.
Physical Android and iOS performance/context testing remains outstanding.
Mobile browsers are best effort until those checks are recorded; the
Phase 3 mobile validation requirement has not been silently declared met.

## Reproduction

```
stack build
stack test
npm --prefix frontend ci
npm --prefix frontend run gpu:browser
npm --prefix frontend run gpu:compatibility -- chrome
npm --prefix frontend run gpu:compatibility -- firefox
npm --prefix frontend run gpu:compatibility -- safari
npm --prefix frontend run gpu:compatibility -- safari --ui-only
```

The runner installs missing pinned Chrome/Firefox revisions into
`out/gpu-browser`. Safari
runs the self-testing page in the installed macOS browser and posts results
back to the local runner; enabling WebDriver remote automation is unnecessary.
Repeat `--ui-only` for Chrome/Firefox as well. Reports and PNGs are written
under `out/compatibility/<browser>/`; these are secondary tests, not part of
fast unit tests. Local runs use hardware by default.
