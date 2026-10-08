# Phase 4 extensions: decisions and validation

## 4.11 Scalar mathematics and coordinates

Angles for `sin`/`cos` are radians. `azimuth` is a true XY atan2 angle in turns
[0,1), with zero on positive X and at the centre; the existing mirrored angular
fan is unchanged. `component` exposes X/Y/Z of a vector without converting it to
colour. Floor rounds toward negative infinity; fract subtracts floor. Clamp sorts
its bounds, and scalar lerp allows extrapolation. Safe divide returns zero for
absolute denominators at most 1e-8. Power supports negative bases only for integer
exponents, defines 0^0 as 1 and returns zero for invalid real powers and results
outside finite FP32 range. Explicit maths guard non-finite results; legacy nodes
retain their existing semantics. These are additive V5 typed nodes, requiring no
reinterpretation or migration of older documents.

New subjects: true-angle guilloche lace, a volumetric analytic crosswave, and
stepped spiral inlay. Digital camouflage uses native floor/component/division;
twill's derived floor and the comb's derived absolute value are simplified.
Reference tests compare the resulting images, with deliberate golden acceptance
only for additions or documented simplification differences. Shared metadata
supplies typed editor controls, constructor fixtures and GPU shader parity.

Validation: 1,126 Haskell tests; frontend suite and production build; 487 numerical
GPU cases, 245 image comparisons and 29 editor workflows pass. Three new goldens
were added intentionally; no existing image golden changed. Browser shader header
ordering has a regression check; GPU browser teardown is bounded after results
are written so driver shutdown cannot stall subsequent suites indefinitely.

## 4.12 Native periodic noise

Each lattice axis has an explicit integer period 1–256. Wrap all corner lattice
coordinates before the existing permutation/gradient hash; local interpolation
and its first derivatives remain continuous. Negative coordinates use positive
modulo. The noise remains 3D and bounded in [0,1]. `periodic-fractal` is a separate
normalised weighted sum with 1–8 octaves, persistence 0–1 and integer lacunarity
1–4. It deliberately has no octave rotations: frequency and the lattice period
both grow by integer lacunarity, preserving the original requested period.
Smooth/billowy/ridged shaping is available; ordinary fractals are unchanged.

Domain scale converts lattice periods to world periods. Warps preserve a period
only when their displacement shares it; rotations/arbitrary nonperiodic warps do
not automatically retain axis-aligned seams. New examples show a raw periodic
cube, ridged agate bands and a period-preserving warped silk composition. Native
noise evaluates one lattice sample, versus four shifted samples in the existing
2D crossfade tile (eight would be needed for full 3D crossfading).

Validation: 1,141 Haskell tests, frontend tests/build, 492 numerical GPU cases,
248 image comparisons and 29 editor workflows pass. Three new goldens are
intentional; existing goldens are unchanged. An interpreted CPU probe on M1 Pro
(runghc, 100,000 unit-cube points) took 3,378.6 ms natively versus 10,717.1 ms
for four-sample crossfading; this is indicative arithmetic cost, not a compiled
GPU performance claim or identical noise realisation.
