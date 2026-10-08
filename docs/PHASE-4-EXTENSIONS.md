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
