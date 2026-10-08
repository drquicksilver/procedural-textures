# Aggregate review of main — 8 October 2026

Scope: the ten commits from `eb7bb4b` through `6949414`, inclusive, reviewed as one aggregate change.

The changes complete Phase 4 extensions to the existing typed scalar, vector, domain and colour-field system: scalar mathematics, native periodic noise, bounded scatter, periodic layouts, field-driven rotation, prepared branching, field-driven reaction–diffusion and differential fields. They extend the Haskell reference evaluator, JSON/schema-driven editor and independent browser renderer together. Reaction preparation also introduces a browser CPU evaluator for typed input fields.

The implementation generally fits the existing architecture and includes substantial analytical, shared-fixture and golden coverage. Four defects should be fixed. Findings are ordered by priority; all have high confidence and were reproduced with targeted checks. No optional redesign recommendations are included.

## 1. [P2] Preparation work accounting can overflow and bypass the limit

Locations: `src/Texture.hs:518`, `src/Texture.hs:549`, `src/TextureJson.hs:682`.

Expanded work counts use `Int`. The parser converts the result to `Integer` only after recursive multiplication and summation have already occurred. Twenty-five nested gradient/component pairs produce a negative work count, `-8463200117489401856`, on the tested 64-bit platform. A field-reaction node containing that input passes the parser's eight-million-evaluation check.

The work limit therefore fails for deeply nested input expressions. Expensive inputs can enter preparation and stall rendering instead of being rejected. The targeted check confirmed the negative count and successful parsing; it did not execute the excessive preparation.

**Recommended fix:** use `Integer` throughout scalar, vector and domain work accounting, including the final sum, or saturate additions and multiplications above the supported budget. Add a parser regression for deeply nested differential inputs.

**Confidence:** high; directly reproduced.

## 2. [P2] Scatter density unintentionally biases placement within every cell

Locations: `src/Texture.hs:35–40`, `frontend/src/gpu/compiler.ts:190–195`.

`C.feature` uses `rx` for a site's fractional X coordinate. Scatter reuses the same `rx` for acceptance against the density field. Constant density 0.25 consequently accepts centres only in each cell's left quarter, creating a repeating spatial bias instead of uniformly thinning the sites. A probe over 1,000 cells found 241 accepted sites; their maximum fractional X coordinate was `0.24945068359375`.

The same coupling makes radius correlate with a site's Y position through `ry`. The CPU and GPU agree with one another, so ordinary parity tests do not reveal the problem. The added numerical scatter fixtures use densities zero and one, which cannot expose the acceptance bias.

**Recommended fix:** derive acceptance and motif controls from a separately salted hash, consistently in Haskell and GLSL. Add a deterministic intermediate-density test for spatial bias. Any affected goldens should be updated deliberately with the correction documented.

**Confidence:** high; follows directly from the random-value reuse and was confirmed by the site probe.

## 3. [P2] Worker-side normalisation changes legacy field semantics

Location: `frontend/src/fields-cpu.ts:13`; callers include the `angular` and `plane` cases.

The new browser CPU evaluator normalises a zero vector to zero. Haskell's existing `Vector3.normalise` instead returns `(0,0,1)` for lengths below `1e-12`. Valid plane and angular fields therefore produce different reaction inputs across implementations.

For a zero-normal plane used as a 3D reaction seed field, with resolution eight and zero iterations, Haskell's first voxel is `(U,V)=(0.96875,0.015625)`; the browser produces `(1,0)`. A zero-axis angular field at `(0,1,1)` evaluates to `1` in Haskell and approximately `0.8535533905932737` in the browser CPU evaluator. These differences occur before simulation and can substantially change subsequent concentrations.

The worker conformance fixtures exercise typed defaults, which do not cover these degenerate vectors.

**Recommended fix:** reproduce the existing reference normalisation semantics for legacy plane and angular fields. Preserve the explicit zero-safe behavior of the new `normalise-vector` node. Add zero and sub-threshold vector fixtures and a reaction-input regression.

**Confidence:** high; field evaluations and the initial reaction voxel were directly compared.

## 4. [P2] Small nonzero rotation axes select the wrong axis in Haskell

Location: `src/Texture.hs:332–337`.

`RotateField` checks for an exactly zero axis, then normalises through the legacy helper. That helper substitutes Z whenever the axis length is below `1e-12`. A small nonzero X axis therefore rotates about Z in Haskell while the browser CPU evaluator and GPU rotate about X.

With axis `(1e-13,0,0)`, angle 90 degrees, pivot zero and input point `(0,1,0)`, Haskell returns approximately `(1,0,0)`. Browser CPU and GPU return approximately `(0,0,-1)`. Changing only an axis's magnitude should preserve the rotation, but currently changes its direction in the reference implementation.

**Recommended fix:** normalise nonzero rotation axes independently of the legacy fallback, preferably scaling by the largest absolute component first. Keep the explicit zero-axis identity behavior. Add CPU/worker/GPU fixtures showing that rescaling a nonzero axis preserves the result.

**Confidence:** high; reproduced in Haskell, browser CPU evaluation and a headless GPU check.

## Verification and overall assessment

All execution and temporary probe files were isolated in `/private/tmp/procedural-textures-review-20261008`. The active working directory was left untouched. No tracked source or golden files were changed.

Passed:

- `stack build`.
- `stack test`: all 1,236 tests.
- Frontend `npm test`: all 1,040 Vitest tests and four cache checks.
- Frontend `npm run build`.
- Targeted Haskell and browser CPU probes for work-count overflow, degenerate fields, reaction initialization and scatter placement.
- Targeted headless GPU checks for rotation-axis scaling.

The full browser conformance and editor E2E suites were not rerun. The main coverage gaps exposed by this review are overflow in recursive work accounting, intermediate-density scatter behavior, degenerate vector semantics and axis-magnitude invariance. The broad existing suites pass, but these targeted cases show that the aggregate change still needs corrections before acceptance.
