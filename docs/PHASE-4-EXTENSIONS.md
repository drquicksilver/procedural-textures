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

## 4.13 Bounded seeded scatter

Scatter owns a unit disc (2D) or sphere (3D) in local motif coordinates.
Site positions use the existing unsigned 32-bit cellular hash. Density is clamped
and sampled at each site, rather than varying across a mark. Independent hash
components determine acceptance, radius and XY rotation in degrees. Radii are
positive, ordered and at most 0.75 lattice units; a fixed 3×3 (2D) or 3×3×3 (3D)
neighbourhood therefore encloses every possible mark. Negative cells use floor.
Highest unsigned hash owns overlaps; hash ties retain lexicographic search order.
Only the owner's motif is evaluated, once. Its transparency reveals the external
background rather than another scattered mark. This is ownership, not an
unbounded stack of alpha-composited marks. 2D scatter ignores Z throughout.

The expanded GPU work guard counts every candidate density query plus one motif
query, including nested scatter. New compositions demonstrate leaves with local
veins and rotation, overlapping concentric discs, and variable-size 3D inclusions.

Validation: 1,155 Haskell tests, 940 frontend tests plus four cache checks and
production build pass; 503 numerical GPU cases, 251 image comparisons and 29
editor workflows pass. Three new example goldens were intentionally accepted;
existing image goldens are unchanged. Scatter resets coordinate-dependent warp
caches when entering the owned motif, and participates in editor structural keys.

## 4.14 Focused periodic layouts

One shared layout owns each XY point and supplies centred coordinates, an integer
identity, a seeded value and unsigned distance to its boundary. Grid and running
bond use floor, including negative cells; odd rows shift by half a unit. Hexagons
use axial centres (i+j/2, j√3/2), unit centre separation, and a bounded 3×3 nearest
centre search. Equal distances retain lexicographic search order. Their inradius
is 0.5. Herringbone uses an exact periodic domino tessellation of unit squares:
(i−j) mod 4 selects vertical/horizontal anchors. Local coordinates align the long
axis with Y, with half extents (0.5,1). Z passes through all local-coordinate
projections and does not affect ownership or edge distance. IDs have Z=0.

Layouts operate in lattice units; domain scaling supplies world units. Projections
share identical ownership, keeping grout and motifs aligned. The bounded GPU work
guard charges nine candidate queries for hexagons, one for the other layouts.
New examples show grid/bond inlays, aligned herringbone wood grain and hexagonal
medallions. The two former fixed prototypes now use native edge projections;
herringbone grout corners deliberately follow exact polygon boundaries rather
than the old rounded outside-box SDF joins. Arbitrary parquet remains deferred.

Validation: 1,177 Haskell tests, 948 frontend tests plus four cache checks and
production build pass. All layout combinations pass numerical GPU conformance,
as do 255 gallery/scene image comparisons. Four new example goldens and the
herringbone prototype's polygon-grout golden were intentionally accepted;
the simplified honeycomb image remains byte-identical to its existing golden.
All 29 editor workflows pass. The final identity-driven bond inlay also passes
separate CPU/GPU comparisons in XY/XZ/YZ and the solid view.

## 4.15 Field-driven orientation

`rotate-field` inverse-rotates sampling coordinates around a fixed axis and an
explicit pivot. Its scalar angle is in degrees, evaluated once at the incoming
point, before rotation. Rodrigues' formula normalises the axis; a zero axis is
identity and skips angle evaluation. Domain composition keeps First-then-Second
semantics. This rotates where a texture is sampled, with no surface frame or
lighting interpretation. Expanded work includes the full angle-field query.

New subjects are noise-oriented hatching, distance-driven radial ribbons and a
volume rotated about an oblique axis with a depth-dependent angle. General 3D
frames and Gabor noise remain separate future work.

Validation: 1,191 Haskell tests, 952 frontend tests plus four cache checks and
production build, 540 numerical GPU cases, 258 image comparisons and 29 editor
workflows pass. Three new image goldens were accepted intentionally; existing
goldens are unchanged. Analytic cases cover constant rotations, an oblique axis,
zero-axis short-circuiting, the fixed pivot and incoming-point angle sampling.

## 4.16 One bounded branching field

Choose a seeded binary tree with a shrinking segment length (×0.65), symmetric
branch spread with bounded hash jitter, and a multiplicative radius taper.
A 3D variant adds bounded depth-direction jitter. Depth 1–7 gives at most 127
segments. Roots start at (0.5,0.06,0/0.5); this is texture-space structure, with
no mesh growth. Distance is a signed tube envelope using closest centreline
position and interpolated radius; it is not an exact tapered-tube SDF.

Haskell prepares the network once per compiled field closure. The browser packs
segments into a fixed 127-segment parameter layout, so numeric hierarchy/seed
edits reuse shader source. The query loop is bounded and expanded-work accounting
charges every segment. Preparing at most 127 segments is small synchronous work,
so a separate worker/cancellation pipeline would add overhead here; long-running
reaction simulation continues to use the existing worker cache. New compositions
show gilded dendritic ink, masked leaf veins and a branching 3D root volume.

Validation: 1,205 Haskell tests, 959 frontend tests plus four cache checks and
production build, 544 numerical GPU cases, 261 image comparisons and 29 editor
workflows pass. Three new goldens were deliberately accepted; existing goldens
are unchanged. Tests enforce hierarchy bounds and seed determinism, check taper
and 2D extrusion, and ensure depth/seed/dimension edits change parameter data
without changing shader source. The 3D model uses restrained depth jitter (0.2)
so RGB slices can reveal its network; the root composition uses thicker branches
and a centred depth repeat. Leaf veins and ink retain fine tapering structures.

## 4.17 Selective reaction–diffusion extensions

Add `field-reaction` without changing the original reaction node. Sample three
scalar inputs at voxel centres, clamp the initial mask to [0,1] and feed/kill to
[0,0.1], then convert each to Float32 once. Initialise U=1−0.5×mask and
V=0.25×mask. Chemistry fields stay fixed during growth. Inputs can use any typed
scalar/vector/domain composition, including completed reaction dependencies.
Worker-side Double field evaluation is tested against Haskell before conversion;
it avoids UI-thread sampling and GPU float errors in chaotic simulation inputs.
Dependencies are finite nested expressions, evaluated on demand in postorder and
memoised within preparation. At most four distinct prepared dependencies are
allowed. Changing any input or its upstream simulation changes the full canonical
cache key; changing output U/V, colour, view or downstream coordinates reuses it.
The existing bounded worker cache handles deduplication, cancellation and eviction.

The solver remains periodic regardless of input-field periodicity. Nonperiodic
seed or chemistry fields can create a conspicuous wrap-region transition even
though sampling remains periodic; use periodic inputs for intentional seamless
materials. Diffusion coefficients/time step
retain their [0,1] stability bounds, with concentrations clamped each update.
Non-finite field inputs become zero. This is a discrete bounded Gray–Scott model,
not a promise of continuous-physics accuracy.

2D is implemented as a useful subset: four-neighbour averaged diffusion, one
XY plane sampled at Z=0, bilinear periodic sampling and extrusion through Z.
Allow resolution 8–256 for 2D, 8–64 for 3D, 0–4096 steps and at most 64 million
voxel updates. Input preparation separately allows at most 8 million expanded
field evaluations. A 128² completed two-chemical array is 128 KiB (128 times
smaller than 128³); the existing cache limits and two-worker concurrency remain.
The GPU uploads a depth-one volume and manually filters it, avoiding float32
hardware-filtering assumptions. New examples show an analytic spiral seed,
spatial feed/kill regimes and a 128² chemical labyrinth. Very fine seed islands
died out in the initial maze experiment; broader periodic patches survive.

Validation: 1,220 Haskell tests, 1,031 frontend tests plus four cache checks and
production build, 550 numerical GPU cases, 264 image comparisons and 30 editor
workflows pass. Shared worker fixtures cover all 64 non-reaction typed defaults;
small 2D/3D field-driven fixtures compare every Float32 voxel exactly. End-to-end
GPU cases cover both dimensions. Cache tests verify canonical input keys, nested
sampling, input invalidation and compact 2D results. The editor workflow verifies
off-thread spatial chemistry and concentration reuse. Three new goldens and the
new field/solver fixtures were intentionally accepted; existing image goldens and
legacy solver cases are unchanged. The full image sweep was repeated successfully
after an earlier local Chrome target closed during a concurrent browser run.

## 4.18 Gradient/curl fields: accepted after prototype

An existing-primitive central-curl prototype produces useful swirling RGB
filaments, distinct from independent noise displacements. Before adding nodes,
a field-only interpreted CPU probe (M1 Pro, 20,000 points, runghc) took 1,708 ms
for three-noise displacement and 6,927 ms for its explicit curl recipe (about
4.1×). On local Chrome/ANGLE Metal, 20 warmed draw+readback runs had median
1.6 ms versus 2.3 ms at 256², and 3.1 ms versus 2.9 ms at 512². These are local
end-to-end observations including readback and scheduling, not portable isolated
GPU timings or proof that curl is free. First 256² draws, including compilation,
were about 258 ms and 242 ms. The visual contribution and measured steady cost
justify bounded differential operators; browser work guards remain conservative.

`gradient` uses six central scalar samples. `curl` uses six central vector
samples, reusing each sampled vector's components. Step is positive, finite and
0.0001–0.5, in the current sampling coordinate units. Derivatives divide by 2×step;
units are source units per coordinate unit. Expanded work multiplies the entire
source cost by six, including nested differentials and reaction-input preparation.
Differences can be inaccurate for steps below Float32 coordinate resolution or
at large coordinates; use a larger step or bounded local coordinates. The default
step is binary-exact 1/64. Changing step updates parameters, preserving shaders.

`normalise-vector` preserves zero vectors. It guards each component and scales
by the largest absolute component before normalising, avoiding squared-norm
overflow for large finite inputs. Derivative components outside finite FP32 range
become zero. No gradient-normalised distance operator is added: it would only be
a local isovalue-distance approximation, not an exact SDF. These are RGB sampling
and displacement tools, with no normal-map or lighting semantics.

New examples show periodic curl filaments, gradient-guided hatching with bounded
unit displacement, and a 3D gradient-direction colour study. Their periodic local
coordinates and binary-exact scales/offsets keep finite differences well resolved.

The editor regression test caught generic 0.01 slider snapping of the 1/64
sample step to 0.0201. Difference-step metadata now uses 1e-6 increments, and
numeric text/arrow nudging retain precision appropriate to such small steps.
Ordinary controls keep their existing four-decimal display. This preserves the
chosen step during editing and undo rather than silently quantising it.

Validation: 1,236 Haskell tests, 1,040 frontend tests plus four cache checks and
production build, 560 numerical GPU cases, 267 image comparisons and 31 editor
workflows pass. No numerical/image tolerances were loosened. Typed worker input
fixtures now cover all 67 non-reaction defaults, including differentials. Analytic
checks cover linear gradients/curls, zero gradients, large finite normalisation,
invalid steps and nested query budgets. Editor tests verify field inspection,
precise step editing, shader reuse and undo. Three new image goldens and expanded
shared fixtures were accepted deliberately; existing goldens are unchanged.

## Completion

All Phase 4 milestones through 4.18 are complete. This extension sequence added
25 example documents, bringing the gallery to 228, and simplified older recipes
where native primitives made them clearer. Each milestone was committed and
pushed after local validation. CI was checked during development; superseded runs
were cancelled by the repository's concurrency policy, and the completed
orientation, branching and field-reaction runs passed. Phase 5 remains next;
lighting/normal effects and the explicitly deferred research ideas remain future
work rather than unimplemented Phase 4 requirements.
