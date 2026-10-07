# Phase 4 texture model

Jules chose **separate scalar, vector, domain and colour types**, retaining
`Compose` (2026-10-07). These mutually recursive algebraic types describe values
and typed child edges directly; neither a GADT nor an untyped node graph is
needed. The editor keeps tagged JSON trees and validates every child category.
The format has no mutable connections or node IDs. Named reusable expressions
are a later choice; expressions can already be copied and inspected separately.

`Texture` preserves existing public/API and JSON conveniences. `lowerTexture`
expands them into `ColourField`: scalar-to-ramp maps, domain application,
checker selection and colour composition. Reference evaluation uses this core.
The GPU compiles the typed fields; existing convenience nodes retain specialised
lowering kernels and the tested lazy same-domain turbulence cache. This preserves
old numerical conventions, exact ramp endpoints and opaque-layer skipping.

Version 5 adds typed expressions. Versions 1–4 migrate through their existing
ramp/coordinate steps, followed by a version-only 4→5 migration. Existing example
files are canonically rewritten with version 5; their image goldens do not change.
Shared fixtures test historical migrations against the Haskell parser. New
examples exercise version 5 without pretending the new language existed in v1.

## Values and composition

- `Scalar`: constants, projected/point/angular distance, Perlin, fractal sums,
  arithmetic, remapping, thresholds and domain application.
- `Vector`: constants, current position, scalar components, addition,
  scalar multiplication and domain application.
- `Domain`: translation, inverse Euler rotation, scale, repeat, mirror,
  polar/radial repeat, twist, bend, vector warp and composition.
- `ColourField`: constant colour, scalar ramp mapping, vector RGB inspection,
  checker selection, alpha-over, scalar-mask mixing and domain application.

Masks are ordinary scalar values. Basic arithmetic and scalar mixing arrive in
4.1 because its composition cases need them; additional colour blend modes
remain in 4.7. A mask can gate ridge detail, define a contiguous cloud silhouette,
replace a knot interior, or attenuate a displacement independently of colour.
Scalar mix clamps its mask to [0,1] and interpolates straight RGB and alpha;
alpha-over retains its previous semantics. Threshold uses cubic smoothstep,
including reversed thresholds; equal thresholds make a hard step. A degenerate
remap returns its output-low value. Arithmetic and remap do not clamp.

## Coordinates and order

Domain maps convert sampling coordinates into another coordinate system.
`Compose first second` means **apply first, then second**:

```haskell
domainField (Compose first second) p = domainField second (domainField first p)
```

This agrees with `ScalarDomain first (ScalarDomain second source)`. Translation
subtracts its offset. Rotation uses degrees and undoes Z, then Y, then X. Scale
divides each coordinate; zero collapses that axis. Repeat uses centred modulo
cells, with nonpositive periods disabling that axis. Mirror folds selected axes
(selection >= 0.5) around a centre. Polar repeat folds XY into equal angular
sectors; radial repeat wraps XY radius while retaining angle. Both preserve Z
and define their origin explicitly to avoid undefined atan/division.

Twist rotates XY around Z by an angle proportional to height; bend rotates XY
by an angle proportional to X. Both are coordinate deformations, use degrees
per unit and expose a centre. They are not physical rigid-body transformations.
A vector domain application changes where the components are sampled; it does
not rotate the resulting vector. Warp adds `amount * vectorField(p)` to `p`.
Scalar-scaled vectors allow local attenuation. Nested warps and `Compose` permit
repeated displacement with each successive field sampled in its current domain.

## Noise combinators and bounds

Fractal and absolute-fractal (turbulence) sums accept **any scalar source**.
The established octave rotations and frequency progression are retained. fBm
also offsets each octave, shapes each sample by the selected style, normalises,
then applies the legacy artistic contrast. Absolute-fractal sums absolute centred
samples without octave offsets or contrast. Nonpositive amplitude sums preserve
the established fallback values; the reference uses at least one octave.
The GPU permits up to 32 octaves and bounds nested expressions to 200 nodes,
64 levels and 4096 expanded noise samples per point. These are explicit browser
resource limits, separate from document semantics and slider ranges. New noise
sources in 4.5/4.6 can use the existing combinators.

## Editing and inspection

Tree traversal follows typed child edges. Type menus, wrapping, unwrapping,
swapping and deletion respect the selected category. Ramp-reference traversal
also visits typed expressions, retaining copy/rename/import behavior.
“Inspect selected field” previews a scalar through greyscale, a vector with
components mapped from [-1,1] to RGB, or a domain as transformed position RGB.
PNG export follows the displayed inspection; turning inspection off returns to
the complete material. Intermediate masks and details can therefore be tuned,
rendered and exported separately without replacing the root document.

## SDF fields (4.4)

Sphere, box, capped Y-cylinder, Y-torus and plane are signed scalar distances:
negative inside, zero on the boundary. Haskell calls `Geometry.distance`; the
GPU shares parameterised primitive kernels with its geometry compiler. Domains
can transform any primitive. Union is minimum, intersection maximum, difference
maximum of A and negated B. Each has an editable smoothing radius: nonpositive
means hard; positive uses the same polynomial smooth minimum as geometry,
with sign changes for intersection/difference. These operators accept arbitrary
scalar children, including deformed or noise-modulated distances. Such fields
are material inputs, not new raymarch solids or guaranteed tracing bounds.
Ramps can repeat outside and inside zero to create contours and surface bands.

## Cellular and Voronoi fields (4.5–4.6)

`worley` is a scalar with selectable F1, F2 or F2−F1 (`gap`) and Euclidean,
Manhattan or Chebyshev distance. All feature-point distances are in domain
units and are not normalised; use remap/ramp modes to choose their presentation.
There is one feature point in each unit lattice cell. Jitter interpolates from
the cell centre to its seeded random point, clamped to [0,1]. Dimensions must
be 2 or 3: 2D ignores Z and extrudes through the preview solid; 3D varies on all
axes. Domains control scale, rotation, repetition and warping as for other fields.

Seeds are unsigned 32-bit integers, including 0 and 4294967295. Haskell Word32
and GLSL uint implement the same overflow arithmetic and avalanche hash of the
seed and signed integer cell coordinates. Random channels use the low 16 bits
of independently salted hashes, divided by 65536. Seeds travel in two 16-bit
uniform components so an FP32 parameter texture does not discard high bits.
Exact distance ties choose the first cell in ascending X/Y/Z search order.
Like existing procedural noise, browser calculations use FP32; extreme sampling
coordinates are not suitable for fine spatial detail.

Voronoi projections always use Euclidean cells. `cell-id` is a **vector** of
integer lattice coordinates of the winning feature point, preserving spatial
identity instead of compressing it into a collision-prone scalar. It can feed
vector operations or a warp. `cell-value` is a scalar random value in [0,1)
per cell. `cell-colour` is a vector of independent seeded channels in [-1,1],
so the existing vector-to-colour map produces RGB in [0,1). The same dimensions,
jitter and seed select the same cells across all projections; changing domain
coordinates changes their sampling, not their type. A new cell expression
category is unnecessary for these stateless projections; mutable connections
and reusable named fields remain outside this phase.

`cell-edge` is the true Euclidean distance to the closest Voronoi boundary,
computed from feature-point bisectors; it is distinct from F2−F1. Even a site
that is not the second-nearest can define the closest boundary. The nearest
search covers a radius-three lattice neighbourhood (F2 <= 3 for these metrics).
The edge search begins with adjacent-site bisectors, then uses an adaptive
radius bounded by six. A competitor at distance greater than F1 + twice the
current edge distance cannot improve the result. Unit-cell lower bounds prune
candidates before hashing; exhaustive larger-neighbourhood tests check both
searches. Browser work accounting conservatively counts up to 49/343 candidate
visits for 2D/3D cellular samples and 227/2567 for edge distance, within the
existing 4096 per-point budget. Octaves multiply this cost just as for Perlin.

## Colour blending (4.7)

Field arithmetic and scalar masks arrived with 4.1. A new `blend` colour node
adds normal, multiply, screen, overlay, soft light, darken, lighten, difference
and exclusion, with source/backdrop children and opacity. Its separable RGB
formulas and source-over alpha follow [W3C compositing](https://www.w3.org/TR/compositing-1/).
Inputs and opacity are clamped; RGB is evaluated in the editor's existing RGB
space. Only the overlapping alpha-weighted area uses the blend formula.
Zero output alpha produces transparent black, and zero opacity skips the source.
The legacy `layer` node preserves its previous conventions. Mode/opacity are
uniform edits. Eleven examples compare every mode and compose blends with
SDF masks, cellular pigments and existing fractal grain.

PNG downloads encode straight-alpha RGBA directly with lossless deflate and
PNG chunks. A Canvas2D round trip would quantise translucent RGB through
premultiplication. Interactive rendering still performs no readback or PNG
encoding; export and golden harnesses explicitly request it.

## Precomputed reaction–diffusion (4.8)

`reaction-diffusion` is a scalar field backed by a deterministic Gray–Scott
simulation, rather than by independent evaluations at each point. Its explicit
parameters are voxel resolution, iteration count, feed and kill rates, U/V
coefficients, time step, seed and initial state. U and V are two projections of
one shared simulation. Resolution is 8–64, iterations 0–4096, with at most
64 million voxel updates per volume. Chemistry and time-step controls are
bounded to keep the forward Euler update practical; concentrations clamp to
[0,1]. Zero iterations exposes the initial state.

The cubic lattice uses periodic boundaries and the normalized six-axial-neighbour
Laplacian (neighbour mean minus centre). Grid spacing is one lattice unit:
resolution changes the material's detail as well as its cost. Each voxel update,
including intermediates, uses Float32 in both Haskell and TypeScript. The shared
integer cellular hash initializes seeded patches, regular spots or a slab, with
small deterministic concentration perturbations. Fixtures compare every voxel,
including an odd-sized grid and seeds across the full unsigned 32-bit range.
The Gray–Scott equations follow the [MIT model description](https://groups.csail.mit.edu/mac/projects/amorphous/GrayScott/).

Voxel centres are `(i + 0.5) / resolution`; trilinear sampling wraps over a unit
cube in every axis, including negative positions. Existing domains, warps,
fractal sampling, ramps and masks compose with this field normally. Changing
output U/V, colours or domains never changes the simulation key.

The browser runs simulations in module workers, with at most two workers active.
A shared cache coalesces requests and pins arrays while a preview, thumbnail or
export needs them. Completed, unpinned arrays are evicted to an 8 MiB budget;
currently pinned arrays can exceed that budget. Obsolete work is cancelled after
a short grace period so camera changes can reacquire the same pending work.
Generation checks prevent stale results from replacing a newer preview. The
editor stays responsive and shows simulation feedback while waiting.

Each renderer uploads a completed volume once into an RG32F 3D texture and
retains at most four volumes. Manual eight-texel interpolation avoids requiring
float-linear-filter extensions. A material may reference at most four distinct
simulations. Context restoration reuploads retained CPU arrays; exports await
preparation and then use the same rendering path. The Haskell reference keeps a
thread-safe four-volume cache. Workers and emitted assets deploy as ordinary
static files on GitHub Pages; no runtime server is involved.

## Library guidance (4.9)

Documents optionally carry a validated `guide`: role, capability tags, comparison
family, integer display order, inspection hint and representative slice plane.
This is descriptive metadata, independent of the typed expression tree and its
evaluation. It remains a backward-compatible version 5 extension. The seven
browsing categories are exported by Haskell with editor metadata so the gallery
and editor share their labels and ordering; unknown user categories still load.

Families preserve controlled studies without presenting each as a separate
finished material. Recommended planes affect thumbnails and gallery previews;
the user explicitly applies them to the editor viewer. Render caches include
preview parameters but ignore purely textual guidance. Historical migrations
are checked against frozen historical documents, independently of current
library curation. The [library guide](docs/TEXTURE-LIBRARY.md) maps supported
capabilities to representative examples and records intentional golden changes.
