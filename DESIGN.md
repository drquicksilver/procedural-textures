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
