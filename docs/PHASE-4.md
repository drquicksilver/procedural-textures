# Phase 4.1–4.8: composable fields and simulated materials

The language now separates scalar, vector, domain and colour expressions.
[Jules's model decision and numerical conventions](../DESIGN.md) describe the
core types, compatibility lowering and composition order. The client remains
entirely static; Haskell generates reference data and renders build-time goldens.

Use **Inspect selected field** while navigating the tree to view a mask or
intermediate field. Scalar inspection uses greyscale; vector inspection maps
[-1,1] to RGB; domain inspection shows transformed position RGB. PNG export
captures that inspection. Turn inspection off to return to the complete texture.
Menus only offer expressions of the selected category. Existing saved documents
migrate to version 5 without changing their rendered image.

## Example guide

All 68 additions have reference image goldens and browser comparisons. Every
new scalar/vector/domain constructor appears in an example, checked by a unit
test. Start with these materials in the editor library:

| Capability | Examples | What to inspect or change |
| --- | --- | --- |
| Scalar arithmetic, remap, smooth/hard masks | [Gated alpine](../examples/gated-alpine.json), [Distance crests](../examples/distance-crests.json) | Broad elevation, ridge detail and the detail-gating threshold; fade crests with distance. |
| Min/max field composition and independent colour mixing | [Cloud lobe union](../examples/cloud-lobe-union.json), [Nested corrosion](../examples/nested-corrosion.json) | The cloud silhouette stays contiguous while its internal shading changes; corrosion uses nested scalar masks. |
| Local masked displacement and complete interior replacement | [Masked knot](../examples/masked-knot.json), [Local current](../examples/local-current.json) | The scalar mask controls grain deflection independently of colour and of the knot's replacement interior. |
| Translation and centred modulo repeat | [Offset medallions](../examples/offset-medallions.json) | The sampling origin and repeat-cell periods. |
| Euler rotation and explicit ordered composition | [Diagonal inlay](../examples/diagonal-inlay.json) | First translate, then rotate, then repeat. Swap domain children to see that order matters. |
| Anisotropic scale | [Stretched enamel](../examples/stretched-enamel.json) | Stretch one axis of the medallion grid. |
| Mirror | [Butterfly fold](../examples/butterfly-fold.json) | Fold selected axes around the centre. |
| Polar repetition | [Polar rosette](../examples/polar-rosette.json) | Number of sectors and the single off-centre petal. |
| Radial repetition | [Radial pearls](../examples/radial-pearls.json) | Ring spacing while retaining angle and depth. |
| Twist | [Twisted brocade](../examples/twisted-brocade.json) | Orbit a solid or move an XY slice through Z to see the height-dependent winding. |
| Bend | [Bent ribbons](../examples/bent-ribbons.json) | Horizontal-position-dependent bending of regular ribbons. |
| Angular scalar plus distance bands | [Swept compass](../examples/swept-compass.json) | Inspect the angular sweep separately from its concentric distance contribution. |
| Arbitrary analytic vector warp, constants and position | [Coordinate lens](../examples/coordinate-lens.json) | Position-derived displacement inside a fading circular mask; no noise source is needed. |
| Vector addition and independently transformed vector sampling | [Crosscurrent silk](../examples/crosscurrent-silk.json) | Two independently sampled flows combined before displacement. |
| Generic fBm source | [Nested fractal frost](../examples/nested-fractal-frost.json) | A ridged fractal samples a billowy fractal rather than hard-wired Perlin. |
| Generic turbulence source | [Warped turbulence sum](../examples/warped-turbulence-sum.json) | Absolute-fractal summation samples warped noise. |
| Repeated fBm-driven warps applied to fBm | [Double warp opal](../examples/double-warp-opal.json) | Tune large and small displacement separately. |
| Scalar components and vector RGB mapping | [Vector iridescence](../examples/vector-iridescence.json) | Decorrelated scalar channels are visible as a colour material. |

The three medallion/inlay/enamel patterns explicitly project Z to zero with a
scalar-domain scale of `[1,1,0]`. This extrudes their XY motifs through the solid:
a repeat period of zero disables repetition on an axis, but does not remove
that axis from a 3D distance calculation. Without the projection, their small
spherical motifs at Z=0 would miss the preview cube.

## Distances and cells

| Capability | Examples | What to inspect or change |
| --- | --- | --- |
| Sphere, box, torus, cylinder and plane SDFs | [Spherical isobars](../examples/spherical-isobars.json), [Box agate](../examples/box-agate.json), [Toroidal copper](../examples/toroidal-copper.json), [Capped columns](../examples/capped-columns.json), [Oblique strata](../examples/oblique-strata.json) | Signed distance mapped to repeating contours, including distances inside the primitive. |
| Hard/smooth union, intersection and difference | [Hard union](../examples/hard-union.json), [Smooth union](../examples/smooth-union.json), [Hard intersection](../examples/hard-intersection.json), [Smooth intersection](../examples/smooth-intersection.json), [Hard difference](../examples/hard-difference.json), [Smooth difference](../examples/smooth-difference.json) | Compare each pair; set smoothing radius to zero for a hard join. |
| SDF plus old noise and transforms | [Weathered relic](../examples/weathered-relic.json), [Twisted signet](../examples/twisted-signet.json) | Noise perturbs a distance field; a twist changes where the torus is sampled. |
| Euclidean, Manhattan and Chebyshev F1 | [Pebbled jade](../examples/worley-euclidean.json), [Diamond paving](../examples/worley-manhattan.json), [Circuit cells](../examples/worley-chebyshev.json) | Same sites and palette, three metrics: rounded, diamond and square contours. |
| F2 and F2−F1 | [Second-neighbour satin](../examples/second-neighbour.json), [Cellular web](../examples/cellular-web.json), [Volcanic cells](../examples/volcanic-cells.json) | Change Output independently of metric; volcanic cells uses 3D sites. |
| Jitter, seed and random cell value | [Ordered cell quilt](../examples/ordered-cell-quilt.json), [Seeded terrazzo](../examples/seeded-terrazzo.json) | Zero jitter creates a grid; jitter changes boundaries, seed changes the sites and cell palette. |
| Random cell RGB | [Prismatic mosaic](../examples/prismatic-mosaic.json), [Volumetric opal](../examples/volumetric-opal.json) | Compare extruded 2D cells with 3D fragments. Full-width seeds are supported. |
| Cell identity vector | [Cell coordinate weave](../examples/cell-coordinate-weave.json) | Inspect the vector of cell lattice coordinates; it displaces the old Perlin pattern. |
| True Voronoi edge distance | [Flowing grout](../examples/flowing-grout.json), [Brecciated marble](../examples/brecciated-marble.json) | Inspect grout masks. Edge distance measures the actual bisector, unlike F2−F1. Perlin warps bend 2D grout; old marble fills 3D fragments. |
| Cellular plus fractal/SDF composition | [Cellular frost](../examples/cellular-frost.json), [Cellular reliquary](../examples/cellular-reliquary.json) | Worley is a generic fractal source; a smooth SDF subtraction masks seeded inlay against old ridged stone. |

Cellular dimensions 2 ignores Z and therefore remains visible through the cube.
Dimensions 3 creates a volume; try all three slice axes. Scale divides sampling
coordinates: 0.2 gives approximately five cells per unit. Distances are in these
local domain units. Scalar and vector cell projections use identical feature
points when dimensions, jitter and seed agree. Cell identity and cell colour
belong to the vector menu; cell value and edge distance belong to the scalar menu.

## JSON sketch

```json
{
  "version": 5,
  "name": "Rotated noise",
  "texture": {
    "type": "domain",
    "domain": {
      "type": "compose",
      "first": {"type": "translate", "offset": [0.5, 0.5, 0]},
      "second": {"type": "rotate", "rotation": [0, 0, 30]}
    },
    "base": {
      "type": "colourise",
      "field": {
        "type": "scalar-domain",
        "domain": {"type": "scale", "scale": [0.25, 0.25, 0.25]},
        "source": {
          "type": "fractal", "octaves": 4, "persistence": 0.5,
          "lacunarity": 2, "style": "smooth", "source": {"type": "noise"}
        }
      },
      "mode": "clamp",
      "ramp": {"type": "builtin", "name": "greyscale"}
    }
  }
}
```

`compose.first` maps sampling coordinates before `compose.second`. Transform
scale divides coordinates, so a scale of 0.25 gives four noise features per unit.
Legacy `perlin.scale` still expresses features per unit, preserving old documents.
Transforms can be applied independently to scalar, vector or colour expressions.
Vector transforms change the sampling position, not the output vector's basis.

The GPU rejects overlarge trees and more than 4096 expanded noise samples per
point. Fractal nesting multiplies sampling work; use a few octaves per level.
These resource limits are separate from slider ranges and reference semantics.
Cellular sampling also counts neighbourhood visits against this budget; 3D
edge distance is heavier than F1. Reaction volumes are limited to four distinct simulations per material and
64 million voxel updates per simulation. See the numerical conventions for
the worker and cache design.

## Blends and simulation examples

The `blend` colour node offers normal, multiply, screen, overlay, soft-light,
darken, lighten, difference and exclusion, with opacity and source-over alpha.
The nine `blend-*` examples demonstrate the modes on the same source/backdrop;
Screened contours and Translucent overprint combine them with existing fields.

Reaction–diffusion adds an editable, precomputed 3D scalar field. Chemical coral
and Chemical complement show V and U of the same volume. Incipient foam shows
seeded patches before they settle; Stratified colony shows a slab through an
oblique domain. Warped membrane distorts that slab with Perlin noise. Gilded
colony combines simulation with an SDF mask and screen blending, Fractal fossil
samples it at multiple scales, and Cellular infection displaces it with Voronoi
cell vectors. Inspect the field, switch slice axes and move through depth to see
the three-dimensional structure. Chemistry, resolution and iterations recompute
the volume; colour, concentration and camera changes reuse it.

## Verification

The final Haskell suite passes 821 tests; the frontend passes 811 Vitest tests,
four gallery-cache tests and its production build. All 28 local headless editor
workflows pass, including typed SDF/cellular editing, inspection, undo/redo,
persistence and PNG export. The Pages CI job additionally checks its gallery.
No runtime Haskell server is required.

Chrome and Firefox check 391 numerical cases and 174 reference images with
unchanged tolerances. All 19 blend/simulation additions were visually reviewed
in XY/XZ/YZ slices and on the cube. Float32 simulation fixtures match every
voxel exactly; worker cancellation, queue limits, failure recovery and eviction
have unit coverage. A production browser workflow checks concentration/view
reuse, chemistry recomputation, undo and context restoration. The eight new
simulation goldens were deliberately added with `stack test --ta --accept`;
Incipient foam was refined to expose the transient structure at 40 iterations.
See [4.7–4.8 evidence](../bench/phase4/blends-reaction-validation.json).

Earlier 4.4–4.6 validation follows. The final three new/refined examples additionally pass
all XY/XZ/YZ slices and 128px cube comparisons in Chrome. All 28 additions were
visually checked in XY and on the cube. The new cell fields were checked
headlessly; earlier Safari evidence remains in the 4.1–4.3 validation record.
Pre-4.4 image goldens remain unchanged. The hard/smooth difference goldens were
intentionally refined to frame the subtraction visibly in default views;
all 28 new image goldens and shared fixtures were regenerated with
`stack test --ta --accept`. See [4.4–4.6 evidence](../bench/phase4/fields-cells-validation.json)
and [earlier validation](../bench/phase4/validation.json).

The medallion visibility correction has depth-invariance and nonconstant
cube-face regressions for all three examples. Headless field comparisons and
all nine slice comparisons pass with unchanged XY goldens. One extra 128px
Offset medallions solid comparison has a single pixel outside the strict image
tolerance: FP32 tracing stops one step earlier near the existing hit threshold;
the material agrees at a shared position. The other two solid comparisons pass.
[Correction evidence](../bench/phase4/medallions-visibility.json) records this
bounded discrepancy; renderer tolerances and tracing are unchanged.
