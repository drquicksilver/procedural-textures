# Phase 4.1–4.3: fields, coordinates and warps

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

All 21 additions have reference image goldens and browser comparisons. Every
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
SDF fields, cellular/Voronoi sources, further blend modes and simulation fields
remain in 4.4–4.8.

## Verification

The Haskell suite passes 609 tests; the frontend passes 725 tests and its
production build. The assembled static Pages artifact passes all 27 headless
browser workflows, including typed scalar/domain editing, inspection, undo/redo,
persistence and PNG export. No runtime Haskell server is required.

GPU checks cover 202 numerical cases and 127 reference images in Chrome,
Firefox and Safari. The final knot mask refinement is rechecked headlessly.
All pre-existing image goldens and numerical tolerances remain unchanged.
The 21 new image goldens and shared version-5/schema fixtures are intentional
additions, regenerated with `stack test --ta --accept`.
See [the recorded validation](../bench/phase4/validation.json).

The medallion visibility correction has depth-invariance and nonconstant
cube-face regressions for all three examples. Headless field comparisons and
all nine slice comparisons pass with unchanged XY goldens. One extra 128px
Offset medallions solid comparison has a single pixel outside the strict image
tolerance: FP32 tracing stops one step earlier near the existing hit threshold;
the material agrees at a shared position. The other two solid comparisons pass.
[Correction evidence](../bench/phase4/medallions-visibility.json) records this
bounded discrepancy; renderer tolerances and tracing are unchanged.
