# Texture library guide

The supplied library separates appearance presets, minimal studies, controlled
comparisons and composition studies. Categories answer where to browse; capability
tags answer what a document demonstrates. Search the editor Library by name,
subject, tag or inspection hint. Comparison families expand together; searching
opens matching families automatically.

The seven browsing categories are Materials, Landscapes & atmosphere, Patterns &
symmetry, Geometry & distance, Colour & compositing, Fields & warps and Simulation.
Unknown and historical category strings still load and appear after these groups.
File IDs remain stable when displayed names change, so existing gallery PNG URLs
and example source IDs continue to work.

## Optional document guidance

Version 5 documents may carry an optional `guide` object:

```json
"guide": {
  "role": "comparison",
  "tags": ["SDF", "mask"],
  "family": "Signed-distance union",
  "order": 100,
  "hint": "Inspect the signed field; reduce smoothing to zero.",
  "preview": {"axis": "xz", "position": 0.5}
}
```

Roles are `preset`, `study`, `comparison` and `composition`. Tags default to an
empty list, order to 100, and family/hint to empty strings. Order is an integer
from 0 to 9999. Preview is optional; its axis defaults to XY and position to zero,
with finite positions between -2 and 2. Haskell and browser parsing preserve and
validate this guidance during migration, copying, autosave, import and export.
It never changes texture evaluation. No format-version bump is needed for the
optional descriptive extension; documents without it retain their behaviour.

Lower orders feature representative entries first. Members of a family are kept
adjacent, using the family's lowest rank, then their own order. The static HTML
gallery collapses comparison families and displays roles, tags and hints. The
contact sheet keeps all individual paired renders for complete visual review.

A chosen preview plane controls editor thumbnails and the gallery's slice image,
whose caption states the plane and coordinate. It does not change default golden
renders or silently reset the user's editor camera. **Show recommended slice**
explicitly selects it in the viewer. Thumbnail and gallery image caches include
the preview parameters; changing titles, tags or hints does not change pixels.

## Useful comparisons

- **Blend modes:** identical source, backdrop and opacity across nine modes.
  Inspect the two inputs separately before changing opacity.
- **Worley distance metrics:** identical sites, scale and palette with Euclidean,
  Manhattan and Chebyshev distance. Distinguish metric from neighbour output.
- **Signed-distance joins:** sharp/smooth union, intersection and difference.
  These are colour fields on the existing solid, not new preview geometry.
- **Repeated medallions:** translation, ordered rotation and anisotropic scale.
  Explicit Z projection makes the planar motifs visible throughout the cube.
- **Ribbon deformation:** analytic displacement, local noise attenuation,
  independently sampled summed flows, bend and height-dependent twist.
- **Ramp easing:** linear versus sinusoidal transitions with the same endpoints;
  six spans give three complete mirrored cycles.
- **Elevation map styles:** unlined colour elevations and two contour treatments.
- **Fractal source studies:** nested noise, cellular noise and warped noise as
  sources for generic multiscale sums.

The [full editorial review](reviews/2026-10-07-texture-library-review.md) records
why the tidy-up was undertaken. It describes the pre-tidy-up library, so its old
names and category counts are historical evidence rather than today's catalog.
