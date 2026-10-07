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

## A short learning path

Start with [Flat colour](../examples/flat-colour.json), then
[Linear gradient](../examples/gradient.json) to see a source mapped through a ramp.
[Position RGB](../examples/position-rgb.json) introduces depth and the signed
vector inspection range. [Perlin field](../examples/perlin-field.json) introduces
unmodified noise; the **Fractal noise controls** family adds shaping and detail.
Use **Threshold boundaries** to make a mask, then **Mix and source-over** to
understand how that mask and alpha change composition. Finish with **Transform
order**, then inspect the intermediate fields in **Coastal highlands**.

## Coverage after the tidy-up

The catalog now has 175 documents. Forty additions fill teaching gaps rather
than multiplying material variants: 36 are minimal studies or controlled
comparisons, and four are new finished compositions. Families keep related
studies together, while featured material examples remain easy to browse.
All original file IDs remain available.

| What to learn | Start here | Controlled change or inspection |
| --- | --- | --- |
| Constant colour and coordinates | Flat colour; Position RGB | Signed coordinates map -1–1 to RGB 0–1; compare Z=-1, 0 and 1 for the blue channel. |
| Unmodified gradient noise | Perlin field | One octave, no warp; try Viridis, Cividis and Inferno on this same field. |
| Fractal shaping and spectrum | Fractal noise controls | Six members isolate smooth/billowy/ridged, two versus five octaves, persistence 0.5 versus 0.8, and lacunarity 2 versus 3. |
| Values outside the ramp interval | Ramp addressing | Identical -1–3 planar field and Viridis palette; clamp, wrap and mirror only. |
| Hard versus smooth masks | Threshold boundaries | Equal bounds versus a smooth interval on the same noise. |
| Mixing versus layering | Mix and source-over | Half-opaque inputs: mixing preserves alpha; source-over accumulates coverage. |
| Transparent colour interpolation | Colour to transparent | Cyan retains its hue as alpha falls to zero. |
| Shared document ramps | Shared accent ramp | One named ramp controls both extruded badges. |
| Ordinary min/max | Scalar extrema | Same two noise fields, unlike the signed-distance join families. |
| Noncommuting transforms | Transform order | Same translation, rotation, repeat and source; only the first two maps swap. |
| Nearest-neighbour outputs | Worley neighbour outputs | F1, F2 and F2−F1 share sites and a fixed 0–1.5 greyscale range. |
| Exact cellular borders | Cell edge versus neighbour gap | Same sites and narrow ramp expose consistent versus variable border widths. |
| Site generation | Cell site controls | Jittered cells is the baseline; change jitter, seed or dimensions individually. |
| Chemical concentrations | Chemical concentrations | Raw U and V share the simulation and unremapped 0–1 greyscale. |
| Simulation evolution | Chemical evolution | 0, 100 and 550 steps; identical seed, grid, chemistry and colour mapping. |
| Reaction parameters | Higher-feed chemical colony | Feed 0.04 versus Chemical colony's 0.035; all other parameters match. |
| Constructed surface pattern | Staggered brickwork | Alternating half-brick offsets and a signed-box mortar mask. |
| Organic cellular marking | Giraffe markings | Matching cell value/edge fields, gentle distortion and explicit XY projection. |
| Coupled landscape scales | Coastal highlands | Coastal falloff plus ridge detail gated by the resulting broad elevation. |
| Truly continuous repetition | Seamless stone tile | Four shifted samples crossfade before repeat; boundary colours and slopes are tested. |

Existing examples supply angular/radial fields, SDF primitives and all three
sharp/smooth Boolean joins, generic and absolute fractals, vector components,
cell identity, arithmetic, local displacement, independent summed warps, bend,
twist, mirror, polar/radial repeat, nine blend modes and all three reaction initial
states. Use the capability tags and selected-field inspection to find the actual
mechanism rather than inferring it from a material name.

The eleven repaired images are intentional: Smiley, Pond ripples, Fissured bark,
Distance-faded ridges, Crosscurrent filaments, Radially repeated shells, Screened
contours and four related chemical compositions. The fossil and slab chemistries
retain their distinct original morphology. Slice hints expose interior effects
without replacing the user's current view automatically.

The examples demonstrate colour and sampling fields on existing preview shapes.
They do not imply displaced geometry, view-dependent reflection/iridescence,
physical lighting, animated reaction volumes or arbitrary native seamless noise.
These are boundaries of the current setup, not missing presets for supported
capabilities. Custom subject-specific ramps remain appropriate; every built-in
palette is available in the ramp picker without needing a separate texture for
all 45 palettes.

## Verification

The completed catalog passes 986 Haskell tests and 874 frontend tests plus four
cache tests. All 431 numerical GPU cases and 214 texture/shape image comparisons
pass on the Apple Metal browser backend without changing tolerances. The static
Pages artifact is built from the same metadata and gallery inputs; all 30 browser
checks pass (29 editor workflows plus the Pages navigation check). Forty new
image goldens were deliberately added; only the eleven reviewed existing images
were changed. Regression tests check the finite smiley face, continuous tile
values/slopes and the different alpha behaviour of mixing and layering.
