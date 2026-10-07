# Review of the texture coverage audit after the gallery tidy-up

Reviewed against the 175 version-5 examples, the current scalar/vector/domain/colour
implementation and the Phase 4 design. The source is
[texture-coverage.md](../texture-coverage.md). This is a recommendation document,
not a change to the master plan or an implementation of the proposed primitives.

Scope: RGB-valued fields of 3D position, with the current compositing facilities.
Lighting, material normals, displacement and geometry-dependent surface
attributes are future work. A planar motif can be explicitly extruded into a
3D field, as existing 2D cellular examples already are. Colour patterns suggesting
holes or grooves do not change the preview solid.

The audit is useful as an idea inventory. Its roughly 400-item tally is not a
count of distinct gallery-worthy textures. One mechanism often appears under
several material names. Recommendations below favour different spatial structure,
meaningful composition and useful learning examples over recolouring a familiar
field. Prospective recipes still need visual prototypes on slices and cutaways.

## 1. Already represented

- Zebra/tiger/giraffe markings; camouflage.
- Wood grain/knots/growth bands, birch bark, fissured plate-like bark and moss.
- Marble/granite/sandstone/agate/malachite/onyx, breccia-like cellular stone,
  pitted stone, cellular mosaics, mineral swirls and strata.
- Sand ripples/dunes/ice/frost, clouds/fog/storm/smoke/fire/plasma, pond/ocean patterns.
- Brushing, rust, patina, paper/watercolour washes, plaid, stripes, rings, moiré,
  medallions, rosettes and checked/cellular patterns.
- Running-bond brickwork and seamless stone; mature chemical labyrinths,
  concentrations, early/late evolution and a matched feed comparison.

The recent brickwork, giraffe, bark and chemistry work directly supersedes several
older audit suggestions. Its labels "foam", "oil-film interference", "basketweave"
and "copper" must not be read as proof of those particular materials: the actual
examples are early reaction patches, vector colour clouds, zero-jitter cells and
torus distance bands, respectively. Actual foam, woven crossings and overlap
structure remain worth considering.

## 2. Good additions with current primitives

### First batch

| Idea | What it contributes | Current route |
| --- | --- | --- |
| Truchet tiles | Locally random choices with globally connected paths | Two legal quarter-arc motifs, repeat, and a zero-jitter per-cell selection mask sampled before the local fold. Match exits at tile edges. |
| Plain weave | Explicit alternating over/under crossings; Plaid is a band pattern | Two thread orientations and checker-controlled crossing masks, with restrained colour detail. |
| Twill / denim | Diagonal progress of crossings through a weave | Row-dependent offset and cyclic selection; keep plain weave as a comparison. Carbon fibre is a palette variant within this family. |
| Overlapping fish scales | Ordered repeated overlap, absent from the independent medallions | Offset bounded scale motifs with layered foreground masks. Snake coloration is a variation, not a second generator. |
| Leopard rosettes | Broken rings with quiet centres, unlike giraffe patches or stripes | F1 annuli, matching cell values, an independent ring-breaking mask and sparse interior colour. |
| Bamboo | Repeated nodes coupled to long grain, not ordinary wood recolouring | Repeated node bands, stretched noise and separate internode variation. |
| Travertine | Long stratum bands interrupted by elongated pores | Layered band fields with stretched cellular pore masks at two scales. |
| Hierarchical crackle glaze | Fine cracks confined to larger compartments | Coarse true-edge network, cell-identity-controlled fine fields and an independently tunable pigment fill. Raku can be the finished presentation. |
| Combed marbled paper | Deliberate repeated comb displacement rather than noise-only turbulence | Smooth coloured bands, repeated triangular scalar displacements and successive orthogonal comb domains. |
| Halftone | Motif coverage responds to an independent broad field | Repeated disc distances compared against a slowly varying radius field sampled outside the repeat. This is a texture pattern, not screen-space postprocessing. |
| Digital camouflage | Deliberate domain quantisation | Quantise the sampling coordinates, then evaluate multiscale noise and discrete pigment masks. |
| Turtle scutes with growth lines | A boundary network coupled to a different interior structure | Matching cell-edge and F1 fields, with thin interior growth bands and subdued per-cell tint. |

These are my strongest first choices. They add topology, hierarchy, conditional
placement or structured sampling rather than just another ramp.

### Worth a second batch, selecting representatives

- Quarter-sawn ray flecks and multi-eye burl: grain coupled to differently arranged
  inclusions. Burl must resemble many local growth centres, not plain cellular dots.
- A sparse spotted coat, preferably Dalmatian: per-cell omission, variable mark
  size and quiet background. Cheetah and trout spots should not become independent
  presets of the same mechanism.
- Octagon-and-dot and a regular hex/honeycomb pattern: different tiling topology.
  Fixed motifs can be assembled with planes/boxes, repeat and offsets now. They
  are useful prototypes before designing a general layout primitive.
- Knitted loops: connected, repeated curved loops with explicit crossing order.
  Prototype recognisable stitches; reject a version that is merely chevrons.
- Chipped multicoat paint and tile-boundary wear: correlate pigment loss with the
  generated mask or mortar edge. One well-composed coating example beats many
  independent "noise mask over substrate" presets.
- Sea-foam cells: broken, unequal white loops on quiet water. The existing early
  reaction patches do not supply this subject.
- One rounded-inclusion material, preferably salami cross-section for new subject
  breadth, or conglomerate. Use separate inclusion-size/pigment/matrix variation;
  do not add both if they are the same recipe with different colours.
- One porous volume cross-section, such as cellular sponge or pumice, demonstrating
  3D pore distribution. Dark pore interiors are colour features, not geometry holes.
- A distinctive compound ornament: paisley, Greek key, a designed damask motif or
  an eight-point star lattice. Pick a recognisable design; the broad label
  "Islamic geometric" is not a concrete specification.
- One radial organic motif, such as a peacock eye or kiwi cross-section. Existing
  rings plus purposeful seed/barb structure can add something visually distinct.
  Do not describe the current angular field as an atan2 coordinate map.
- A small fixed herringbone repeat: assemble a finite periodic arrangement of
  rectangular SDF motifs before investing in a general ownership/layout node.
- Source-linked short stain streaks: combine a bounded set of shifted copies of
  a sparse source mask, with fading weights. This is an approximate finite-sample
  smear, already expressible; it does not supply arbitrary blur or fluid history.
- A Bayer threshold study, paired with halftone if a technical pattern is wanted.
  It demonstrates ordered threshold placement, not a new material finish.

Variants such as herringbone weave, basketweave, snake scales, Raku and coloured
thread palettes belong in small families. A family should justify each member
with a structural change, not collect every material synonym from the audit.

## 3. Poor additions as proposed

"Poor" here means poor value for this gallery and this RGB scope, not an assertion
that the real material is uninteresting.

| Family to decline | Audit ideas | Reason |
| --- | --- | --- |
| Flat or faint-noise finish labels | Smooth plastic/ABS/soft-touch/rubber, chocolate, glazed ceramic/porcelain, anodised aluminium, smooth painted walls, smooth leather/suede, felt, velvet | The offered recipe is flat colour or ordinary faint noise. Much of the distinction depends on reflectance, fibres or relief. |
| More granular recolourings | Fine/coarse sand, soil/loam/clay/mud/ash, cork, scree, speckled laminate/engineered quartz, recycled paper flecks, ordinary carpet | Existing grain/cell examples already show the proposed structure. Add a specific new hierarchy or placement mechanism first. |
| More plain cellular mosaics | Facet-coloured quartz, sycamore patches as simple cell colour, salt flats, columnar basalt, flagstones/crazy paving, generic stone masonry, simple cracked mud/leather, loopy slime-mould networks | Changing grout width and palette does not justify another nominal material. Hierarchical crackle is the stronger new composition. |
| More banded or stretched noise | Slate/obsidian/Jasper as specified, Eucalyptus, choppier water, ordinary river turbulence, reactive glaze without runs, corduroy, scuffs, fibreglass, Venetian plaster | Existing strata, grain, water and flow studies already cover those recipes. |
| More identical sparse pore/dot treatments | Cast iron pits, skin pores, cheese holes, citrus stipple, seeded glass, stippled paint, apple lenticels, strawberry dots | A dot pattern under a new name is weak. The selected porous or spotted example should provide actual size/hierarchy/selection differences. |
| Minor bond/material variants | Stack/English/Flemish bond, subway/setts/ashlar/concrete blocks, terracotta/slate floors, recoloured handmade brick | Use a future layout comparison if needed; the current running-bond brickwork is already a strong standalone. |
| Generic recoloured coating masks | Tarnish, bleaching, mould, moss/algae overlays, chips as arbitrary noise, tide lines, deposits, generic weathered/painted brick | Current patina, corrosion, masks and contours cover the machinery. Correlated edge wear or multiple visible coats has a clearer new lesson. |
| Nominal textile variants without structure | Separate linen/canvas/hessian/tweed/Kevlar/carbon-fibre presets, Batik as recoloured crackle, vague tie-dye, fuzzy/flocked wallpaper | Select one actual weave and one designed ornament. Variants need a visible structural reason to survive. |
| Finish names whose proposed pattern loses the subject | Crumpled paper as flat cell colours, quilting as a rotated grid, diamond plate/knurling/corrugation as stripes, weld bead as crescents, board-marked concrete without a convincing imprint | Relief and shading carry the recognition. The minimal RGB recipe would mainly rename an existing pattern. |
| Simple repeat motifs without a new lesson | Extra penny/polka-dot grids, plain ceramic grids, simple grilles/perforated sheets/mesh, ribbed rubber, lathe marks, corrugated cardboard, basic chevrons | Covered repeat/shape/band concepts. Variable coverage and connected/overlapping motifs are better additions. |
| Food versions of existing patterns | Marbled chocolate, blue-cheese veins, watermelon stripes, melon netting, pineapple diamonds, onion rings, generic coffee crema, bread/cake/toast as another pore colourway | Two distinctive food examples are enough; do not reproduce the entire mineral/cell library in food colours. |
| Unconvincing shortcuts | Leaf veins via loopy cell edges, butterfly wings via arbitrary grout, palm-trunk chevrons, regular cells labelled crocodile, coarse cells labelled realistic gravel | The nominal subject requires more specific organisation. These particular shortcuts would overpromise. |

Cable-knit, crochet/lace and detailed mechanical tread are expensive compositions
relative to their unproven visual benefit now. Start with recognisable knit loops
and structured weave; revisit these only if they produce a clearly different
RGB pattern. True loose fibres, irregular overlapping gravel, growth venation,
sequential cracks, source-dependent streaks and shell pigmentation belong with
future generator decisions rather than weak current approximations.

## Corrections to the audit's implementation assumptions

- Repeat is centred: `repeat(x,1)` is in [-0.5,0.5), so
  `x - repeat(x,1) = floor(x+0.5)`, not ordinary floor. A half-cell shift and
  offset derive floor; negatives and exact ties need correct handling.
- Mirror folds selected axes once around a centre. It is not a periodic triangle
  wave on its own. Periodic scalar triangles need repeat plus absolute value or
  equivalent arithmetic. A mirrored colour ramp is not automatically a scalar.
- The angular scalar is a cosine-derived symmetric fan, not full atan2 azimuth.
  The audit's literal generic polar-coordinate warp is therefore unavailable.
- Sine is not strictly impossible today. A domain can map a point to `(1,0,f(p))`, then Twist by 360 degrees per Z unit. The existing angular fan returns `(1-sin(2πf(p)))/2`; remapping yields sine. A current-renderer prototype confirms the cycle. This is a good reason for a direct scalar operator, not proof of an expressive impossibility.
- Cell-edge is Euclidean. Chebyshev/Manhattan apply to Worley distance outputs,
  not to the existing true-edge projection.
- Cell ID is the winning feature's integer lattice identity, not its jittered
  feature position. It can vary a child field; it does not automatically provide
  arbitrary feature-centred child coordinates or a general scatter operation.
- Variable halftone radius is derived by subtracting a scalar radius from a
  distance before thresholding; current primitive radius parameters are constants.
- Herringbone and hexagonal periodic RGB motifs are not intrinsically beyond
  the language. General ownership, local coordinates and irregular layouts are
  what a layout primitive would make practical.
- T-junctions alone do not require sequential fracture: a fine network clipped at a coarse compartment boundary already creates them. The missing process is chronological crack propagation and arrest.
- Periodic noise alone does not construct Escher-style interlocking tile edges.
  Such boundaries need their own coordinated motif/warp design.
- The current RD cube repeats because of its unit-period sampling contract.
  More voxel resolution alone does not remove periodicity, and the 64-million
  update budget prevents increasing resolution and iterations without tradeoffs.

## Proposed primitives: decisions for later Phase 4

| ID | Decision | Scope and rationale |
| --- | --- | --- |
| N1 scalar maths | Add first | sin/cos, abs, floor/fract, clamp, safe divide, pow and scalar mix. abs/clamp/mix/floor are useful ergonomics; direct sine/cosine avoid elaborate constructions, while general division/power support useful calculations. Add true atan2/azimuth too. Specify units, negative-coordinate behaviour and singularities, rather than treating the package as automatically trivial. |
| N13 periodic noise | Add early | User-chosen integer lattice periods, useful 3D periodic noise and a periodic fractal contract. Current Perlin has an intrinsic 256-lattice repeat; generic rotated fractal octaves do not preserve arbitrary axis periods. Native repeatable noise is still valuable despite our four-sample XY seamless tile. Verify values and slopes across all three boundaries and the composed warp. |
| N3 scatter/bombing | Add; highest substantial new generator | Bounded child motifs in 3D or extruded 2D with seeded placement, size/rotation, density and explicit overlap. It adds actual objects/marks, rather than nearest-site distances. Bound footprints and neighbour searches: cost is not merely k times the child because multiple neighbouring cells may overlap the query. Keep deterministic CPU/GPU behaviour and resource limits. |
| N2 tile layouts | Add a narrower form after prototypes | Shared layout configuration with typed projections for local coordinates, identity/random and edge distance, following the cell projection precedent. Start grid, running bond, hex and herringbone. Grid/bond are conveniences; correct ownership and local coordinates across more complex layouts are the main value. Avoid starting with Versailles and arbitrary size/position/rotation jitter together. |
| N5 orientation fields | Add a focused domain operation; defer Gabor noise | Field-valued angle with a fixed rotation axis, then a well-defined 3D frame if needed. It supports pigments/fibres following an authored direction field. With sin/cos this is often expressible through component arithmetic and warp, so a dedicated node mainly improves clarity and cost. Gabor is a separate noise generator with a larger numerical/performance burden. Surface-driven orientation remains out of scope. |
| N4 gradient/curl field maths | Consider later, independently of relief | Scalar gradient and curl of a suitable vector potential can drive RGB displacement fields. Do not add height lighting or material normals. Finite differences resample the source; 3D forward gradient needs four samples and central gradient six. Nested cellular/fractal sources can make this expensive. Require a convincing flow example and compiler work accounting. Normalised iso-distance is a first-order local estimate, not an exact distance transform; near-zero gradients need explicit handling. |
| N4 relief/shade | Exclude | Explicitly changes lighting response, even if it ultimately returns RGB. |
| N7 growth networks | Worth a later Phase 4 milestone | One bounded deterministic branching generator sampled as a scalar distance/density and mapped to RGB. Tapering parent/child branches add topology not supplied by cellular loops. Start one growth model; do not bundle DLA, L-systems and space colonisation into a single feature. It does not alter preview geometry. |
| N10 RD extensions | Add selectively, after the foundations | Scalar-defined initial seeding and bounded spatial feed/kill fields are the best initial subset. They couple chemistry to authored structure. Field sampling, dependencies, periodic seams and simulation/cache keys need design; the current worker does not already evaluate arbitrary material trees. An optional extruded high-resolution 2D mode fits the 2D-cell precedent. Defer anisotropic diffusion and 1D-over-time shell models to separate justified milestones. |
| N6 non-local filters | Skip this Phase 4 expansion | RGB streaks and spread are valid subjects, but this is a large filtering/baking subsystem. Some fixed filters are finite local shifted samples and do not inherently require a bake. Arbitrary blur/morphology/distance transforms need specified sampling, resolution, boundaries and approximation quality. Prototype one source-linked streak operator before adopting the whole package. |
| N8 sequential fracture | Defer beyond the next expansion | Sequential arrest and fracture history are useful new processes, but narrow next to scatter and branching. Simple T-junctions can already result from clipping fine cracks at coarse boundaries. It should be a dedicated bounded simulation if revisited, not a claim that another Voronoi variant supplies fracture history. |
| N9 surface attributes | Exclude | Shape normals, curvature, AO and thickness would make the material depend on preview geometry. World-up is already a constant direction; it does not require a surface-attribute package. |
| N11 lighting | Exclude | Gloss, specular, anisotropic reflection and refraction are outside the user's current scope. |
| N12 aperiodic tilings | Skip this Phase 4 expansion | Penrose has genuine visual novelty but is a specialised tiling problem. Establish useful layout projections and composition ergonomics before a dedicated aperiodic generator. |

Vector-component extraction would also be a small useful adjunct to N1/N5: the
current vector constructor assembles components but does not expose an arbitrary
vector child's components as scalar fields. This is field algebra, not normals.

Suggested sequence: current-gallery first batch; N1; N13; bounded N3; narrowly
specified N2; focused N5; then one N7 milestone and selected N10 extensions.
N4's non-lighting mathematics is optional when a flow example justifies the
cost. N6/N8/N12 stay on a future idea list. N4 relief, N9 and N11 are excluded.

## Complete decisions for the audit’s “Can build” entries

This appendix classifies all 179 original recipe entries. Several entries
contain multiple subject names, so this count is not the audit’s approximate
400-item tally. A recommendation for a family means select its strong
representative; it does not mean adding every named colourway. The older
“Have” subjects are covered by the brief list above and its corrections.


### Animal skin, fur and shells

| Audit idea | Group | Decision reason |
| --- | --- | --- |
| Leopard | 2 — Recommend | Broken feature-centred rosettes add a new marking structure. |
| Cheetah | 3 — Decline as proposed | Other spot colourways repeat the chosen sparse-coat example; generic vein/grout or cell shortcuts would overpromise the specific animal structure. |
| Jaguar | 3 — Decline as proposed | Other spot colourways repeat the chosen sparse-coat example; generic vein/grout or cell shortcuts would overpromise the specific animal structure. |
| Dalmatian | 2 — Recommend | Choose one sparse spotted coat to demonstrate cell omission and size variation. |
| Trout/salmon | 3 — Decline as proposed | Other spot colourways repeat the chosen sparse-coat example; generic vein/grout or cell shortcuts would overpromise the specific animal structure. |
| Turtle shell | 2 — Recommend | Couple scute boundaries to internal growth bands. |
| Tortoiseshell | 3 — Decline as proposed | Other spot colourways repeat the chosen sparse-coat example; generic vein/grout or cell shortcuts would overpromise the specific animal structure. |
| Fish scales | 2 — Recommend | Demonstrate explicit ordered overlap. |
| Snake skin | 2 — Recommend | A fish-scale family variation; do not add a separate identical generator. |
| Crocodile | 3 — Decline as proposed | Other spot colourways repeat the chosen sparse-coat example; generic vein/grout or cell shortcuts would overpromise the specific animal structure. |
| Butterfly wing | 3 — Decline as proposed | Other spot colourways repeat the chosen sparse-coat example; generic vein/grout or cell shortcuts would overpromise the specific animal structure. |
| Peacock eye | 2 — Recommend | Second-batch radial organic motif; inspect the actual folded angular semantics. |

### Wood, bark and plant material

| Audit idea | Group | Decision reason |
| --- | --- | --- |
| Quarter-sawn ray fleck | 2 — Recommend | Ray flecks coupled to grain give a distinct wood structure. |
| Burl | 2 — Recommend | Many local growth eyes rather than another single knot; require a convincing prototype. |
| Bamboo | 2 — Recommend | Repeated nodes coupled to axial grain are a clear new structure. |
| Cork | 3 — Decline as proposed | Existing grain, bark, moss, band and cellular structures cover the proposed shortcut; genuine venation requires different topology. |
| Pine bark | 1 — Represented | The repaired Fissured bark already combines stretched boundary fissures and wood detail. |
| Plane/sycamore bark | 3 — Decline as proposed | Existing grain, bark, moss, band and cellular structures cover the proposed shortcut; genuine venation requires different topology. |
| Eucalyptus | 3 — Decline as proposed | Existing grain, bark, moss, band and cellular structures cover the proposed shortcut; genuine venation requires different topology. |
| Palm trunk | 3 — Decline as proposed | Existing grain, bark, moss, band and cellular structures cover the proposed shortcut; genuine venation requires different topology. |
| Leaf surface | 3 — Decline as proposed | Existing grain, bark, moss, band and cellular structures cover the proposed shortcut; genuine venation requires different topology. |
| Lichen | 3 — Decline as proposed | Existing grain, bark, moss, band and cellular structures cover the proposed shortcut; genuine venation requires different topology. |
| Dried leaf | 3 — Decline as proposed | Existing grain, bark, moss, band and cellular structures cover the proposed shortcut; genuine venation requires different topology. |
| Woven straw | 3 — Decline as proposed | Existing grain, bark, moss, band and cellular structures cover the proposed shortcut; genuine venation requires different topology. |
| Bracket fungus | 3 — Decline as proposed | Existing grain, bark, moss, band and cellular structures cover the proposed shortcut; genuine venation requires different topology. |

### Stone, minerals and geology

| Audit idea | Group | Decision reason |
| --- | --- | --- |
| Limestone | 3 — Decline as proposed | A cellular, banded or fine-noise colourway adds limited value; selected porous/inclusion materials and hierarchical cracks are stronger. |
| Slate | 3 — Decline as proposed | A cellular, banded or fine-noise colourway adds limited value; selected porous/inclusion materials and hierarchical cracks are stronger. |
| Travertine | 2 — Recommend | Elongated pores and strata form a new coupled material. |
| Quartz/crystalline | 3 — Decline as proposed | A cellular, banded or fine-noise colourway adds limited value; selected porous/inclusion materials and hierarchical cracks are stronger. |
| Jasper | 3 — Decline as proposed | A cellular, banded or fine-noise colourway adds limited value; selected porous/inclusion materials and hierarchical cracks are stronger. |
| Conglomerate | 2 — Recommend | Rounded variable inclusions; choose this or salami if their structures duplicate. |
| Pumice | 3 — Decline as proposed | A cellular, banded or fine-noise colourway adds limited value; selected porous/inclusion materials and hierarchical cracks are stronger. |
| Obsidian | 3 — Decline as proposed | A cellular, banded or fine-noise colourway adds limited value; selected porous/inclusion materials and hierarchical cracks are stronger. |
| Columnar basalt | 3 — Decline as proposed | A cellular, banded or fine-noise colourway adds limited value; selected porous/inclusion materials and hierarchical cracks are stronger. |
| Cracked mud | 3 — Decline as proposed | A cellular, banded or fine-noise colourway adds limited value; selected porous/inclusion materials and hierarchical cracks are stronger. |
| Rock face | 3 — Decline as proposed | A cellular, banded or fine-noise colourway adds limited value; selected porous/inclusion materials and hierarchical cracks are stronger. |

### Terrain, soil and granular materials

| Audit idea | Group | Decision reason |
| --- | --- | --- |
| Fine sand | 3 — Decline as proposed | Granular/cellular/banded variations are well covered; additional names alone do not add structure. |
| Wet beach sand | 3 — Decline as proposed | Granular/cellular/banded variations are well covered; additional names alone do not add structure. |
| Pebbles/shingle and gravel | 3 — Decline as proposed | Granular/cellular/banded variations are well covered; additional names alone do not add structure. |
| Scree | 3 — Decline as proposed | Granular/cellular/banded variations are well covered; additional names alone do not add structure. |
| Soil, rich loam, clay, mud, ash, volcanic soil | 3 — Decline as proposed | Granular/cellular/banded variations are well covered; additional names alone do not add structure. |
| Dried cracked earth | 3 — Decline as proposed | Granular/cellular/banded variations are well covered; additional names alone do not add structure. |
| Salt flats | 3 — Decline as proposed | Granular/cellular/banded variations are well covered; additional names alone do not add structure. |
| Snow | 3 — Decline as proposed | Granular/cellular/banded variations are well covered; additional names alone do not add structure. |
| Glacier ice | 3 — Decline as proposed | Granular/cellular/banded variations are well covered; additional names alone do not add structure. |

### Water, atmosphere and fluid-like patterns

| Audit idea | Group | Decision reason |
| --- | --- | --- |
| Choppy water | 1 — Represented | Ocean already has directional disturbances and broken crests. |
| Sea foam cells | 2 — Recommend | Actual broken cellular foam remains absent from the gallery. |
| River turbulence | 1 — Represented | Generic displaced flow is represented by the flow/warp family; exact curl is a future field operator. |

### Biological / microscopic / reaction patterns

| Audit idea | Group | Decision reason |
| --- | --- | --- |
| Brain coral | 1 — Represented | The new Chemical colony supplies the proposed mature labyrinth morphology. |
| Sponge | 2 — Recommend | Choose one RGB porous volume cross-section; no geometric-hole claim. |
| Slime mould | 3 — Decline as proposed | A plain cellular/stripe/dot variant supplies little new structure; avoid assigning a stronger biological identity to it. |
| Skin pores | 3 — Decline as proposed | A plain cellular/stripe/dot variant supplies little new structure; avoid assigning a stronger biological identity to it. |
| Wrinkles | 3 — Decline as proposed | A plain cellular/stripe/dot variant supplies little new structure; avoid assigning a stronger biological identity to it. |
| Trabecular bone | 3 — Decline as proposed | A plain cellular/stripe/dot variant supplies little new structure; avoid assigning a stronger biological identity to it. |

### Brick, masonry and architectural surfaces

| Audit idea | Group | Decision reason |
| --- | --- | --- |
| Running bond | 1 — Represented | Staggered brickwork now supplies running bond; stack bond is a family variation. |
| English bond | 3 — Decline as proposed | Minor bond/material variations or generic masked noise; reserve a gallery slot for actual feature-correlated wear or layout teaching. |
| Flemish bond | 3 — Decline as proposed | Minor bond/material variations or generic masked noise; reserve a gallery slot for actual feature-correlated wear or layout teaching. |
| Old handmade brick | 3 — Decline as proposed | Minor bond/material variations or generic masked noise; reserve a gallery slot for actual feature-correlated wear or layout teaching. |
| Weathered brick and painted brick | 3 — Decline as proposed | Minor bond/material variations or generic masked noise; reserve a gallery slot for actual feature-correlated wear or layout teaching. |
| Mortared stone, random rubble, dry-stone | 3 — Decline as proposed | Minor bond/material variations or generic masked noise; reserve a gallery slot for actual feature-correlated wear or layout teaching. |
| Coursed stone | 3 — Decline as proposed | Minor bond/material variations or generic masked noise; reserve a gallery slot for actual feature-correlated wear or layout teaching. |
| Ashlar and concrete blocks | 3 — Decline as proposed | Minor bond/material variations or generic masked noise; reserve a gallery slot for actual feature-correlated wear or layout teaching. |
| Smooth cast concrete | 3 — Decline as proposed | Minor bond/material variations or generic masked noise; reserve a gallery slot for actual feature-correlated wear or layout teaching. |
| Board-marked concrete | 3 — Decline as proposed | Minor bond/material variations or generic masked noise; reserve a gallery slot for actual feature-correlated wear or layout teaching. |
| Aggregate concrete | 3 — Decline as proposed | Minor bond/material variations or generic masked noise; reserve a gallery slot for actual feature-correlated wear or layout teaching. |
| Cracked concrete | 3 — Decline as proposed | Minor bond/material variations or generic masked noise; reserve a gallery slot for actual feature-correlated wear or layout teaching. |
| Stucco/render, Pebbledash | 3 — Decline as proposed | Minor bond/material variations or generic masked noise; reserve a gallery slot for actual feature-correlated wear or layout teaching. |

### Tiles, paving and geometric coverings

| Audit idea | Group | Decision reason |
| --- | --- | --- |
| Square ceramic tiles | 3 — Decline as proposed | Minor rectangle/disc/cellular variation of existing examples; more useful after a layout family has a distinct teaching goal. |
| Subway tiles | 3 — Decline as proposed | Minor rectangle/disc/cellular variation of existing examples; more useful after a layout family has a distinct teaching goal. |
| Octagon-and-dot | 2 — Recommend | New tiling arrangement using current shape composition. |
| Basketweave | 2 — Recommend | A structural variant in the plain/twill weave family, not a separate palette preset. |
| Penny tiles | 3 — Decline as proposed | Minor rectangle/disc/cellular variation of existing examples; more useful after a layout family has a distinct teaching goal. |
| Terracotta and slate floor | 3 — Decline as proposed | Minor rectangle/disc/cellular variation of existing examples; more useful after a layout family has a distinct teaching goal. |
| Flagstones and crazy paving | 3 — Decline as proposed | Minor rectangle/disc/cellular variation of existing examples; more useful after a layout family has a distinct teaching goal. |
| Cobblestones | 3 — Decline as proposed | Minor rectangle/disc/cellular variation of existing examples; more useful after a layout family has a distinct teaching goal. |
| Laminate floorboards | 3 — Decline as proposed | Minor rectangle/disc/cellular variation of existing examples; more useful after a layout family has a distinct teaching goal. |
| Hexagonal tiles | 2 — Recommend | Prototype a regular hex layout now; keep the complex expression bounded. |

### Ceramics, glass and enamel

| Audit idea | Group | Decision reason |
| --- | --- | --- |
| Glazed ceramic, porcelain, stoneware | 3 — Decline as proposed | Faint noise, speckles or an arbitrary mask mainly repeat existing colour fields; gloss/refraction distinctions are outside scope. |
| Crackle glaze and crazed enamel | 2 — Recommend | Hierarchical fine/coarse cracks supply a new lesson. |
| Speckled glaze | 3 — Decline as proposed | Faint noise, speckles or an arbitrary mask mainly repeat existing colour fields; gloss/refraction distinctions are outside scope. |
| Reactive glaze | 3 — Decline as proposed | Faint noise, speckles or an arbitrary mask mainly repeat existing colour fields; gloss/refraction distinctions are outside scope. |
| Raku | 2 — Recommend | Potential finished presentation of the hierarchical crackle family. |
| Frosted glass | 3 — Decline as proposed | Faint noise, speckles or an arbitrary mask mainly repeat existing colour fields; gloss/refraction distinctions are outside scope. |
| Seeded glass | 3 — Decline as proposed | Faint noise, speckles or an arbitrary mask mainly repeat existing colour fields; gloss/refraction distinctions are outside scope. |
| Chipped enamel | 3 — Decline as proposed | Faint noise, speckles or an arbitrary mask mainly repeat existing colour fields; gloss/refraction distinctions are outside scope. |

### Metals

| Audit idea | Group | Decision reason |
| --- | --- | --- |
| Circular brushed metal | 3 — Decline as proposed | As specified this is familiar noise, arcs, circles or stripes; the metal finish relies strongly on lighting or relief. |
| Galvanised steel | 3 — Decline as proposed | As specified this is familiar noise, arcs, circles or stripes; the metal finish relies strongly on lighting or relief. |
| Cast iron | 3 — Decline as proposed | As specified this is familiar noise, arcs, circles or stripes; the metal finish relies strongly on lighting or relief. |
| Tarnished brass and bronze patina | 1 — Represented | Current rust/patina/corrosion compositions cover the suggested mechanism. |
| Anodised aluminium | 3 — Decline as proposed | As specified this is familiar noise, arcs, circles or stripes; the metal finish relies strongly on lighting or relief. |
| Machined/toolpath | 3 — Decline as proposed | As specified this is familiar noise, arcs, circles or stripes; the metal finish relies strongly on lighting or relief. |
| Perforated metal | 3 — Decline as proposed | As specified this is familiar noise, arcs, circles or stripes; the metal finish relies strongly on lighting or relief. |
| Diamond plate | 3 — Decline as proposed | As specified this is familiar noise, arcs, circles or stripes; the metal finish relies strongly on lighting or relief. |
| Corrugated metal | 3 — Decline as proposed | As specified this is familiar noise, arcs, circles or stripes; the metal finish relies strongly on lighting or relief. |

### Fabric and textiles

| Audit idea | Group | Decision reason |
| --- | --- | --- |
| Plain weave | 2 — Recommend | Actual crossing structure is absent; Plaid does not replace it. |
| Twill, denim, carbon-fibre-style | 2 — Recommend | Diagonal crossing progression; denim representative, carbon-fibre a variant. |
| Herringbone fabric | 2 — Recommend | A structured weave comparison after the plain/twill prototype. |
| Linen, canvas, hessian | 3 — Decline as proposed | Prioritise recognisable weave and knit structure; these colourways, noise stand-ins or complex unproven variants do not justify separate presets now. |
| Corduroy | 3 — Decline as proposed | Prioritise recognisable weave and knit structure; these colourways, noise stand-ins or complex unproven variants do not justify separate presets now. |
| Tweed | 3 — Decline as proposed | Prioritise recognisable weave and knit structure; these colourways, noise stand-ins or complex unproven variants do not justify separate presets now. |
| Knitted fabric | 2 — Recommend | Connected loop structure, conditional on recognisable stitches in RGB. |
| Cable knit | 3 — Decline as proposed | Prioritise recognisable weave and knit structure; these colourways, noise stand-ins or complex unproven variants do not justify separate presets now. |
| Crochet and lace | 3 — Decline as proposed | Prioritise recognisable weave and knit structure; these colourways, noise stand-ins or complex unproven variants do not justify separate presets now. |
| Carpet | 3 — Decline as proposed | Prioritise recognisable weave and knit structure; these colourways, noise stand-ins or complex unproven variants do not justify separate presets now. |
| Felt, wool, suede-like | 3 — Decline as proposed | Prioritise recognisable weave and knit structure; these colourways, noise stand-ins or complex unproven variants do not justify separate presets now. |
| Quilting | 3 — Decline as proposed | Prioritise recognisable weave and knit structure; these colourways, noise stand-ins or complex unproven variants do not justify separate presets now. |
| Paisley | 2 — Recommend | A concrete designed compound motif is worth a prototype. |
| Batik | 3 — Decline as proposed | Prioritise recognisable weave and knit structure; these colourways, noise stand-ins or complex unproven variants do not justify separate presets now. |
| Tie-dye | 3 — Decline as proposed | Prioritise recognisable weave and knit structure; these colourways, noise stand-ins or complex unproven variants do not justify separate presets now. |
| Tartan | 1 — Represented | Plaid supplies the crossed colour-band design; actual woven crossings belong to the new weave family. |
| Velvet | 3 — Decline as proposed | Prioritise recognisable weave and knit structure; these colourways, noise stand-ins or complex unproven variants do not justify separate presets now. |

### Leather, hide and paper-like materials

| Audit idea | Group | Decision reason |
| --- | --- | --- |
| Smooth leather and suede | 3 — Decline as proposed | Noise/cells/stripes repeat existing fields; the claimed tactile or folded finish is poorly represented by the minimal RGB recipe. |
| Pebbled leather | 3 — Decline as proposed | Noise/cells/stripes repeat existing fields; the claimed tactile or folded finish is poorly represented by the minimal RGB recipe. |
| Cracked aged leather | 3 — Decline as proposed | Noise/cells/stripes repeat existing fields; the claimed tactile or folded finish is poorly represented by the minimal RGB recipe. |
| Office paper, cardboard, handmade paper | 3 — Decline as proposed | Noise/cells/stripes repeat existing fields; the claimed tactile or folded finish is poorly represented by the minimal RGB recipe. |
| Corrugated cardboard | 3 — Decline as proposed | Noise/cells/stripes repeat existing fields; the claimed tactile or folded finish is poorly represented by the minimal RGB recipe. |
| Recycled paper flecks | 3 — Decline as proposed | Noise/cells/stripes repeat existing fields; the claimed tactile or folded finish is poorly represented by the minimal RGB recipe. |
| Crumpled paper | 3 — Decline as proposed | Noise/cells/stripes repeat existing fields; the claimed tactile or folded finish is poorly represented by the minimal RGB recipe. |
| Torn paper | 3 — Decline as proposed | Noise/cells/stripes repeat existing fields; the claimed tactile or folded finish is poorly represented by the minimal RGB recipe. |
| Marbled paper | 2 — Recommend | Ordered comb displacement differs from noise-only warps. |

### Paint, plaster and coated surfaces

| Audit idea | Group | Decision reason |
| --- | --- | --- |
| Smooth painted wall | 3 — Decline as proposed | Basic noise/stipple/coat variations repeat existing fields; select one coherent multicoat or tile-wear composition. |
| Spray paint | 3 — Decline as proposed | Basic noise/stipple/coat variations repeat existing fields; select one coherent multicoat or tile-wear composition. |
| Stippled paint | 3 — Decline as proposed | Basic noise/stipple/coat variations repeat existing fields; select one coherent multicoat or tile-wear composition. |
| Chipped paint | 2 — Recommend | Select a multicoat/feature-correlated composition, not arbitrary noise masking. |
| Layered old paint | 2 — Recommend | One representative with several visible coats and linked boundaries. |
| Craquelure | 3 — Decline as proposed | Basic noise/stipple/coat variations repeat existing fields; select one coherent multicoat or tile-wear composition. |
| Venetian plaster | 3 — Decline as proposed | Basic noise/stipple/coat variations repeat existing fields; select one coherent multicoat or tile-wear composition. |
| Rough plaster | 3 — Decline as proposed | Basic noise/stipple/coat variations repeat existing fields; select one coherent multicoat or tile-wear composition. |
| Wallpaper | 2 — Recommend | Choose a designed damask motif, not a generic label or another rosette. |
| Flocked wallpaper | 3 — Decline as proposed | Basic noise/stipple/coat variations repeat existing fields; select one coherent multicoat or tile-wear composition. |

### Plastics, rubber and manufactured composites

| Audit idea | Group | Decision reason |
| --- | --- | --- |
| Smooth moulded plastic, ABS, soft-touch, rubber, tyre rubber, Formica, Corian | 3 — Decline as proposed | Basic finish colourways, grain and cell variants; use the weave/porous representative instead. |
| Injection-moulded stipple | 3 — Decline as proposed | Basic finish colourways, grain and cell variants; use the weave/porous representative instead. |
| Carbon fibre and Kevlar | 3 — Decline as proposed | Basic finish colourways, grain and cell variants; use the weave/porous representative instead. |
| Fibreglass | 3 — Decline as proposed | Basic finish colourways, grain and cell variants; use the weave/porous representative instead. |
| Speckled laminate and engineered quartz | 3 — Decline as proposed | Basic finish colourways, grain and cell variants; use the weave/porous representative instead. |
| Foam | 3 — Decline as proposed | Basic finish colourways, grain and cell variants; use the weave/porous representative instead. |

### Mechanical and industrial patterns

| Audit idea | Group | Decision reason |
| --- | --- | --- |
| Knurling and checker plate | 3 — Decline as proposed | Simple repeats duplicate existing patterns, or relief carries the proposed subject; prioritize connected/overlapping colour structures. |
| Perforated sheet | 3 — Decline as proposed | Simple repeats duplicate existing patterns, or relief carries the proposed subject; prioritize connected/overlapping colour structures. |
| Wire mesh, woven wire | 3 — Decline as proposed | Simple repeats duplicate existing patterns, or relief carries the proposed subject; prioritize connected/overlapping colour structures. |
| Chain-link fence and expanded metal mesh | 3 — Decline as proposed | Simple repeats duplicate existing patterns, or relief carries the proposed subject; prioritize connected/overlapping colour structures. |
| Grille | 3 — Decline as proposed | Simple repeats duplicate existing patterns, or relief carries the proposed subject; prioritize connected/overlapping colour structures. |
| Ribbed rubber | 3 — Decline as proposed | Simple repeats duplicate existing patterns, or relief carries the proposed subject; prioritize connected/overlapping colour structures. |
| Conveyor belt and tyre tread | 3 — Decline as proposed | Simple repeats duplicate existing patterns, or relief carries the proposed subject; prioritize connected/overlapping colour structures. |
| Tool marks and milling marks | 3 — Decline as proposed | Simple repeats duplicate existing patterns, or relief carries the proposed subject; prioritize connected/overlapping colour structures. |
| Lathe marks | 1 — Represented | Existing rings/shell bands provide this colour pattern. |
| Weld bead | 3 — Decline as proposed | Simple repeats duplicate existing patterns, or relief carries the proposed subject; prioritize connected/overlapping colour structures. |
| Carbon-fibre weave | 3 — Decline as proposed | Simple repeats duplicate existing patterns, or relief carries the proposed subject; prioritize connected/overlapping colour structures. |
| Honeycomb | 2 — Recommend | Use the single regular hex/honeycomb prototype, rather than a second identical generator. |

### Wear, ageing and damage

| Audit idea | Group | Decision reason |
| --- | --- | --- |
| Fine scuffs | 3 — Decline as proposed | Generic masked overlays repeat existing corrosion and mask studies; use the boundary-correlated wear representative. |
| Chips, flaking, peeling (mask only), crazing, cracks, pitting | 3 — Decline as proposed | Generic masked overlays repeat existing corrosion and mask studies; use the boundary-correlated wear representative. |
| Tarnish, bleaching, sun fading | 3 — Decline as proposed | Generic masked overlays repeat existing corrosion and mask studies; use the boundary-correlated wear representative. |
| Mould | 3 — Decline as proposed | Generic masked overlays repeat existing corrosion and mask studies; use the boundary-correlated wear representative. |
| Moss/algae | 1 — Represented | Mossy stone and independent mask/composition examples already supply this idiom. |
| Water stains and tide lines | 3 — Decline as proposed | Generic masked overlays repeat existing corrosion and mask studies; use the boundary-correlated wear representative. |
| Mineral deposits | 3 — Decline as proposed | Generic masked overlays repeat existing corrosion and mask studies; use the boundary-correlated wear representative. |
| Edge wear in tiled materials | 2 — Recommend | Wear tied to generated tile/mortar boundaries is valid RGB composition. |

### Decorative and graphic manufactured patterns

| Audit idea | Group | Decision reason |
| --- | --- | --- |
| Polka dots and halftone | 2 — Recommend | Static dots duplicate medallions; recommend field-controlled halftone only. |
| Chevrons, zigzags | 3 — Decline as proposed | Simple stripes/grid/chevron variants repeat current motifs without a new lesson. |
| Greek key | 2 — Recommend | A distinctive connected ornament, useful in the second batch. |
| Truchet | 2 — Recommend | Legal per-cell motif choices with matching exits demonstrate global structure. |
| Digital camouflage | 2 — Recommend | A clear domain-quantisation example. |
| Islamic geometric | 2 — Recommend | Specify an eight-point star lattice; do not promise the whole pattern class. |
| Hex grids | 3 — Decline as proposed | Simple stripes/grid/chevron variants repeat current motifs without a new lesson. |
| Dither | 2 — Recommend | Optional Bayer threshold companion to halftone, not screen-space postprocessing. |

### Food surfaces

| Audit idea | Group | Decision reason |
| --- | --- | --- |
| Bread crumb and cake crumb | 3 — Decline as proposed | A food-coloured version of familiar bands, pores, cells or noise; choose distinctive salami or radial fruit structure first. |
| Toast | 3 — Decline as proposed | A food-coloured version of familiar bands, pores, cells or noise; choose distinctive salami or radial fruit structure first. |
| Cheese holes | 3 — Decline as proposed | A food-coloured version of familiar bands, pores, cells or noise; choose distinctive salami or radial fruit structure first. |
| Blue cheese | 3 — Decline as proposed | A food-coloured version of familiar bands, pores, cells or noise; choose distinctive salami or radial fruit structure first. |
| Salami | 2 — Recommend | One rounded-inclusion food cross-section; avoid duplicating conglomerate. |
| Chocolate | 3 — Decline as proposed | A food-coloured version of familiar bands, pores, cells or noise; choose distinctive salami or radial fruit structure first. |
| Citrus peel | 3 — Decline as proposed | A food-coloured version of familiar bands, pores, cells or noise; choose distinctive salami or radial fruit structure first. |
| Apple skin | 3 — Decline as proposed | A food-coloured version of familiar bands, pores, cells or noise; choose distinctive salami or radial fruit structure first. |
| Watermelon rind | 3 — Decline as proposed | A food-coloured version of familiar bands, pores, cells or noise; choose distinctive salami or radial fruit structure first. |
| Melon netting | 3 — Decline as proposed | A food-coloured version of familiar bands, pores, cells or noise; choose distinctive salami or radial fruit structure first. |
| Strawberry seeds | 3 — Decline as proposed | A food-coloured version of familiar bands, pores, cells or noise; choose distinctive salami or radial fruit structure first. |
| Kiwi | 2 — Recommend | A radial fruit cross-section; revise the literal atan2-based recipe for current semantics. |
| Pineapple | 3 — Decline as proposed | A food-coloured version of familiar bands, pores, cells or noise; choose distinctive salami or radial fruit structure first. |
| Onion layers | 3 — Decline as proposed | A food-coloured version of familiar bands, pores, cells or noise; choose distinctive salami or radial fruit structure first. |
| Coffee crema | 3 — Decline as proposed | A food-coloured version of familiar bands, pores, cells or noise; choose distinctive salami or radial fruit structure first. |
