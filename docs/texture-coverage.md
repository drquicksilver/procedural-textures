# Texture coverage audit

A wish-list of ~400 natural and manufactured textures, sorted against the
Phase 4 model (2026-10-07). The aim is a coverage suite: which textures the
system already demonstrates, which it can express with its current
primitives, and which need primitives it does not have.

Buckets:

- **Have**: an example already exists, or one does after only a ramp or
  parameter change (example named in brackets).
- **Can build**: expressible with today's primitives; the recipe is given.
  Items marked **(awkward)** are possible, but need long arithmetic chains;
  they show where better ergonomics would pay off.
- **Needs**: needs one of the new primitives N1–N13 defined below. When a
  rough version can be built today, the entry says so.

## What we have (summary)

| Category | Primitives |
|---|---|
| Noise | Perlin; fractal sums of any scalar (smooth, billowy, ridged); absolute-fractal turbulence |
| Cells | Worley F1/F2/F2−F1 (Euclidean, Manhattan, Chebyshev, 2D/3D, jitter, seed); `cell-value`, `cell-id`, `cell-colour`; exact `cell-edge` |
| Distance | SDF sphere/box/cylinder/torus/plane; hard and smooth union, intersection and difference |
| Simulation | Gray–Scott reaction–diffusion: a baked periodic volume, at most 64³ |
| Domain | translate, scale (so anisotropy), rotate, repeat, mirror, polar repeat, radial repeat, twist, bend, compose, vector warp |
| Scalar algebra | add, multiply, min, max, remap, smoothstep threshold |
| Vector | constant, position, components, add, scale-by-scalar, domain |
| Colour | ramps (clamp, wrap, mirror), checker, alpha-over, nine blend modes, scalar-masked mix, vector→RGB |

Useful derived idioms. They already work, and many recipes below rely on them:

- **sawtooth / fract**: `planar` sampled in a `repeat` domain. **triangle
  wave**: the same in a `mirror` domain, or with a `mirror` ramp. **round/floor**:
  `x − repeat(x)`. **abs**: `max(x, −x)`. **Pixelate a domain**: warp by
  `−fract(p)`.
- **Per-tile random**: `cell-value` with jitter 0 gives one random value per
  unit square, so it works as a tile ID for any lattice laid out with
  repeat/warp.
- **Running-bond offset**: warp x by `0.5·round(y)`, then `repeat`. The same
  trick gives twill diagonals and offset (hex-like) lattices.
- **Per-cell selection**: `mix(threshold(cell-value), A, B)` chooses between
  two whole sub-textures per cell (Truchet, basketweave, alternate bonds).
- **Hierarchical cells**: warp a fine Worley by `k · cell-id` of a coarse one.
  Each parent cell then gets its own child pattern; `min` of the two
  `cell-edge`s gives sub-cracks.
- **Overlapping regular scales**: alpha-over of several offset copies of a
  disc lattice. The layer order sets which scale lies on top.
- **Feature-centred rings**: bands of F1, so each Worley cell gets concentric
  rings (rosettes, turtle-scute growth lines, eye-spots).
- **Polar coordinates**: warp by `(distance − x, angular − y, 0)`. This gives
  radial/circular brushing and kiwi-style fibres (with a seam at the angular
  origin).

## Missing primitives

Ordered roughly by how much new expressive power each adds per unit of
implementation cost.

| ID | Primitive | Description | Kind |
|---|---|---|---|
| **N4** | **Gradient & relief** | Finite-difference gradient of a scalar, as a vector; a *relief/shade* colour op (height + light direction → lit colour); and the derived normalised iso-distance `\|f−t\|/\|∇f\|` and curl of noise. Many materials are mostly lit height (hammered metal, leather, stucco, knurling, crumpled paper). Today they can only be faked with ramps. | local, cheap (≈4× the subtree's cost) |
| **N2** | **Tile layouts** | A layout node that returns tile-local coordinates, tile ID/random, and distance to the tile edge for grid, running bond, English/Flemish bond, hex, herringbone, basketweave, Versailles, with per-tile jitter of size, position and rotation. Grid, running bond and basketweave are only ergonomics today. Herringbone, Versailles and per-tile size irregularity are genuinely new. Edge distance also drives edge wear. | local |
| **N3** | **Scatter / bombing** | Place k instances per cell of a child field with random offset, rotation, scale and colour, an optional density mask, and overlap ordering. Worley gives one *point* per cell; this gives one *object* per cell, of any shape. | local, k×cost |
| **N5** | **Orientation fields** | Rotate/scale a domain by a *field* rather than a constant. Add anisotropic (Gabor-style) noise that follows a vector field, for fibres and stripes along body or flow direction. | local |
| **N1** | **Scalar maths** | sin/cos, abs, pow/gamma, floor/fract, divide, clamp, scalar lerp. Mostly derivable already (see idioms). Sine is the real gap (guilloché, spirograph rosettes), and the rest saves long expression chains. | trivial |
| **N6** | **Non-local filters** | Blur, directional smear (drips and streaks *below* a source), dilate/erode, distance transform of an arbitrary mask. These need neighbourhood sampling or a baked raster, like the RD cache. | baked/expensive |
| **N7** | **Growth networks** | Tree-topology structures: space colonisation, DLA, L-systems. Gives tapering, hierarchical, mostly loop-free branches (venation, lightning, mycelium, dendritic frost). Ridged noise and Voronoi edges only give loopy networks. | baked |
| **N8** | **Sequential fracture** | Crack networks with T-junctions and hierarchy (each crack stops at an older one), unlike the Y-junctions of Voronoi. | baked |
| **N9** | **Surface attributes** (3D) | Normal, world-up, curvature, ambient occlusion and thickness as fields. Gives edge wear, dirt in crevices, dust/moss on upward faces, and rain streaks down walls. For flat 2D textures, N4 on the material's own height field is the substitute. | renderer |
| **N10** | **RD extensions** | 2D high-resolution RD; feed/kill as *fields* (spots grading into stripes); anisotropic diffusion (aligned stripes, fingerprints); seeding from a scalar; 1D-over-time automata for seashell pigmentation (Meinhardt). The current 64³ volume visibly repeats. | baked |
| **N11** | **Lighting beyond relief** | Gloss and specular, anisotropic highlights, refraction. Polished and anodised metal, wet sand and glaze read mainly through reflection, which is not really a texture. | renderer |
| **N12** | **Aperiodic tilings** | De Bruijn pentagrid → Penrose rhombs (pentagrid stripes alone are already possible). | local |
| **N13** | **Periodic noise** | Noise with an integer period. Gives seamless tiles and warps that respect a lattice (Escher-like interlocking tiles, tileable exports). | local |

## Classification

### Animal skin, fur and shells

- **Have:** Giraffe [`flowing-grout`, recoloured: warped `cell-edge`], Cowhide [`camouflage`, hard two-stop ramp], Zebra [`zebra`].
- **Can build:**
  - Leopard: F1 band rings per jittered cell, broken by an fBm mask; tinted centres.
  - Cheetah: F1 threshold, warped.
  - Jaguar: F1 ring plus a second, finer Worley masked to the ring interior.
  - Dalmatian: F1 threshold, with sparse cells (drop cells whose `cell-value` is above a threshold), warped edges.
  - Trout/salmon: small F1 spots over a `planar` gradient plus fBm.
  - Turtle shell: low-jitter `cell-edge` scutes plus F1 growth rings inside each.
  - Tortoiseshell: layered warped fBm with an amber ramp (as in `tiger-eye`).
  - Fish scales: overlapping offset disc lattices.
  - Snake skin: as fish scales, with a per-scale `cell-value` tint (true irregular overlap → N3).
  - Crocodile: high-jitter Chebyshev `cell-edge` plus hierarchical cells (better with N4).
  - Butterfly wing: mirror fold (`butterfly-fold`), `cell-edge` veins, F1 eye-spots with a wrapped ramp.
  - Peacock eye: nested anisotropic distance bands, warped; barbs from wrapped `angular`.
- **Needs:**
  - Tiger: today's `tiger-fur` is close, but stripes that *follow body direction* and taper → **N5**.
  - Seashell: growth bands and radial ridges can be built now; Conus-style pigmentation → **N10**.

### Wood, bark and plant material

- **Have:** Straight grain [`walnut`], Wavy grain [warped `walnut`], Knotted wood [`wood-knot`, `masked-knot`], Tree rings [`rings`], Birch bark [`birch-bark`], Oak bark [`bark`, ridged and vertically stretched], Moss [`moss`].
- **Can build:**
  - Quarter-sawn ray fleck: anisotropically scaled Worley threshold layered on grain.
  - Burl: dense warped F1 "eyes" plus turbulence.
  - Bamboo: Y repeat with node bands (box SDF) plus stretched noise.
  - Cork: multiscale F1 thresholds plus speckle.
  - Pine bark: vertically stretched `cell-edge` plates plus ridged fBm (depth → N4).
  - Plane/sycamore bark: high-jitter cells with `cell-value` → multi-colour ramp, warped.
  - Eucalyptus: vertically stretched, warped fBm layers with alpha.
  - Palm trunk: rotated, repeated chevron bands (mirror + repeat) plus fibres.
  - Leaf surface: ridged/hierarchical `cell-edge` "veins" plus fine cells.
  - Lichen: thresholded fBm rosettes with F1 rings.
  - Dried leaf: the same plus mottling (wrinkles → N4).
  - Woven straw: weave idiom (checker of two stretched-noise orientations).
  - Bracket fungus: warped concentric distance bands under an SDF mask.
- **Needs:** Leaf skeleton / true venation → **N7**.

### Stone, minerals and geology

- **Have:** Marble [`marble`], Granite [`granite`], Diorite and Gabbro [`granite` with other ramps and scales], Sandstone [`sandstone`], Shale [`oblique-strata`, warped], Agate [`agate`, `box-agate`], Malachite [`malachite`], Onyx [`onyx`], Terrazzo [`seeded-terrazzo`], Breccia [`brecciated-marble`], Lava/basalt [`lava`, `volcanic-cells`], Weathered pitted stone [`weathered-relic`].
- **Can build:**
  - Limestone: soft fBm plus small sparse F1 pits.
  - Slate: strongly stretched fBm lamination (cleavage relief → N4).
  - Travertine: horizontal bands plus stretched F1 pores.
  - Quartz/crystalline: cells with a `cell-value` facet shade.
  - Jasper: multiscale warped fBm with a hard multi-stop ramp.
  - Conglomerate: F1-thresholded rounded inclusions in an fBm matrix (size variety → N3).
  - Pumice: multiscale F1 holes.
  - Obsidian: warped, stretched dark bands.
  - Columnar basalt: low-jitter `cell-edge`, viewed end-on.
  - Cracked mud: warped `cell-edge` plus a per-cell tint.
  - Rock face: ridged fBm plus `cell-edge` fractures.
- **Needs:** Realistic cracked mud with T-junctions → **N8**; any lit rock face → **N4**.

### Terrain, soil and granular materials

- **Have:** Wind-blown ripples [`sand-ripples`], Dunes [`dunes`], Ice [`ice`], Frost crystals [`cellular-frost`, `nested-fractal-frost`].
- **Can build:**
  - Fine sand: high-frequency noise speckle. Coarse sand: small cells with `cell-colour`.
  - Wet beach sand: dark smooth fBm plus speckle (sheen → N11).
  - Pebbles/shingle and gravel: F1-gap rounded cells with a `cell-value` tint (varied sizes and overlap → N3).
  - Scree: angular Chebyshev cells.
  - Soil, rich loam, clay, mud, ash, volcanic soil: multiscale fBm plus speckle with suitable ramps.
  - Dried cracked earth: as cracked mud.
  - Salt flats: polygonal `cell-edge` ridges.
  - Snow: soft fBm plus sparse high-frequency glints. Wind-packed snow: stretched ridged noise.
  - Glacier ice: `ice` plus `cell-edge` cracks and F1 bubbles.
- **Needs:** Dendritic frost ferns → **N7**.

### Water, atmosphere and fluid-like patterns

- **Have:** Calm ripples [`water-ripples`], Ocean waves [`ocean`], Caustics [`caustics`], Foam [`rd-incipient-foam`], Oil-film interference [`vector-iridescence`, `interference-rings`], Cirrus [`cirrus`], Cumulus [`cumulus`], Storm [`storm`], Fog [`fog`], Smoke [`smoke`], Fire [`fire`], Plasma [`warped-turbulence-sum`, `nebula`].
- **Can build:**
  - Choppy water: ridged fBm with directional stretch.
  - Sea foam cells: warped `cell-edge` network with a broken mask.
  - River turbulence: stretched turbulence warp (true curl flow → N4).
- **Needs:** Lightning → **N7** (ridged-noise approximation possible today).

### Biological / microscopic / reaction patterns

- **Have:** Cells and Voronoi tissue [`worley-*`, `cellular-web`], Cracked membranes [`rd-warped-membrane`], RD spots and stripes [`rd-*` with Gray–Scott parameters], Coral [`rd-coral`], Bacterial colony [`rd-gilded-colony`].
- **Can build:**
  - Brain coral: RD labyrinth regime, or warped ridged noise.
  - Sponge: 3D F1 multiscale holes.
  - Slime mould: loopy networks, so warped `cell-edge` with variable width works.
  - Skin pores: small sparse F1.
  - Wrinkles: warped ridged noise (lit → N4).
  - Trabecular bone: 3D Worley gap threshold.
- **Needs:**
  - Veins/vascular networks, capillaries, mycelium → **N7**.
  - Fingerprints → **N5** or **N10** (anisotropic RD / orientation field).

### Brick, masonry and architectural surfaces

- **Have:** Rough concrete [`concrete`].
- **Can build:**
  - Running bond: running-bond idiom, box SDF in a repeat for mortar, `cell-value` per brick. Stack bond: plain repeat.
  - English bond **(awkward)**: mix two layouts by row parity.
  - Flemish bond **(awkward)**: two brick lengths inside one period.
  - Old handmade brick: warped layout plus per-brick tint (size jitter → N2).
  - Weathered brick and painted brick: threshold-noise layers over brick.
  - Mortared stone, random rubble, dry-stone: high-jitter `cell-edge` mortar plus per-cell tint.
  - Coursed stone: per-row horizontally stretched cells.
  - Ashlar and concrete blocks: bonds with chamfered box SDFs.
  - Smooth cast concrete: low-contrast fBm plus F1 bugholes.
  - Board-marked concrete: plank repeat with grain and a per-board offset.
  - Aggregate concrete: terrazzo idiom.
  - Cracked concrete: concrete plus warped ridged cracks.
  - Stucco/render, Pebbledash: noise and F1 dots (lit → N4).
- **Needs:**
  - Herringbone brick → **N2**.
  - Efflorescence below mortar and weathering streaks → **N6**.
  - Most masonry gains a lot from **N4**.

### Tiles, paving and geometric coverings

- **Have:** Checkerboard [`checker`], Mosaic and irregular mosaic [`prismatic-mosaic`], Encaustic/cement tiles [`offset-medallions`-style motif], Parquet basketweave [`ordered-cell-quilt`-style].
- **Can build:**
  - Square ceramic tiles: repeat plus box SDF grout.
  - Subway tiles: running bond.
  - Octagon-and-dot: box ∩ rotated box in a repeat, plus an offset small square.
  - Basketweave: checker selecting two stripe orientations.
  - Penny tiles: offset disc lattice.
  - Terracotta and slate floor: grid plus per-tile `cell-value` tint and noise.
  - Flagstones and crazy paving: high-jitter `cell-edge` plus per-cell tint.
  - Cobblestones: F1-gap rounded cells. Setts: small running bond with rounded boxes.
  - Laminate floorboards: long running bond plus grain.
  - Hexagonal tiles **(awkward)**: two offset repeats of a mirror-folded plane hexagon.
- **Needs:** Herringbone tiles, Herringbone parquet, Versailles parquet → **N2**.

### Ceramics, glass and enamel

- **Have:** Stained glass [`prismatic-mosaic`].
- **Can build:**
  - Glazed ceramic, porcelain, stoneware: flat colour plus subtle noise.
  - Crackle glaze and crazed enamel: hierarchical `cell-edge`.
  - Speckled glaze: sparse F1 specks.
  - Reactive glaze: warped fBm (vertical runs → N6).
  - Raku: crackle plus smoky fBm.
  - Frosted glass: alpha speckle.
  - Seeded glass: small, slightly stretched F1 bubbles.
  - Chipped enamel: threshold-noise mask.
- **Needs:**
  - Hammered/textured glass → **N4** (dimple relief, and refraction as a warp by the gradient).
  - Dirty/streaked glass → **N6** (a stretched-noise approximation is possible today).
  - Any convincing gloss → **N11**.

### Metals

- **Have:** Brushed aluminium [`brushed-steel`], Brass [`brushed-gold`], Rusted iron [`rust`], Verdigris [`verdigris`], Copper [`toroidal-copper`], Patchy oxidation [`nested-corrosion`].
- **Can build:**
  - Circular brushed metal: stretched noise in a polar-coordinate warp.
  - Galvanised steel: high-jitter cells with a `cell-value` sheen plus fine streaks.
  - Cast iron: dark fine noise plus pits.
  - Tarnished brass and bronze patina: masked layering of oxidation over metal.
  - Anodised aluminium: flat colour plus faint brushing.
  - Machined/toolpath: radial-repeat arcs in an offset lattice.
  - Perforated metal: alpha disc lattice.
  - Diamond plate: rotated SDF lozenges, alternated by checker.
  - Corrugated metal: mirror-wrapped `planar`.
- **Needs:**
  - Hammered metal → **N4**.
  - Scratched metal → **N3** (several rotated, thresholded stretched-ridge layers approximate it).
  - Polished steel → **N11**.
  - Diamond plate and corrugated metal really want **N4**.

### Fabric and textiles

- **Have:** none directly (`crosscurrent-silk` and `twisted-brocade` are adjacent).
- **Can build:**
  - Plain weave: checker selecting horizontal or vertical stretched-noise threads, with mirror-ramp shading per thread.
  - Twill, denim, carbon-fibre-style: diagonal offset via floor-warp.
  - Herringbone fabric: twill mirrored per column.
  - Linen, canvas, hessian: weave plus slub noise and irregular warp.
  - Corduroy: stretched ribs.
  - Tweed: twill plus `cell-colour` flecks.
  - Knitted fabric: mirrored V-loops (SDF) in an offset repeat.
  - Cable knit **(awkward)**: knit warped by a column triangle wave.
  - Crochet and lace **(awkward)**: polar-repeat motifs with alpha.
  - Carpet: speckle plus cells. Loop-pile carpet: offset F1 ring lattice.
  - Felt, wool, suede-like: fine turbulence.
  - Quilting: rotated grid lines.
  - Paisley: bent and warped teardrop SDF in an offset lattice.
  - Batik: thin warped `cell-edge` crackle plus dye blotches.
  - Tie-dye: polar-repeat warped bands.
  - Tartan: summed `planar` stripe masks in X and Y, over twill.
  - Velvet: soft noise (sheen → N11).
- **Needs:** Fur/fleece and convincing wool fibres → **N5**; quilting puff → **N4**.

### Leather, hide and paper-like materials

- **Have:** Parchment [`parchment`].
- **Can build:**
  - Smooth leather and suede: low-contrast fBm.
  - Pebbled leather: F1-gap cells.
  - Cracked aged leather: warped `cell-edge`.
  - Office paper, cardboard, handmade paper: fine turbulence plus fibres approximated by stretched noise.
  - Corrugated cardboard: stripes.
  - Recycled paper flecks: sparse F1 with `cell-colour`.
  - Crumpled paper: flat-shaded facets via `cell-value` in warped high-jitter cells.
  - Torn paper: alpha threshold of turbulence.
  - Marbled paper: chained comb warps (triangle-wave vector warps; cf. `swirly-stripes`).
- **Needs:** Lit pebbling and creases → **N4**; real fibres in handmade paper → **N3**.

### Paint, plaster and coated surfaces

- **Have:** Whitewash and limewash [`watercolour`-style].
- **Can build:**
  - Smooth painted wall: flat plus faint noise. Roller-painted wall: stretched stipple.
  - Spray paint: fine speckle under a soft mask.
  - Stippled paint: F1 dots.
  - Chipped paint: threshold-noise mask over substrate.
  - Layered old paint: several threshold levels of one noise.
  - Craquelure: hierarchical `cell-edge`.
  - Venetian plaster: multiscale smooth warped fBm.
  - Rough plaster: noise.
  - Wallpaper: damask motif from mirror, polar repeat and SDF in repeat.
  - Flocked wallpaper: the same with fuzzy noise in the motif.
- **Needs:**
  - Brush strokes → **N3** (oriented stroke stamps) and **N5**.
  - Peeling paint: curled lips and shadows → **N6** / **N4**; the mask itself can be built.
  - Rough plaster and flock relief → **N4**.

### Plastics, rubber and manufactured composites

- **Have:** none named, but most are trivial.
- **Can build:**
  - Smooth moulded plastic, ABS, soft-touch, rubber, tyre rubber, Formica, Corian: flat colour plus fine noise.
  - Injection-moulded stipple: fine F1.
  - Carbon fibre and Kevlar: twill weave idiom.
  - Fibreglass: sum of several rotated anisotropic noises.
  - Speckled laminate and engineered quartz: terrazzo/speckle idioms.
  - Foam: F1 cells. Expanded polystyrene: bead cells (`cell-edge`) plus inner texture.
- **Needs:** Stipple and orange-peel relief → **N4**; gloss differences between these finishes → **N11**.

### Mechanical and industrial patterns

- **Can build:**
  - Knurling and checker plate: two rotated mirror-repeats.
  - Perforated sheet: alpha disc lattice.
  - Wire mesh, woven wire: weave with alpha.
  - Chain-link fence and expanded metal mesh: rotated diamond repeats with over/under.
  - Grille: repeat plus box SDF.
  - Ribbed rubber: stripes.
  - Conveyor belt and tyre tread: mirrored chevron SDF blocks in a repeat.
  - Tool marks and milling marks: offset arc lattice.
  - Lathe marks: `rings`.
  - Weld bead: overlapping crescents along a line (the 1D fish-scale idiom).
  - Carbon-fibre weave: twill.
  - Honeycomb **(awkward)**: as hex tiles.
- **Needs:** Almost every item here is relief-dominated → **N4**; Honeycomb is clean with **N2**.

### Wear, ageing and damage

The layering itself works today: any wear effect is a `mix`/blend over an
arbitrary base. The gaps are in *where* wear goes.

- **Have:** Rust [`rust`].
- **Can build:**
  - Fine scuffs: faint stretched noise.
  - Chips, flaking, peeling (mask only), crazing, cracks, pitting: threshold masks, `cell-edge`, sparse F1.
  - Tarnish, bleaching, sun fading: gradient or fBm mask with multiply/screen blends.
  - Mould: clustered sparse F1 under an fBm mask.
  - Moss/algae: `moss` through a mask.
  - Water stains and tide lines: bands of thresholded warped noise (contour idiom).
  - Mineral deposits: crusty threshold layers.
  - Edge wear in tiled materials: distance-to-mortar is already available from the SDF.
- **Needs:**
  - Scratches, deep gouges → **N3** (+ **N4**).
  - Mud splatter → **N3** (warped-F1 approximation possible today).
  - Fingerprints/grease → **N3** + **N5**.
  - Edge wear and repeated-contact polishing on objects → **N9**.
  - Dirt accumulation in crevices → **N9**, or **N4**/**N6** on the base height.
  - Dust on upward faces → **N9**.
  - Soot and efflorescence streaks → **N6**.

### Decorative and graphic manufactured patterns

- **Have:** Stripes and pinstripes [`stripes`], Waves [`wobbly-stripes`], Concentric circles [`rings`], Voronoi mosaic [`prismatic-mosaic`], Op-art and Moiré [`moire`, `interference-rings`], Camouflage [`camouflage`].
- **Can build:**
  - Polka dots and halftone: disc lattice, with radius driven by a field for halftone.
  - Chevrons, zigzags: mirror plus repeat.
  - Greek key: box SDFs in a repeat.
  - Truchet: per-cell selection idiom.
  - Digital camouflage: pixelated-domain fBm.
  - Islamic geometric: p4m patterns from mirror and polar repeat (p6m → N2).
  - Hex grids **(awkward)**: as hex tiles.
  - Dither **(awkward)**: Bayer matrix from summed parity terms.
- **Needs:**
  - Guilloché → **N1** (sine).
  - Penrose tiling → **N12**.
  - Escher-like interlocking tiles → **N13**.

### Food surfaces

- **Have:** Marbled chocolate [`marble`, recoloured].
- **Can build:**
  - Bread crumb and cake crumb: multiscale stretched F1 holes.
  - Toast: crumb plus browning gradient.
  - Cheese holes: sparse F1.
  - Blue cheese: ridged warped veins.
  - Salami: sparse `cell-value` inclusions.
  - Chocolate: flat colour.
  - Citrus peel: dense F1 pores.
  - Apple skin: vertical streaks plus lenticel dots.
  - Watermelon rind: warped stripes (zebra idiom).
  - Melon netting: `cell-edge` network.
  - Strawberry seeds: offset dot lattice.
  - Kiwi: polar-warped radial fibres plus a seed ring.
  - Pineapple: rotated diamond lattice.
  - Onion layers: `rings`.
  - Coffee crema: warped fBm plus fine F1 bubbles.
- **Needs:** Bread crust, chocolate and citrus relief → **N4**.

## Tally

Approximate counts over the ~400 items:

- **Have:** ~75.
- **Can build:** ~270, of which ~12 are awkward.
- **Needs:** ~55 (some need more than one primitive).

| Primitive | Items it unlocks or substantially improves |
|---|---|
| N4 relief | ~45 (hammered/corrugated/diamond metal, leather, stucco, crumpled paper, mechanical, food crusts, lit rock) |
| N3 scatter | ~15 (scratches, gouges, fibres, splatter, brush strokes, pebble/scale size variety) |
| N2 layouts | ~12 (herringbone ×3, Versailles, hex/honeycomb, Flemish/English, handmade irregularity) |
| N5 orientation | ~8 (tiger, fur, wool, fingerprints, brush strokes, grease) |
| N6 non-local | ~8 (streaks, efflorescence, soot, peeling lips, reactive-glaze runs) |
| N7 growth | ~7 (venation, vessels, capillaries, mycelium, lightning, frost ferns) |
| N9 surface attributes | ~6 (edge wear, polishing, dust, crevice dirt) |
| N11 lighting | ~6 (polished steel, glaze, wet sand, velvet, plastics) |
| N1, N8, N10, N12, N13 | 1–2 each |

Main finding: the Phase 4 core is already expressive. Cells, distance
fields, warps, layering and RD cover the large majority, often through
idioms rather than first-class nodes. The genuine gaps are:

1. **Shading:** treating a scalar as height (N4, then N11).
2. **Placing objects rather than points** (N3).
3. **Following a direction field** (N5).
4. **Non-local or historical processes:** growth, fracture order, things
   that flow downhill (N6–N8).

N2 and N1 are mostly ergonomics, but heavily used ergonomics.

## Capability matrix: compact benchmark suite

● = essential, ○ = helps. Columns are capabilities, not nodes. **Gaps**
names the missing primitives (beyond today's) the texture needs to be
convincing.

| Texture | Warp | Aniso | Cells | SDF | Layout | Multiscale | Dir. field | Cracks | RD | Masks/layers | Relief | Scatter | Non-local | Status | Gaps |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| Granite | | | ● | | | ● | | | | ● | | ○ | | Have | — |
| Marble | ● | | | | | ● | | ○ | | ● | | | | Have | — |
| Sandstone | ○ | ● | | | | ● | | | | ● | | | | Have | — |
| Wood grain | ● | ● | | | | ○ | ○ | | | | | | | Have | N5 (fibre flow) |
| Wood knot | ● | ● | | ● | | | ● | | | ● | | | | Have | N5 |
| Oak bark | ● | ● | ○ | | | ● | | ● | | ● | ● | | | Have (flat) | N4 |
| Giraffe | ● | | ● | | | | | ● | | ○ | | | | Have | — |
| Leopard | ● | | ● | ● | | | | | | ● | | ○ | | Build | N3 (oriented rosettes) |
| Zebra | ● | ○ | | | | ○ | ● | | ○ | | | | | Have | N5 |
| Snake scales | ○ | | ● | ● | ● | | ○ | | | ● | ○ | ○ | | Build | N2, N3 |
| Cracked mud | ● | | ● | | | ○ | | ● | | ● | ○ | | | Build | N8 (T-junctions) |
| Sand ripples | ● | ● | | | | ○ | ○ | | | | ○ | | | Have | — |
| Water caustics | ● | | ● | | | ● | | | | | | | | Have | — |
| Clouds | ● | | | | | ● | | | | ● | | | | Have | — |
| RD coral | ○ | | | | | | | | ● | ● | ○ | | | Have | N10 (no tiling, graded params) |
| Brickwork | ○ | | ● | ● | ● | | | | | ● | ○ | | ○ | Build | N2, N6 (efflorescence) |
| Random stone masonry | ○ | | ● | ● | | ○ | | | | ● | ● | | | Build | N4 |
| Hex tiles | | | | ● | ● | | | | | ● | ○ | | | Build (awkward) | N2 |
| Parquet | | ● | | ● | ● | | ○ | | | ● | | | | Build | N2 (herringbone / Versailles) |
| Brushed metal | | ● | | | | ○ | ○ | | | | ○ | ○ | | Have | N11 (anisotropic highlight) |
| Rust | ● | | ○ | | | ● | | | | ● | ○ | | ○ | Have | N6 (run-off streaks) |
| Denim | | ● | | | ● | ○ | | | | ● | ○ | | | Build | — |
| Leather | ○ | | ● | | | ● | | ○ | | ● | ● | | | Build | N4 |
| Crackle glaze | ○ | | ● | | | ● | | ● | | ● | | | | Build | N8 (optional) |
| Concrete | | | ○ | | | ● | | ○ | | ● | ○ | ○ | | Have | — |
| Peeling paint | ● | | | | | ● | | ○ | | ● | ● | | ● | Build (mask) | N4, N6 |

Reading the columns:

- Warp, multiscale noise, cells and masks/layers are the workhorses; they are
  all in place.
- Layout and relief are the most-demanded *missing* capabilities in this suite.
- Directional fields, scatter and non-local effects each block a few
  textures that are hard to fake.

## Suggested order

1. **N4 gradient + relief.** Cheap, local and GPU-friendly, with the widest
   reach. It also gives curl noise (river turbulence, marbled paper) and
   normalised iso-distance (stain rims, peeling-paint lips without N6).
2. **N2 tile layouts.** Unlocks a whole family (brick, tile, parquet, fabric,
   honeycomb), and its edge-distance output gives tile-aware wear.
3. **N1 scalar maths.** Trivial. Shortens many recipes above and adds sine.
4. **N3 scatter.** The biggest *qualitative* addition: objects, not just points.
5. **N5 orientation fields / Gabor noise.**
6. **Baked processes (N6, N7, N8, N10):** these extend the RD cache machinery
   (deterministic bake, worker preparation, bounded caches, CPU/GPU fixtures).
7. **N9, N11:** renderer-side, best paired with the Phase 2 scene renderer.

As a coverage suite, building the benchmark table's "Build" rows as examples
first would test the idioms above. Each "awkward" recipe written along the way
is evidence for N1/N2.
