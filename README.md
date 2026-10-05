# procedural-textures

Procedural texture playground in Haskell. It defines a small algebra of
texture primitives (flat, linear, radial, circular, Perlin noise, turbulence,
tiled, layered) and a flexible colour ramp system with clamped, wrapped, and
mirrored modes plus multi-stop discontinuous ramps.

Textures are plain data (`Texture` values) interpreted to pixel functions in a
single place, and rendered to PNG with JuicyPixels. They are saved as JSON
documents; the examples live in `examples/*.json`.

Where this is heading is described in [`PLAN.md`](PLAN.md), the master plan.
[`IDEAS.md`](IDEAS.md) is a scratchpad of ideas.

## Layout

- `src/` the library:
  - `Colours` CSS colour constants and RGB helpers.
  - `ColourRamps` ramp modes and evaluation across arbitrary stops.
  - `Perlin` 2D Perlin noise.
  - `Texture` the texture ADT and its interpreter.
  - `Render` JuicyPixels adapter and image writer.
  - `HtmlOutput` the static HTML gallery.
  - `PNGCompareCore` image comparison used by `png-compare`.
  - `TextureJson` the JSON document format.
  - `Examples` loads the example documents from `examples/`.
  - `Schema` describes the texture language for the editor (node types,
    fields, widget kinds, ranges, defaults).
  - `Server` the editor's backend (rendering API and static files).
- `app/` executables: `procedural-textures` (CLI), `texture-server` (the
  editor backend), `png-compare`.
- `examples/` the example texture documents (the source of truth).
- `golden/` expected renders for the regression suite.
- `test/` the tasty test suite.

## Running

Render the examples as 128×128 PNGs into `out/` (`--size`, `--out` and
`--examples` change the defaults):
```
stack run procedural-textures
```

Render one document:
```
stack run procedural-textures -- render examples/marble.json marble.png --size 512
```

Rewrite documents in canonical form (the test suite checks that the examples
are canonical):
```
stack run procedural-textures -- format examples/*.json
```

Render the 512×512 gallery into `site/` (published to GitHub Pages by CI):
```
stack run procedural-textures -- gallery
```

Time each example at 128² and 512²:
```
stack run procedural-textures -- benchmark
```

Serve the rendering API on port 8080:
```
stack run texture-server
```
Its endpoints are `GET /api/schema`, `GET /api/examples`,
`POST /api/render?size=N` (document in, PNG out) and `POST /api/migrate`
(document of any version in, canonical document out).

Compare two PNGs. Prints the mean and maximum per-pixel RGBA distance, and
exits 1 if a threshold is given and exceeded:
```
stack run png-compare -- [--threshold MEAN] [--max-threshold MAX] a.png b.png
```

Run the tests:
```
stack test
```

## Texture documents

```json
{
  "version": 1,
  "name": "Checker",
  "description": "An 8 by 8 checkerboard.",
  "texture": {
    "type": "tiled",
    "columns": 8,
    "rows": 8,
    "a": {"type": "flat", "colour": "#e6e6e6ff"},
    "b": {"type": "flat", "colour": [0.1, 0.1, 0.1, 1]}
  }
}
```

Textures and ramps are objects tagged with `"type"`. Colours are
`"#rrggbbaa"` strings when exactly representable with 8-bit channels and
`[r, g, b, a]` arrays otherwise. `version` lets old documents be migrated when
the format changes.

## Golden images

`golden/textures/` holds the expected 128×128 render of every example. The
test suite renders each example and fails if it differs beyond a tight
tolerance (`defaultTolerance` in `PNGCompareCore`). This is the regression
suite for refactors and optimisations.

When a change to the images is intended, regenerate them and say why in the
commit:
```
stack test --ta --accept
```
