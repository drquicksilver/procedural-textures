# procedural-textures

Procedural texture playground in Haskell. It defines a small algebra of
texture primitives (flat, linear, radial, circular, Perlin noise, turbulence,
tiled, layered) and a flexible colour ramp system with clamped, wrapped, and
mirrored modes plus multi-stop discontinuous ramps.

Textures are plain data (`Texture` values) interpreted to pixel functions in a
single place, and rendered to PNG with JuicyPixels.

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
  - `Examples` the example textures.
- `app/` executables.
- `test/` the tasty test suite.

## Running

Render the examples as 128×128 PNGs into `out/`:
```
stack run procedural-textures
```

Render the 512×512 gallery into `site/` (published to GitHub Pages by CI):
```
stack run procedural-textures -- --html
```

Time each example at 128² and 512²:
```
stack run procedural-textures -- --benchmark
```

Compare two PNGs. Prints the mean and maximum per-pixel RGBA distance, and
exits 1 if a threshold is given and exceeded:
```
stack run png-compare -- [--threshold MEAN] [--max-threshold MAX] a.png b.png
```

Run the tests:
```
stack test
```

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
