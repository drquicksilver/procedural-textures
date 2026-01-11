# procedural-textures

Procedural texture playground in Haskell. It defines a small algebra of
texture primitives (linear, radial, circular, tiled, layered) and a flexible
colour ramp system with clamped, wrapped, and mirrored modes plus multi-stop
discontinuous ramps.

The executable renders a set of example textures to PNG files using
JuicyPixels. Image definitions live as `Texture` values and are interpreted to
pixel functions in a single place.

Current modules:
- `src/Colours.hs` standard CSS colour constants and RGB helpers.
- `src/ColourRamps.hs` ramp modes and evaluation across arbitrary stops.
- `src/Texture.hs` texture ADT and interpreter.
- `src/Render.hs` JuicyPixels adapter and image writer.

Build and run:
```
stack run
```
