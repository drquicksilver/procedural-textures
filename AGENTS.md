# Agent Instructions

- After making code changes, run `stack build` and then `stack test`, and address any issues that arise.
- When adding new functionality, consider whether it should have tests and add them when appropriate.

Project structure:
- `src/` core library and executable modules (textures, ramps, rendering, Perlin).
- `test/` tasty test suite (`test/Spec.hs`).
- `procedural-textures.cabal` package definition and build config.
- `stack.yaml`/`stack.yaml.lock` Stack resolver and lockfile.
