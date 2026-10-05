# Agent Instructions

- `PLAN.md` is the master plan: work through its milestones in order. `IDEAS.md` is a loose scratchpad of ideas, not a plan.
- After making code changes, run `stack build` and then `stack test`, and address any issues that arise.
- When adding new functionality, consider whether it should have tests and add them when appropriate.
- Commit working code, with a good commit message (at least a sentence per logical change, longer for big changes) at logical points when the code is working and tests pass.
- Keep the tests fast enough to run often. Migrate slow tests to a secondary suite used less often.

Project structure:
- `src/` core library and executable modules (textures, ramps, rendering, Perlin).
- `test/` tasty test suite (`test/Spec.hs`).
- `procedural-textures.cabal` package definition and build config.
- `stack.yaml`/`stack.yaml.lock` Stack resolver and lockfile.
