# Agent Instructions

- `PLAN.md` is the master plan: work through its milestones in order. `IDEAS.md` is a loose scratchpad of ideas, not a plan.
- After making code changes, run `stack build` and then `stack test`, and address any issues that arise. For changes in `frontend/`, also run `npm test` and `npm run build` there (or `make test`).
- When adding new functionality, consider whether it should have tests and add them when appropriate.
- Commit working code, with a good commit message (at least a sentence per logical change, longer for big changes) at logical points when the code is working and tests pass.
- Keep the tests fast enough to run often. Migrate slow tests to a secondary suite used less often.

Project structure:
- `src/` core library (textures, ramps, rendering, Perlin, examples).
- `app/` executables (`procedural-textures`, `texture-server`, `png-compare`).
- `examples/` example texture documents (JSON, the source of truth); `golden/` expected renders.
- `test/` tasty test suite (`test/Spec.hs`).
- `frontend/` the web editor (TypeScript, Vite, Preact; vitest tests alongside the code).
- `procedural-textures.cabal` package definition and build config.
- `stack.yaml`/`stack.yaml.lock` Stack resolver and lockfile.
