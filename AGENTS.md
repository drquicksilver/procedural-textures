# Agent Instructions

- `PLAN.md` is the master plan: work through its milestones in order. `IDEAS.md` is a loose scratchpad of ideas, not a plan.
- After making code changes, run `stack build` and then `stack test`, and address any issues that arise. For changes in `frontend/`, also run `npm test` and `npm run build` there (or `make test`).
- When adding new functionality, consider whether it should have tests and add them when appropriate.
- Commit working code, with a good commit message (at least a sentence per logical change, longer for big changes) at logical points when the code is working and tests pass.
- Keep the tests fast enough to run often. Migrate slow tests to a secondary suite used less often. The browser tests in `frontend/e2e/` are that secondary suite: run `make e2e` after changing editor behaviour.
- Golden files (`golden/`, `test-vectors/`) change only on purpose: regenerate with `stack test --ta --accept` and say why in the commit.

Project structure:
- `src/` core library (textures, ramps, rendering, Perlin, examples).
- `app/` executables (`procedural-textures`, `texture-server`, `png-compare`).
- `examples/` example texture documents (JSON, the source of truth); `golden/` expected renders.
- `test/` tasty test suite (`test/Spec.hs`).
- `frontend/` the web editor (TypeScript, Vite, Preact; vitest tests alongside the code, browser tests in `frontend/e2e/`).
- `test-vectors/` fixtures written by the Haskell suite and read by the frontend tests.
- `bench/` tasty-bench benchmarks (`stack bench`) and recorded results.
- `procedural-textures.cabal` package definition and build config.
- `stack.yaml`/`stack.yaml.lock` Stack resolver and lockfile.
