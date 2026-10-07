# CI and Pages pipeline

The single `CI` workflow runs on pull requests, main pushes and manual dispatch.
Haskell and frontend tests build their outputs once. `gpu-samples` runs numerical
conformance and harness self-tests independently; `gpu-images` consumes the tested
CLI binaries for all golden comparisons. Both verify the pinned Chrome and Mesa
llvmpipe backend. Ubuntu 24.04 is pinned so the shared Linux executables have the
same runtime environment in each job.

The site job consumes the tested CLI and frontend artifacts, assembles the complete
static site, and runs all 27 browser workflows including gallery checks. It runs
alongside GPU conformance, on PRs as well as main. Only main publishes a Pages
artifact. The Publish site job depends on all five check/build jobs succeeding,
and deploys precisely that uploaded artifact; there is no second build or test run.

Stack dependency and project caches are separate. Project cache keys include
Haskell sources and tests. Building with `stack build --test --no-run-tests` before
`stack test` keeps component configuration consistent. Consumers unpack the tested
CLI tarball instead of installing GHC or rebuilding it. Locally the Pages assembler
uses the already installed CLI, so run `stack build` before `make pages` after
changing Haskell sources.

Gallery PNGs are cached under `out/gallery-cache`. SHA-256 input keys cover the
renderer/geometry source, build configuration, builtin ramps, each document's
version/texture/local ramps, output identity, view and resolution. A material edit
invalidates that material's slice, solid and shape images; renderer or builtin-ramp
edits conservatively invalidate all images. Frontend and description edits can
reuse the pixels. Cached PNG signatures and content checksums are checked before
reuse; missing or corrupt entries are rendered again. HTML is always regenerated.
The gallery CLI's `--reuse-images` flag is used only after the assembler validates
those inputs. Ordinary `gallery` runs still render every image.

Browser tests use an isolated browser context and one initial app load per test.
They wait for the main preview to render rather than for a fixed network-idle
period. Storage persistence within a test and explicit reload tests are preserved.
No numerical tolerance, image resolution or conformance coverage is reduced.

The first pipeline run fills the new gallery/project caches. Measure both cold
and warm runs against the same commit when evaluating performance. Compare total
job durations separately from publication latency: parallel jobs reduce elapsed
time even when runner-minutes remain substantial.
