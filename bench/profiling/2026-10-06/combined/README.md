# Combined optimisation evidence

See [the analysis](../../../PERFORMANCE-COMBINED-2026-10-06.md).

Compare three detached worktrees from the independent study: the unchanged
baseline (`6158fdd`), octave-coordinate inlining (`068e66c`), and a combined
candidate with inlining, opaque-layer skipping and same-domain displacement
sharing. The combined candidate uses the same normal `-O1` build, ten RTS
capabilities, four-row task chunks and PNG settings as the controls.

`combined.patch` contains the evaluator and focused regression tests relative
to the baseline. To reconstruct, create three detached worktrees at `f62239a`,
apply `../worktrees/common-probe.patch` to all three, then
`../worktrees/inline.patch` to `inline` and `combined.patch` to `combined`.
Build and test each worktree with `stack build && stack test`, then compile its
probe with:

```sh
mkdir -p out/probe
stack exec ghc -- -O1 -Wall -threaded -rtsopts -package procedural-textures \
  -outputdir out/probe bench/profiling/Profile.hs -o out/probe/probe
```

From the main repository run:

```sh
python3 bench/profiling/measure-worktrees.py \
  --root /tmp/procedural-texture-experiments-20261006 \
  --out out/performance-combined-reproduction \
  --variants baseline inline combined --skip-scheduling
```

Use your own worktree parent path when reconstructing. The driver snapshots
all 67 baseline examples at 1, 3, 13, 96 and 512 square pixels and checks exact
RGBA bytes in both candidates. It then interleaves cases and variants with a
fixed shuffle seed. The primary 512² render-and-PNG outcome has five samples
of five operations; 512² render-only and 96² render-and-PNG have three samples
of five and forty operations respectively. Startup, document resolution and
warm-up are excluded; final collection is included. No builds or tests run
concurrently with timing. CPU time sums across cores, and allocated decimal
MB means cumulative allocation rather than resident memory.

`measurements.csv` is the raw timing data, `pixel-verification.csv` records
exact-image counts, `summary.json` contains median summaries and geometric
mean comparisons, and `revisions.json` identifies the source commits.
Original independent results and the full-library historical baseline are
preserved. Timings will vary on another machine or run.
