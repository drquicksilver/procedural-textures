# Independent worktree experiment evidence

Read the [analysis](../../../PERFORMANCE-EXPERIMENTS-2026-10-06.md) for results.
The patches reconstruct the seven trees from `f62239a`; experiment commits and
absolute worktree paths are also in `revisions.json`. No treatment includes
another treatment. `common-probe.patch` adds identical exact-pixel validation
commands to the measurement harness. `batch.patch` also routes that harness
through its new backend, so it validates and times the implementation actually
being tested.

To reconstruct in new worktrees, run from the main repository root:

```sh
texture_perf_root=$(mktemp -d /tmp/texture-perf.XXXXXX)
texture_perf_patches="$PWD/bench/profiling/2026-10-06/worktrees"
for texture_perf_case in baseline o2 inline opaque shared batch schedule
do
  git worktree add --detach "$texture_perf_root/$texture_perf_case" f62239a
  git -C "$texture_perf_root/$texture_perf_case" apply "$texture_perf_patches/common-probe.patch"
  if [ "$texture_perf_case" != baseline ]
  then
    git -C "$texture_perf_root/$texture_perf_case" apply "$texture_perf_patches/$texture_perf_case.patch"
  fi
  (
    cd "$texture_perf_root/$texture_perf_case" || exit 1
    stack build && stack test || exit 1
    mkdir -p out/probe
    texture_perf_opt=-O1
    if [ "$texture_perf_case" = o2 ]
    then
      texture_perf_opt=-O2
    fi
    stack exec ghc -- "$texture_perf_opt" -Wall -threaded -rtsopts \
      -package procedural-textures -outputdir out/probe \
      bench/profiling/Profile.hs -o out/probe/probe
  ) || exit 1
done
python3 bench/profiling/measure-worktrees.py --root "$texture_perf_root" \
  --out out/performance-worktree-reproduction
```

The program first snapshots the unchanged baseline at five sizes and compares
all 67 examples in each treatment, then runs measurements sequentially. It
uses a fixed shuffle seed and interleaves all seven variants within each case.
The original study uses ten RTS capabilities on an Apple M1 Pro. The primary
512² render-and-PNG outcome has five samples of five operations per case;
render-only 512² and combined 96² have three samples, using five and forty
operations respectively. Two primary rounds confirm the initial three after
the secondary/control measurements. Warm-up, parsing and resolution are
excluded. A final collection is included in the interval so allocation and
GC statistics correspond to the same work.

The scheduling controls compare both chunk sizes with 4, 8 and 10 capabilities
on three examples at two sizes, with three samples each. All counts and
runtime settings are explicitly labelled in CSV rows. `block` identifies the
interleaved round; the probe's internal `batch` is one for these invocations.
CPU time sums across cores; allocated decimal MB is cumulative allocation,
not resident memory. Aggregate scores use the geometric mean of ten
per-example median speedups. Allocation deltas use the geometric mean of
per-example allocation ratios. Original full-library baseline files are kept.

The reproduction should pass exact pixels but will not reproduce exact
milliseconds, particularly on another machine. Retain the worktrees if you
want to inspect or combine treatments; no experiment is automatically merged
or pushed. Avoid concurrent builds/tests during timing.
