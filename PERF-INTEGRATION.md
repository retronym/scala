# scala2-perf-integration

Integration branch of retronym's open performance PRs against scala/scala 2.13.x, for benchmarking a released-style (optimizer on) build.

Base: `upstream/2.13.x` at f67d42767acd04fc561cb94d22c4682b8e76a7ae (Merge pull request #11328 from lrytz/presentation-flaky).

## Included

| PR | Title | Merge |
|---|---|---|
| #11315 | Prune the members of lubBase considered for refinement in lub | clean |
| #11313 | Give each top-level lub/glb its own memo tables | clean |
| #11314 | Don't share mutable type-operation state across reflection threads | conflict in `project/MimaFilters.scala` only (both PRs append filters at the same spot); resolved by keeping both blocks |
| #11295 | Avoid per-tree allocations when checking unused types | clean |
| #11307 | patmat: faster analysis of large matches, with unchanged output | clean (draft, measured) |
| #11306 | Avoid megamorphic interface calls to the analyzer when several Globals share a JVM | clean (draft, measured) |
| #11312 | Cast a trait's this to its self type's class, not the member's owner | clean (draft, codegen) |

Cherry-picked from #11318 (`integration/2.13.x`), not in any PR above and not in 2.13.x:

- `338e10142b` Avoid allocation in Symbol.filter when all alternatives match
- `8bcd79b374` Test Symbol.filter on overloaded symbols

## Excluded

- #11291 Order base types of the same depth by name rather than by symbol id: explicitly not perf-tested.
- #11318 integration/2.13.x: 80 commits, conflicts with 2.13.x; not merged wholesale. Its remaining commits are either already in 2.13.x in final form (#11316 Name.encode, #11317 NotAMemberError hints, #11314), part of the PRs merged above, or non-perf / larger features without an upstream-ready PR (Java constant folding #11290, Java `permits`, xsbt-api hasher rework, fields/trait-setter fixes, scala/bug#12224, determinism PRs #11319/#11320).
- #11319, #11320, #11330, #11331, #11332: determinism and docs, not perf.
- #11317, #11316, #11296, #11294, #11011, #11010: already merged into 2.13.x.

## Build

Released-style: `sbt --client setupPublishCoreNonOpt publishLocal` with the untracked `local-opt.sbt` at the root pinning `-opt:l:inline -opt-inline-from:scala/**` (the same settings `setupPublishCore`'s `enableOptimizer` applies), so a plain `compile` from another build that loads this worktree as a ProjectRef stays optimized. Version suffix is `SHA-SNAPSHOT`, i.e. `2.13.19-bin-<sha>-SNAPSHOT`.
