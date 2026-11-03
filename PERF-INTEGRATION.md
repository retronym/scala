# scala2-perf-integration

Integration branch of retronym's open performance PRs against scala/scala 2.13.x, for benchmarking a released-style (optimizer on) build.

Base: `upstream/2.13.x` at 278cab10408742a464413c445957658bb226527b (Merge pull request #11356 from retronym/review/map-builder-clear).

## Included

Each PR rebased onto the base (all clean) and merged with `--no-ff`:

| PR | Title | Merge |
|---|---|---|
| #11307 | patmat: faster analysis of large matches, with unchanged output | clean (draft) |
| #11306 | Avoid megamorphic interface calls to the analyzer when several Globals share a JVM | clean |
| #11312 | Cast a trait's this to its self type's class, not the member's owner | clean (draft, codegen) |
| #11350 | Share a producer-consumer analysis between method-local optimizations | clean (optimizer only) |
| #11352 | Check a NEW's class before analyzing producers in box-unbox elimination | clean (draft, optimizer only) |
| #11353 | Build ProdConsAnalyzer's consumer map in place | clean (draft, optimizer only) |

Cherry-picked from #11318 (`integration/2.13.x`), not in any PR above and not in 2.13.x:

- Avoid allocation in Symbol.filter when all alternatives match
- Test Symbol.filter on overloaded symbols

## Excluded

- #11315, #11313, #11314, #11295: merged into 2.13.x since the previous refresh. The Depth-import and NotAMemberErrorTest integration fixups are no longer needed (the latter is in #11306 itself).
- #11291 Order base types of the same depth by name rather than by symbol id: explicitly not perf-tested.
- #11290 Java constant expressions: not perf.
- #11318 integration/2.13.x: not merged wholesale (see its description).

## Build

Released-style: `sbt --client setupPublishCoreNonOpt publishLocal` with the untracked `local-opt.sbt` at the root pinning `-opt:l:inline -opt-inline-from:scala/**` (the same settings `setupPublishCore`'s `enableOptimizer` applies), so a plain `compile` from another build that loads this worktree as a ProjectRef stays optimized. Version suffix is `SHA-SNAPSHOT`, i.e. `2.13.19-bin-<sha>-SNAPSHOT`.
