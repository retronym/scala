# scala2-perf-integration

Integration branch of retronym's open performance PRs against scala/scala 2.13.x, for benchmarking a released-style (optimizer on) build. It is rebuilt from scratch on each refresh, so force-pushes are expected.

Base: `upstream/2.13.x` at 57587f108e9570c565e08f04009cbdb090cec294.

## Included

Each PR head is merged onto the base with `--no-ff`.

| PR | Title | Notes |
|---|---|---|
| scala/scala#11369 | Fix optimizer miscompiles of variables copied around a loop | clean |
| scala/scala#11375 | Make Symbol's hot methods JIT friendly for faster compiles | clean |
| scala/scala#11307 | patmat: faster analysis of large matches, with unchanged output | clean |
| scala/scala#11312 | Cast a trait's this to its self type's class, not the member's owner | clean (draft, codegen) |
| scala/scala#11389 | Give Symbol.typeParams one implementation in the compiler | conflict in `project/MimaFilters.scala`; both sides' filters kept; replaces retronym/scala#208 |
| scala/scala#11390 | Look up base type sequences by symbol, not by symbol id | clean |
| scala/scala#11376 | Test for the singleton types by class, not through their accessors | clean; replaces retronym/scala#209 |
| retronym/scala#148 | Create typing transformers' contexts lazily | clean; changes the `-Ylog` output checked by `neg/t12494`, whose check file is updated in this branch |
| retronym/scala#207 | Compute override pairs' member types fewer times | clean |
| retronym/scala#210 | Flatten intersections that a type map puts in a refinement's parents | clean |
| retronym/scala#200 | Avoid unreachable-code analysis for most methods | clean |
| retronym/scala#205 | Don't box UnapplySeqWrapper to compare it with null | clean |
| retronym/scala#206 | Don't box Array.UnapplySeqWrapper to compare it with null | clean (stacked on #205) |

Cherry-picked from #11318 (`integration/2.13.x`), not in any PR above and not in 2.13.x:

- Avoid allocation in Symbol.filter when all alternatives match
- Test Symbol.filter on overloaded symbols

## Excluded

- Merged into 2.13.x since the previous refresh: #11350, #11352, #11353, #11365 (cake diet, formerly retronym/scala#151), #11367, #11290, #11382.
- #11306: closed, replaced by #11367 (now merged).
- #11291, #11318: see their descriptions; not perf-tested or not merged wholesale.
- retronym/scala#204 (name table), #203 (direct class writing): parked.

## Build

Released-style: `sbt --client setupPublishCoreNonOpt publishLocal` with the untracked `local-opt.sbt` at the root pinning `-opt:l:inline -opt-inline-from:scala/**`, so a plain `compile` from another build that loads this worktree as a ProjectRef stays optimized. Version suffix is `SHA-SNAPSHOT`, i.e. `2.13.19-bin-<sha>-SNAPSHOT`.
