# scala2-perf-integration

Integration branch of retronym's open performance PRs against scala/scala 2.13.x, for benchmarking a released-style (optimizer on) build. It is rebuilt from scratch on each refresh, so force-pushes are expected.

Base: `upstream/2.13.x` at 632df4153c21bf0513f5735967fb1220185ab78a.

## Included

Each PR head is merged onto the base with `--no-ff`, in this order. retronym/scala#249 comes first: it removes a dependence on C2's escape analysis, which may make results steadier for every comparison stacked above it.

| PR | Title | Notes |
|---|---|---|
| retronym/scala#249 | Build compound base type sequences without relying on escape analysis | clean |
| scala/scala#11369 | Fix optimizer miscompiles of variables copied around a loop | clean |
| scala/scala#11375 | Make Symbol's hot methods JIT friendly for faster compiles | conflict in `project/MimaFilters.scala` with #11389's filters, now in 2.13.x; both sides' filters kept |
| scala/scala#11307 | patmat: faster analysis of large matches, with unchanged output | clean |
| scala/scala#11312 | Cast a trait's this to its self type's class, not the member's owner | clean (draft, codegen) |
| scala/scala#11376 | Test for the singleton types by class, not through their accessors | clean; replaces retronym/scala#209 |
| retronym/scala#148 | Create typing transformers' contexts lazily | clean; changes the `-Ylog` output checked by `neg/t12494`, whose check file is updated in this branch |
| retronym/scala#207 | Compute override pairs' member types fewer times | clean |
| retronym/scala#210 | Flatten intersections that a type map puts in a refinement's parents | clean |
| retronym/scala#200 | Avoid unreachable-code analysis for most methods | clean |
| retronym/scala#205 | Don't box UnapplySeqWrapper to compare it with null | clean |
| retronym/scala#206 | Don't box Array.UnapplySeqWrapper to compare it with null | clean (stacked on #205) |
| retronym/scala#243 | Fold ArgsTypeRef and NoArgsTypeRef into TypeRef | clean |
| retronym/scala#239 | Intern types in a chunked table that G1 keeps young | conflict in `SymbolTable.scala`: the base had already turned `isCompilerUniverse` into a val defined elsewhere, so only `uniquesLayout` is added. Off by default: select with `-Dscala.uniques.layout=chunked-weak` |
| retronym/scala#244 | Stop relying on the uniques table for type identity | conflicts with #239 in `Types.scala`, where #239's layout selection is kept and `dropUniques()` also drops the chunked table; and in `TypesTest.scala`, where both sides' tests are kept |
| retronym/scala#245 | Compute lub depths for solving and merging only when needed | clean. retronym/scala#246 is an alternative formulation of the same change, left out until a paired A/B against #245 says which one to keep |
| retronym/scala#250 | Avoid TypeTag in the f interpolator's type constants | clean |
| retronym/scala#247 | Drop typer and namer state that pins old trees past typer | clean |
| retronym/scala#253 | Dispatch typed1 on a class rather than the TypTree trait | clean |

Cherry-picked from #11318 (`integration/2.13.x`), not in any PR above and not in 2.13.x:

- Avoid allocation in Symbol.filter when all alternatives match
- Test Symbol.filter on overloaded symbols

## Excluded

- Merged into 2.13.x since the previous refresh: #11389, #11390.
- #11291, #11318: see their descriptions; not perf-tested or not merged wholesale.
- retronym/scala#204 (name table), #203 (direct class writing), #202 (asSeenFrom fast path): parked.

## Build

Released-style: `sbt --client setupPublishCoreNonOpt publishLocal` with the untracked `local-opt.sbt` at the root pinning `-opt:l:inline -opt-inline-from:scala/**`, so a plain `compile` from another build that loads this worktree as a ProjectRef stays optimized. Version suffix is `SHA-SNAPSHOT`, i.e. `2.13.19-bin-<sha>-SNAPSHOT`.
