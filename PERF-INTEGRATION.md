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
| retronym/scala#148 | Create typing transformers' contexts lazily | auto-merged `TypingTransformers.scala` (fork draft; `-Yeager-typing-transformer-contexts` restores the old behaviour) |
| retronym/scala#151 | Cake diet: take implementation-detail traits out of SymbolTable and Global | clean (fork draft; overlaps #11306's goal of fewer interfaces on `Global`) |
| retronym/scala#199 | Make Symbol's kind predicates and info/rawInfo final | clean (fork draft) |
| retronym/scala#207 | Reuse RefChecks' matched pairs in Erasure's double-definition check, reuse the override cursor's member types, test erased types first | clean (fork draft; merged from its branch before the PR existed; `PROPOSAL-refchecks.md` left in) |
| retronym/scala#205 | Don't box UnapplySeqWrapper to compare it with null | clean (fork draft) |
| retronym/scala#204 | Select how names are stored with -Yname-table | clean (fork draft, latest version; `PLAN.md` left out) |
| retronym/scala#208 | Give Symbol.typeParams one implementation in the compiler | conflict with #199's `typeParams` fast path, kept both (fork draft) |
| retronym/scala#209 | Test for the singleton types by class, not through their accessors | clean, plus #199's `mapOverType` rewritten to match (fork draft) |

Two commits follow #204: one stops `isVariableName` reading the first char of an empty name, which throws for String-backed names, and one defaults `-Yname-table` to `shared`, so that branch campaigns measure it with one set of options. #204 itself still defaults to `chars`.

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
