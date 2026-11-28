# scala2-perf-integration

Integration branch of retronym's open performance PRs against scala/scala 2.13.x, for benchmarking a released-style (optimizer on) build. It is rebuilt from scratch on each refresh, so force-pushes are expected.

Base: `upstream/2.13.x` at 96c00082869328bdd994cab332cbc973f7798ae3.

## Included

Each PR is first rebased onto the base, then its head is merged with `--no-ff`, in this order. retronym/scala#249 comes first: it removes a dependence on C2's escape analysis, which may make results steadier for every comparison stacked above it.

| PR | Title | Notes |
|---|---|---|
| retronym/scala#249 | Build compound base type sequences without relying on escape analysis | clean |
| scala/scala#11307 | patmat: faster analysis of large matches, with unchanged output | clean |
| scala/scala#11312 | Cast a trait's this to its self type's class, not the member's owner | clean (draft, codegen) |
| retronym/scala#207 | Compute override pairs' member types fewer times | clean |
| retronym/scala#210 | Flatten intersections that a type map puts in a refinement's parents | clean |
| retronym/scala#200 | Avoid unreachable-code analysis for most methods | clean |
| retronym/scala#205 | Don't box UnapplySeqWrapper to compare it with null | clean |
| retronym/scala#206 | Don't box Array.UnapplySeqWrapper to compare it with null | clean (stacked on #205) |
| retronym/scala#243 | Fold ArgsTypeRef and NoArgsTypeRef into TypeRef | clean |
| retronym/scala#239 | Intern types in a chunked table that G1 keeps young | clean. Off in the PR; a commit below makes it the default here |
| retronym/scala#244 | Stop relying on the uniques table for type identity | conflicts with #239 in `Types.scala`, where #239's layout selection is kept and `dropUniques()` also drops the chunked table; and in `TypesTest.scala`, where both sides' tests are kept |
| retronym/scala#245 | Compute lub depths for solving and merging only when needed | clean. retronym/scala#246 is an alternative formulation of the same change, left out until a paired A/B against #245 says which one to keep |
| retronym/scala#250 | Avoid TypeTag in the f interpolator's type constants | clean |
| retronym/scala#247 | Drop typer and namer state that pins old trees past typer | clean |
| retronym/scala#253 | Dispatch typed1 on a class rather than the TypTree trait | clean |
| retronym/scala#256 | Erase in one traversal, without retyping with a Typer | conflict in `Erasure.scala` with scala/scala#11312: #256 moves `Eraser.adaptMember`'s qualifier cast into `ErasureTypeAdapter.adaptQualifier`, where #11312's cast to the self type's class (`selfTypeClass`) is applied |
| retronym/scala#258 | Test for a parameter accessor before looking up a value class's unbox | clean |
| retronym/scala#259 | Type synthesised trees after typer without a Typer | conflict in `PatternMatching.scala` with scala/scala#11307, which moved `typedStable` out of `Substitution` and caches its types: its `typedStable` now calls `typedStableRef` |
| retronym/scala#264 | Remove Tree.id | clean |
| retronym/scala#267 | Skip virtual widen/dealias in dealiasWiden for class type refs | textually clean, adapted to retronym/scala#243 in the merge: the fast path tests for its single `ClassTypeRef` class, and `AliasTypeRef` is a final class there |
| retronym/scala#268 | Type post-typer trees without a context, and make contexts only when read | conflict in `PatternMatching.scala` with scala/scala#11307: there its `typedStableRef(tree)` and stable-reference shortcut are applied to scala/scala#11307's `substitute` |
| retronym/scala#204 | Select how names are stored with -Yname-table | clean, without its `PLAN.md`. A commit on top makes `-Yname-table:shared` the default, so benchmarks need no flag |
| retronym/scala#270 | Pack opaque range positions into one Int when they fit | clean |
| retronym/scala#271 | Keep override-pair cursors' members in arrays; skip Object/Any pairs | its two commits, cherry-picked and merged: the PR is stacked on `claude/erasure-typeassigner-alloc`, whose other commits are not in this branch. Clean |
| retronym/scala#272 | Test a member for ignoreDeferred only if it is deferred | clean |
| retronym/scala#273 | Walk scope entries in Scope.exists/forall/count | clean |
| retronym/scala#496 | Remember failed wildcard-import lookups per import | clean |
| retronym/scala#511 | Allocate less in Context.lookupSymbol | conflict in `Contexts.scala` with retronym/scala#268, which already avoids the closure over the imported symbol: its side is taken there. `SymbolLookup` is pooled on this branch, so the pooled lookup's `clear()` also drops the import state that #511 moves into fields and the reused `ImportCursor`'s contexts |
| retronym/scala#513 | Filter accessible symbols without a closure | conflict in `Contexts.scala` with retronym/scala#511, whose `accessibleImported` already filters an imported symbol without a closure: its side is taken there, and the PR's `filterAccessible` serves the other callers |
| retronym/scala#515 | Reject polymorphic overloads on arity before cloning type params | clean |
| retronym/scala#516 | Make mixin forwarders cheaper to clone | clean |
| retronym/scala#517 | Skip the constructors phase's set copies and dead walks | clean |
| retronym/scala#518 | Four small CPU wins in typer, RefChecks and type interning | clean (cherry-picked: the PR is based on `perf-int-pekko-synth`) |
| retronym/scala#519 | Three more small CPU wins: override search, stabilize, uncurry | clean (cherry-picked: the PR is based on `perf-int-pekko-synth`) |
| retronym/scala#520 | Clone type params in isApplicable only when inference could rewrite them | conflict in `Infer.scala` with retronym/scala#515, which adds an arity check before the same `PolyType` case: both kept, the arity check first |
| retronym/scala#521 | Cache which implicits are plausible for an expected type | clean (auto-merged in `Implicits.scala`) |
| retronym/scala#522 | Share the enclosing scope when completing members without params | clean (auto-merged in `Namers.scala`) |
| retronym/scala#524 | Return method types early from followApply | clean (auto-merged in `Infer.scala`) |
| retronym/scala#523 | Find an existing unique TypeRef without allocating one | conflict in `Types.scala` with the TypeRef class layout and retronym/scala#239's chunked `unique`: kept this branch's TypeRef subclasses and layout switch, added the probes; a follow-up commit makes `findUnique` search the `ChunkedInterner` too |
| retronym/scala#525 | Skip specialization's per-member pass for unspecializable classes | clean |

A commit on top makes `-Yuniques-layout` default to `chunked-weak` (retronym/scala#239), so benchmarks need no `-Dscala.uniques.layout`; the property still applies when the option is not given.

## Excluded

- Merged into 2.13.x since the previous refresh: scala/scala#11369, #11375, #11376, #11381.
- retronym/scala#148: closed, superseded by retronym/scala#268.
- retronym/scala#246: the alternative formulation of retronym/scala#245, see above.
- #11291, #11318: see their descriptions; not perf-tested or not merged wholesale.
- retronym/scala#203 (direct class writing), #202 (asSeenFrom fast path): parked.

## Build

Released-style: `sbt --client setupPublishCoreNonOpt publishLocal` with the untracked `local-opt.sbt` at the root pinning `-opt:l:inline -opt-inline-from:scala/**`, so a plain `compile` from another build that loads this worktree as a ProjectRef stays optimized. Version suffix is `SHA-SNAPSHOT`, i.e. `2.13.19-bin-<sha>-SNAPSHOT`.
