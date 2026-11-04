# Cutting the cost of override-pair checking

Status: draft for review. The branch has three commits on `2.13.x`: "Reuse RefChecks' matched pairs in Erasure's double-definition check", "Reuse the override cursor's member types in RefChecks" and "Test erased types first in Erasure's double-definition check". All the measurements below were taken on `scala2-perf-integration` at `00e3bbafb8`, not on `2.13.x`. On the M4 the first two commits made the scala corpus 5.9% faster (± 2.1 at 99.9%, 3 forks per build) and left scalap flat. The third commit was measured alone, in profile frames only (one fork per variant): its timing A/B was cancelled. The three together have not been measured.

## Problem

On the M4 scala corpus (`m4-int00e3bb-kperf-l1d`, kperf cycles, compiler thread = stacks under `GlobalPhase`), three walks over override pairs cost:

| frame (inclusive) | % compiler-thread cycles | % compiler-thread L1D load misses |
|---|---|---|
| `RefChecks.checkAllOverrides` | 9.54 | 4.17 |
| `Erasure.checkNoDoubleDefs` | 4.66 | 2.65 |
| Erasure bridges (`EnterBridges` / `GenerateBridges`) | 3.01 | 2.36 |
| of which `SymbolPairs.Cursor.<init>` (scope building), all three | 0.77 | 1.62 |
| of which `fillDecls` | 0.43 | 1.36 |

Nearly all of it is `Type.memberType` → `asSeenFrom`. The same member types are computed several times over for the same pairs.

## Where the time goes

All three use `SymbolPairs.Cursor`. Per class it copies the declarations of every base class into one scope, deferred ones first and concrete ones second, and then walks each entry against the later same-named entries. `OverridingPairs.Cursor.matches` compares `self.memberType(low)` (cached per low) with `self.memberType(high)`.

Counts from an instrumented quick build compiling the scala corpus once (patch in the shared notes folder, `refchecks-pair-counters.patch`):

| | RefChecks `Cursor` | Erasure `BridgesCursor` | Erasure `DoubleDefsCursor` |
|---|---|---|---|
| cursors (classes) | 5,135 | 4,259 | 5,401 |
| base-class scans in `fillDecls` (both passes) | 86,122 | 69,860 | 89,110 |
| declarations seen / entered | 1.17M / 532k | 1.01M / 428k | 1.35M / 515k |
| same-name candidates | 241,369 | 189,302 | 207,888 |
| reached `memberType(high)` | 200,461 | 160,988 | — (`matches` = `!high.isPrivate`) |
| matched | 181,213 | 147,457 | 207,860 |
| pairs yielded | 185,322 | 53,290 | 195,684 |

The cheap filters (same owner, private, Java field) remove only 17% of RefChecks' candidates before `memberType`. Of the rest, 90% match: these are genuine overrides, not wasted candidates. So the cost lies in how often each surviving pair's member types are computed, not in candidates that can't conflict.

1. **RefChecks computes each pair's member types twice.** `matches` computes `memberType(low)` and `memberType(high)` to accept the pair. Then `checkOverride` → `checkOverrideTerm` asks `SymbolPair.lowType` / `highType`, which are `def`s that compute both again (150k term pairs). In the profile this is `SymbolPair.highType` 0.95% + `lowType` 0.88% of all samples, under `checkOverrideTerm`. That is 2.6% of compiler-thread cycles, a quarter of `checkAllOverrides`. `MethodSymbol.typeAsMemberOf`'s cache is off after typer (t8011), so nothing catches the repeat.

2. **Erasure's double-definition check repeats RefChecks' matching.** `DoubleDefsCursor` pairs every same-named non-private member. For each pair `isErasureDoubleDef` evaluates `exitingRefchecks(lowType matches highType)` first, then the cheap `sameTypeAfterErasure`. That refchecks-phase `memberType` is 3.9% of compiler-thread cycles (`isErasureDoubleDef` inclusive; `sameTypeAfterErasure` shows 0.01% of samples, but today it only runs on the 19k pairs that don't match). But RefChecks has no info transformer, so this is the question RefChecks' cursor just answered for the same class. Of the 195,684 pairs, **173,962 (89%) are pairs RefChecks matched for the same base**. 140,167 have the same erasure, and only 2,598 of those are not already known.

3. **Scope rebuilding.** Each of the three cursors rebuilds the scope of all base classes. That is on average ~8.4 base classes and ~114 declarations per class, each visited twice (deferred pass, concrete pass), by three cursors per class, with fresh `ScopeEntry`s each time. It is small in cycles (0.77%) but costs twice as much in L1D misses (1.62%), from walking cold decl lists of library base classes. `fillDecls` recurses once per base class (up to 78 frames for collection classes) to visit them root-first. The recursion is only the iteration order; its depth costs nothing by itself.

4. **`checkNoAbstractMembers`** (2.2% compiler-thread) runs a second `findMembers` over every base class of each concrete class (`nonPrivateMembersAdmitting(VBRIDGE)`), with its own `memberType` calls in `isNewMember`.

History: until 2018 RefChecks skipped pairs that a parent class "already checked". `cc55bd92dd` (Adriaan, "Subclassing on traits does not imply suffixing for their linearisations") removed that for correctness, so every class re-checks every inherited pair. A naive version of that pruning is still wrong. Pairs inside a non-trait parent's linearization can start to match only in the subclass, once its type arguments are fixed:

```scala
class A[T] { def f(x: T): Int = 1 }
class B[T] extends A[T] { def f(x: Int): Int = 2 }
class C extends B[Int]   // error: class C inherits conflicting members
```

So the matching has to run in `C` anyway, and the matching is the expensive part.

## Proposals, ranked

### 1. Erasure's double-def check reuses RefChecks' matched pairs (prototyped)

RefChecks records each term pair its cursor yields (the member types matched) per class, in a per-run map in `overridingPairs`: a `LongMap` of packed `(low.id, high.id)`. `isErasureDoubleDef` checks the map before computing `exitingRefchecks(lowType matches highType)`. Erasure drops a class's entry after checking it, so the map holds at most the classes between refchecks and erasure (~174k entries for the scala corpus).

- Gain: up to the 3.9% of compiler-thread cycles in `isErasureDoubleDef`. 89% of its pairs become a hash lookup.
- Risk: low. The pairs, their order and the predicate's value are unchanged, so errors and their order are too. The answer is the same computation, at refchecks instead of refchecks.next, and RefChecks has no info transform. Java-defined members are not recorded, because `checkOverride` cooks their raw types after matching. Classes RefChecks never saw, such as specialized subclasses, fall back to computing. The one gap: if a later phase `setInfo`s an inherited symbol and so rewrites its refchecks-phase history, the old code would see the rewritten type and the cache the original one. I know of no case, and the double-def check already excludes symbols without a refchecks-phase type.
- A variant with no cross-phase state: test `sameTypeAfterErasure` first, which skips the match for the 28% of pairs whose erasures differ. Timed around both predicates in two cold single compiles of the scala corpus (not an m4 measurement), the match takes 383–411 ms in all, 117–140 ms of it on those pairs, and `sameTypeAfterErasure` takes 21–32 ms across all pairs. That predicted a net saving of about a quarter of the match time, so roughly 1 point of compiler-thread cycles. It also forces post-erasure infos for about 177k more pairs than today. Measured on the m4 (see "Flip only" below), it saves 1.10 points against the cache's 3.29 in its own pilot. `partest --pos --neg --run` on the flip build shows only the `neg/t12494` failure that base has.
- The flip on top of the cache: of the 42.6k pairs the cache misses, 19.3k have different erasures and take 55 of the 66 ms the misses spend matching. Net of `sameTypeAfterErasure` on all pairs, that is very roughly 0.3 points of the 0.72 left in `isErasureDoubleDef`. This is the order the branch uses: the cache lookup, then the erased types, then the match. It is an estimate from local timings; this combination has not been measured.

### 2. RefChecks reuses the cursor's member types (prototyped)

The cursor remembers `memberType(high)` as it already does `memberType(low)` and seeds the `SymbolPair` with both. `lowType` / `highType` return the seeded type while the period and the symbol's `rawInfo` are the ones it was computed at, and recompute otherwise. Cooking a Java raw type and reading the pair in another phase (Erasure's `doubleDefError`) both recompute.

- Gain: up to 2.6% of compiler-thread cycles (the `lowType` + `highType` frames under `checkOverrideTerm`).
- Risk: low. No pair, check or message changes. The cost is one small holder object per `memberType` the cursor computes.

### 3. Fewer findMembers / scope builds per class (not prototyped)

- Derive `checkNoAbstractMembers`' missing members from the deferred highs that no concrete low overrides in the pairs RefChecks already walks, instead of a second `findMembers` (2.2%). This has medium risk: its error listing is built from `nonPrivateMembers`, and the pair walk's shadowing rules are not `findMembers`' rules.
- Build the per-class declaration snapshot once and filter it per cursor, iterating base classes from a reversed array instead of recursing (≤0.8% cycles, 1.6% L1D). This has low risk but needs care, because the three cursors run at different phases and use different `exclude`s.

### Considered: a per-name lockstep walk of the base-class scopes (not recommended)

Walking names in lockstep across the base classes, instead of copying every declaration into one scope, would only save the cursor's own work. That is `SymbolPairs$Cursor` minus `matches`: 1.0% of compiler-thread cycles and 2.2% of L1D load misses. The pairs, and so the `memberType` calls in `matches` (5.7%), stay the same. Scopes aren't sorted by name, so it needs a sort or a k-way merge, and pair order would change. Pair order is what sets error order, so that would have to be restored.

Memoising member types per symbol within a cursor would not help much either: 15,486 of RefChecks' 200,461 `memberType(high)` calls (8%) repeat a symbol.

### Possible: skip pairs that lie wholly in the first parent (not prototyped, needs a soundness argument)

The first parent's linearization is always a suffix of the class's, so pairs with both owners in it occur in the same order in the parent. `class C extends B[Int]` above shows they can still start to match in the subclass, so type arguments rule out skipping them in general. A narrower condition: the first parent has no type arguments and the bases new to the class declare no type members. Counts for RefChecks on the scala corpus:

| | `memberType(high)` calls | pairs |
|---|---|---|
| all | 200,461 | 186,276 |
| both owners in the first parent's linearization | 113,954 (57%) | 106,381 (57%) |
| and the narrower condition holds | 44,865 (22%) | 42,962 (23%) |

This needs no lockstep walk: an owner test in `matches` before `memberType` is enough. The 22% is by count, not time. If time follows count, the gain is under 1 point of compiler-thread cycles in RefChecks, and possibly some more in the bridges cursor, which was not counted. It also relies on the parent having been checked, which is the assumption `cc55bd92dd` backed out of in its broader form, and on separate compilation not invalidating that. So the risk is much higher than for proposals 1 and 2.

## A/B result

**Profile pilot** [`m4-refchecks-pairs-prof`](https://perflab.localhost/p/scala2-b7bb3d/ab/index.html#c-m4-refchecks-pairs-prof): M4, scala corpus, one fork per build, kperf cycles + instructions, patched JDK 26, `SCALA_RELEASE_OPT`. Base `00e3bbafb8` vs proto, the first two commits on that base. Single forks, so no interval:

| | base | proto | Δ |
|---|---|---|---|
| ms/op | 9,614 | 8,968 | −6.7% |
| cycles/op | 36.7G | 34.4G | −6.4% |
| instructions/op | 156G | 143G | −8.6% |
| alloc/op | 5,983 MB | 5,750 MB | −3.9% |

Inclusive frames, as % of the base's compiler-thread cycles:

| frame | base | proto | Δ |
|---|---|---|---|
| `checkAllOverrides` | 9.83 | 6.91 | −2.92 |
| `checkOverrideTerm` | 3.37 | 0.58 | −2.79 |
| `checkNoDoubleDefs` | 4.78 | 1.42 | −3.36 |
| `isErasureDoubleDef` | 4.01 | 0.72 | −3.29 |
| `OverridingPairs.Cursor.matches` | 5.68 | 5.73 | +0.05 |
| Erasure bridges | 3.11 | 3.14 | +0.03 |
| `Type.memberType` (all callers) | 24.36 | 18.36 | −6.00 |
| compiler thread total | 100 | 93.6 | −6.4 |

The two targeted frames account for the whole drop. Nothing else moved by more than noise.

**Timing A/B** [`m4-refchecks-pairs-ab`](https://perflab.localhost/p/scala2-b7bb3d/ab/index.html#c-m4-refchecks-pairs-ab): M4, no profiler, builds alternating fork by fork, 3 rounds (3 forks per build per corpus), patched JDK 26, `SCALA_RELEASE_OPT`. Means ± 99.9% CI over forks, Δ by Welch's t (perflab API `campaign`):

| corpus | base | proto | Δ ms/op | Δ alloc/op |
|---|---|---|---|---|
| scala (single-shot 4+4) | 9,208 ± 241 ms | 8,666 ± 227 ms | **−5.9% ± 2.1, better** | −4.6% ± 1.6, better |
| scalap (20+20 × 2 s) | 194.7 ± 4.1 ms | 195.4 ± 4.1 ms | +0.3% ± 1.7, flat | +0.4% ± 1.1, flat |

The scala result agrees with the pilot. scalap is flat as expected: in its base profile (`m4-int00e3bb-prof`, cpu) the two frames the change removes, `checkOverrideTerm` and `isErasureDoubleDef`, are 0.15% + 0.35% of compiler-thread time, below the A/B's resolution. scalap's classes have shallow hierarchies, while the scala corpus defines and extends the collections.

**Flip only** [`m4-refchecks-flip-prof`](https://perflab.localhost/p/scala2-b7bb3d/ab/index.html#c-m4-refchecks-flip-prof): base, base + the order flip (no cache and no member-type reuse), and a second base fork as an A/A. M4, scala corpus, one fork per variant, kperf cycles + instructions. Inclusive frames, as % of the first base fork's compiler-thread cycles (instructions in brackets):

| frame | base | flip | Δ flip | Δ A/A |
|---|---|---|---|---|
| `isErasureDoubleDef` | 4.11 (5.36) | 3.01 (3.90) | −1.10 (−1.46) | +0.02 (+0.01) |
| `checkNoDoubleDefs` | 4.87 (5.89) | 3.82 (4.46) | −1.06 (−1.43) | 0.00 (0.00) |
| `sameTypeAfterErasure` | 0.03 (0.02) | 0.19 (0.16) | +0.17 (+0.14) | 0.00 (0.00) |
| `Type.memberType` (all callers) | 23.94 (29.43) | 22.72 (27.70) | −1.22 (−1.73) | +0.12 (+0.19) |
| `OverridingPairs.Cursor.matches` | 5.61 (7.39) | 5.61 (7.37) | 0.00 (−0.02) | +0.24 (+0.33) |
| compiler thread total | 100 | 98.5 (98.2) | −1.50 (−1.76) | +0.73 (+0.73) |

The frame gain is real: −1.10 points in a frame whose A/A moved by 0.02, and `sameTypeAfterErasure`'s extra cost is small. The whole-op result is not resolved: ms/op is −1.3% (9,493 → 9,369), but the A/A moved +0.6%, so the single forks can't separate a change of this size.

## Tests

**Can proposal 1 drop a diagnostic?** Only if the cache says "matched at refchecks" while the original `exitingRefchecks(lowType matches highType)` says otherwise. I checked that directly with a temporary build: on every cache hit it also evaluated the original predicate and logged any disagreement.

- `partest --pos --neg --run` (5,417 tests): 117,792 hits, **0 disagreements**.
- Scala corpus: 153,055 hits, 0 disagreements.
- Nine hand-written cases (in the shared notes folder, `refchecks-dd-tricky-cases/`), all reporting the expected errors:
  - an inherited erased clash between a trait and a class;
  - a bridge clash with a user `f(Object)`;
  - a double definition with a polymorphic overload;
  - by-name vs `Function0`, and varargs vs `Seq`;
  - a Java generic/raw parent with a bridge clash, and Java varargs vs `Array`;
  - a Java interface clash;
  - value class vs underlying type;
  - inherited clashes across two traits.

  In one of these, `class W8 extends U8[List[Int]] with V8[String]` (two inherited `h`s that both erase to `h(List)`) gets no error. Released 2.13.16 accepts it as well, so this predates the change.

The check above ran on the first two commits on `scala2-perf-integration`, before the third commit changed the order of the predicate. It tested the cache's answer, and the third commit does not change that answer.

`partest --pos --neg --run` on this branch, on `2.13.x`: 5,424 passed, 2 skipped, none failed. On `scala2-perf-integration`, `neg/t12494` failed with and without these commits, for a reason unrelated to them (it is missing `[context] ++` log lines from "Create typing transformers' contexts lazily", which `2.13.x` does not have).
