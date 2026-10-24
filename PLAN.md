# Spike: one composite for per-thread scratch state in `SymbolTable`

Branch: `claude/reflect-scratch-4f66f7`. Scope: kind A only (per-operation scratch state that runtime reflection must keep per thread). The GIL, `LOCKED`, uniques, `perRunCaches` and the derived-type caches stay as they are.

## Problem

`scala.reflect.internal.SymbolTable` keeps the scratch state of its type operations (recursion counters, pending sets, the undo log, lub/glb memo tables) as fields scattered over the cake: `Types`, `TypeComparers`, `TypeConstraints`, `GlbLubs`, `TypeToStrings`, `Symbols`. Each field is reached through a `def` so that `scala.reflect.runtime.SynchronizedTypes`/`SynchronizedSymbols` can override it with `mkThreadLocalStorage(...)`, one override (and for `var`s, a getter/setter pair) per field. Two consequences:

- The set of "this must be per thread" state is defined only implicitly, by which accessors the runtime remembered to override. Fields added later without an override are silently shared between threads (findings F1–F9 below).
- `mkThreadLocalStorage` is a `Collections.synchronizedMap(new WeakHashMap[Thread, T])`: every access takes a monitor and hashes `currentThread`. `isSubType` touches `subsametypeRecursions` four times, `undoLog` twice and `knownFalseSubTypes`/`pendingSubTypes` on the slow path, each a separate locked lookup.

The Scala 3 presentation compiler (retronym/scala3#84) solved the same problem by gathering every such field into one object reached through a single accessor that is a plain field in batch and a `ThreadLocal` in the concurrent universe. This spike does the same for Scala 2.

The outcome is summarised under "Results" at the end.

## Kinds

From the Scala 3 multi-reader design (retronym/scala3#84, `docs/_docs/internals/interactive-multi-reader.md`), with their Scala 2 instances:

- **A. Per-operation scratch, wrongly shared:** recursion guards, pending sets, the undo log, lub/glb memo tables, helpers with mutable fields (`ReusableInstance` pools, shared `TypeCollector`s). Treatment: confine to the thread (`Scratch`).
- **B. Lazy facts with a black hole:** symbol completion, runtime reflection's initialisation mask, base type sequences under construction; what the GIL protects. Treatment: owner and barrier per completion root. Not done.
- **C. Derived caches:** `intersectionWitness`, a type's cached base type sequence, base classes, parents, underlying, normalized. Treatment: safe publication or concurrent maps. Not done, except that `intersectionWitness` is only touched under the GIL and so needs no per-thread copies.
- **D. Interning and counters:** `unique`, the name table, symbol ids. Treatment: an implementation chosen when the universe is built, from `isConcurrentUniverse`.
- **E. Trees:** typed trees mutated in place. Not relevant to runtime reflection's type operations.

## Inventory

Kind A = per-operation scratch that is semantically per thread. "Runtime today" = how `scala.reflect.runtime` handles it.

### Already per thread via `mkThreadLocalStorage`

| Field | Defined at | Overridden at | Notes |
|---|---|---|---|
| `skolemizationLevel` (var Int) | `internal/Types.scala:123` | `runtime/SynchronizedTypes.scala:53` | bumped in `isSubType2`/existential subtyping, saved/restored in `ExistentialsAndSkolems` |
| `undoLog` (UndoLog) | `internal/tpe/TypeConstraints.scala:29` | `runtime/SynchronizedTypes.scala:57` | registered with `perRunCaches` in the compiler universe |
| `subsametypeRecursions` (var Int) | `internal/tpe/TypeComparers.scala:56` | `runtime/SynchronizedTypes.scala:63` | entry/exit of every `isSubType`/`isSameType`/`isDifferentType` |
| `pendingSubTypes` (HashSet) | `internal/tpe/TypeComparers.scala:31` | `runtime/SynchronizedTypes.scala:67` | cycle detection past `LogPendingSubTypesThreshold` |
| `knownFalseSubTypes` (IdentityHashMap) | `internal/tpe/TypeComparers.scala:34` | `runtime/SynchronizedTypes.scala:70` | scala/bug#13119 |
| `basetypeRecursions` (var Int) | `internal/Types.scala:4609` | `runtime/SynchronizedTypes.scala:73` | `baseTypeOfNonClassTypeRef` |
| `pendingBaseTypes` (HashSet) | `internal/Types.scala:4613` | `runtime/SynchronizedTypes.scala:77` | |
| `lubGlbCaches` (var LubGlbCaches) | `internal/tpe/GlbLubs.scala:312` | `runtime/SynchronizedTypes.scala:80` | per outermost `lub`/`glb` since #11313 |
| `indent` (var String) | `internal/Types.scala:5204` | `runtime/SynchronizedTypes.scala:84` | `explain` / `printLubs` debugging only |
| `toStringRecursions` (var Int) | `internal/tpe/TypeToStrings.scala:27` | `runtime/SynchronizedTypes.scala:88` | |
| `toStringSubjects` (HashSet) | `internal/tpe/TypeToStrings.scala:31` | `runtime/SynchronizedTypes.scala:92` | only referenced from commented-out code |
| `recursionTable` (var Map) | `internal/Symbols.scala:39` | `runtime/SynchronizedSymbols.scala:30` | `Symbol.lock` recursion counts; written under the GIL in runtime (completion), kept per thread anyway |

### Per thread via `mkThreadLocalStorage`, but not kind A (stays)

| Field | Defined at | Overridden at | Why it stays |
|---|---|---|---|
| `intersectionWitness` | `internal/Types.scala:132` | `runtime/SynchronizedTypes.scala:60` | kind C/D (hash-consing cache of intersection types). Per-thread copies are a workaround for an unsynchronised `WeakHashMap`, not a statement that the state is per operation. Out of scope; keeps the last `mkThreadLocalStorage` use. |

### Per call (allocated afresh in runtime instead of per thread)

| Field | Defined at | Runtime today |
|---|---|---|
| `commonOwnerMap` | `internal/tpe/CommonOwners.scala:38` | `SynchronizedTypes.scala:28` overrides with `new CommonOwnerMap` per call |
| `findMemberInstance` | `tpe/FindMembers.scala:245` | `ReusableInstance(..., enabled = isCompilerUniverse)`: reuse off, every call allocates |
| `containsCollectorInstances` | `tpe/TypeConstraints.scala:202` | same |
| `varianceInTypeCache` | `Variances.scala:272` | same |
| `copyRefinedTypeSSM` | `Types.scala:4032` | same |
| `cloneSymbolsSubstSymMap` | `Symbols.scala:3801` | same |
| `onlyChildAccumulator` | `Trees.scala:2106` | same |
| `setChildrenPosAccumulator` | `Positions.scala:305` | same |
| compiler-side: `symbolLookupCache` (`Contexts.scala:1320`), `localShadowerCache` (`Implicits.scala:2007`), `classFileDataReader` (`SymbolLoaders.scala:317`) | `src/compiler` | same switch; these types live in the compiler, so a `Scratch` in `src/reflect` cannot hold them |

`ReusableInstance` is this whole design in miniature: a stack of scratch objects indexed by reentrancy depth, correct on one thread only, so the concurrent universe opts out (`enabled = isCompilerUniverse`) and allocates per call. The other kind-A fields express the same reentrancy stack by hand: `LubGlbCaches(outer)`, `TypeCollector.collect` saving and restoring `result`, `VariancedTypeMap.withVariance`, `UndoLog.undo`. Once the thread is the unit that owns `Scratch`, reentrancy is the only remaining concern and `ReusableInstance` is the right shape for it, in every universe.

### Kind A, shared between threads in runtime reflection (findings)

None of these is reached under the GIL on the paths listed, so concurrent callers race on them.

| # | Field | Defined at | Reached from | Effect of a race |
|---|---|---|---|---|
| F1 | `globalGlbDepth` (var Depth) | `internal/tpe/GlbLubs.scala:531` | `glb` → `glbNorm` refinement branch (`GlbLubs.scala:646-665`), from `<:<`/`lub` on refined types | `x = x.incr` / `x = x.decr` from two threads loses updates; the counter drifts permanently. Drift up past `globalGlbLimit` (2) makes every later glb of refinements drop all members; drift down disables the recursion guard. |
| F2 | `IsRelatableCollector.barLevel` + `.result` | `internal/tpe/TypeMaps.scala:1303` (object), used at `Types.scala:3729` | `TypeVar.isRelatable` during constraint registration in subtyping | one thread's `barLevel` used for another's `collect`, and `result` overwritten mid-traversal: wrong `isRelatable`, so a constraint is wrongly accepted or rejected. |
| F3 | `UnrelatableCollector.barLevel` + `.buffer` | `TypeMaps.scala:1294`, used at `Types.scala:3719` | `TypeVar.unrelatable` | same, plus collected skolems leak between callers. |
| F4 | `ErroneousCollector.result` | `TypeMaps.scala:1157`, used by `Type.isErroneous` (`Types.scala:297`) | everywhere (`isErroneous` is checked all over typing and printing) | a type reported erroneous or not depending on another thread's traversal. |
| F5 | `IsDependentCollector.result` | `TypeMaps.scala:912`, used at `Types.scala:2889` | `MethodType.isDependentMethodType` (cached in `isdepmeth`, so a wrong answer sticks) | wrong dependent-method-type flag, cached on the `MethodType`. |
| F6 | `wildcardExtrapolation` variance (`VariancedTypeMap._variance`) | `TypeMaps.scala:391`, used at `Definitions.scala:860` (`samOf`) | SAM type computation | wrong variance → wildcard extrapolated to the wrong bound. |
| F7 | `boxingErasure.boxPrimitives` / `boxing3Erasure` | `transform/Erasure.scala:562` | `erasedValueClassArg` (`Erasure.scala:103`) for parametric value classes | primitive boxed or not depending on another thread being inside an array. |
| F8 | `perRunCaches.caches` (`::=` on a var) | `internal/SymbolTable.scala:408` | each thread's first `intersectionWitness` access registers a new per-thread map | lost registrations; harmless in runtime (never cleared), noted because `ReflectGlobal` is a compiler universe with the runtime overrides. |
| F9 | `isWildCardOrNonGroundTypeVarCollector.result` (a `FindTypeCollector` in a field) | `internal/tpe/GlbLubs.scala:195` | `lub` → `elimSub` → `maxTypes` | **Reproduced** by the baseline run of `ReflectionTypeOpsBenchmark.lubLeafOther -t 4`: `NullPointerException: ... FindTypeCollector.result() is null` at `TypeMaps.scala:1153`, from `GlbLubs.maxTypes(GlbLubs.scala:230)`. `collect` saves `result`, sets it to `None`, traverses and restores the saved value; another thread's restore writes back `null` (the field's initial value) under a traversal in progress. |

Covered by the GIL today, listed so that replacing the GIL does not forget them: `typeContainsTypeVar` (`Types.scala:5344`, a `FindTypeCollector` in a field), used only in `defineBaseTypeSeqOfCompoundType`, which runtime reflection runs under `gilSynchronized`.

Shared `Transformer`s, whose `currentOwner` is a mutable field: `duplicateAndResetPos` (`Trees.scala:1912`, compiler only) and `patvarTransformer` (`TreeGen.scala:997`, reached from `ReificationSupport` `SyntacticPatDef` → `gen.mkPatDef`, i.e. quasiquotes evaluated against the runtime universe). Not type operations; out of scope here, but the same shape (F11, F12).

Benign or out of scope: `explainSwitch` (`Types.scala:100`, only set by `explaining`, i.e. `-explaintypes`); `lockedCount` (`Symbols.scala:43`, read only when `isCompilerUniverse`); `_lockingTrace` (`Symbols.scala:47`, only with `-Ycheck-symbol-locks`-style tracing, written under the GIL); `ph`/`per`/`phStack` (`SymbolTable.scala:228`, runtime only ever sets `SomePhase`; per run, not per thread).

## Design

```scala
// internal/SymbolTable.scala (nested so it sees Type, UndoLog, LubGlbCaches, ...)
final class Scratch {
  var subsametypeRecursions = 0
  val pendingSubTypes      = new mutable.HashSet[SubTypePair]
  val knownFalseSubTypes   = new IdentityHashMap[Type, List[Type]]
  val undoLog              = new UndoLog
  var skolemizationLevel   = 0
  var basetypeRecursions   = 0
  val pendingBaseTypes     = new mutable.HashSet[Type]
  var lubGlbCaches         = new LubGlbCaches(null)
  var globalGlbDepth       = Depth.Zero          // F1
  var indent               = ""
  var toStringRecursions   = 0
  val toStringSubjects     = new mutable.HashSet[Type]
  var recursionTable       = Map.empty[Symbol, Int]
  // F2-F7: per-thread instances of the shared collectors/maps
}
def scratch: Scratch          // a plain val in internal.SymbolTable
```

- `internal.SymbolTable` holds one `Scratch` in a `lazy val` (so its initialiser, which allocates the `ReusableInstance`s' first instances, runs after every trait body); `def scratch` returns it. Batch (`Global`, toolbox, presentation compiler) pays a field load per fetch.
- `runtime.SynchronizedTypes` overrides `scratch` to return one per thread. The 12 kind-A `mkThreadLocalStorage` overrides go away. `ReflectGlobal` (compiler universe + runtime traits) inherits the override, as it inherited the old ones. The universe owns the instances in a `synchronizedMap(WeakHashMap[Thread, Scratch])`, consulted once per thread, and a `ThreadLocal[WeakReference[Scratch]]` is the fast path. A plain `ThreadLocal[Scratch]` leaks: `Scratch` is an inner class, so the value strongly reaches the universe and through it the `ThreadLocal` key, and a pooled thread keeps every universe (and classloader) it used. `ScratchTest.scratchDoesNotRetainUniverse` checks this.
- The old accessors (`undoLog`, `skolemizationLevel`, `lubGlbCaches`, ...) stay as non-overridden forwarders to `scratch.x` for source compatibility with the compiler, scaladoc, toolbox and third-party plugins. MiMa filters for the removed runtime overrides as needed.
- Hot paths fetch once: `val s = scratch` at entry of `isSubType`, `isSameType`, `isDifferentType`, `withLubGlbCaches`/`claimLubGlbCaches`, `baseTypeOfNonClassTypeRef`, the `glb` refinement branch, `typeToString`, `Symbol.lock`/`unlock`. The undo-log users in the typer (`Implicits`, `Analyzer`) keep `undoLog`; they are compiler-only.
- `ThreadLocalStorage` remains for `intersectionWitness` only.
- Reentrant scratch objects are `ReusableInstance`s held in `Scratch`, always enabled: the seven `enabled = isCompilerUniverse` instances in `src/reflect` move in, so runtime reflection reuses them per thread instead of allocating per call. The shared singletons with mutable state (F2–F7) get the same treatment: call sites go through `scratch.erroneousCollectors.using(_.collect(tp))` etc. The public `object`s stay for binary compatibility and direct users. `commonOwnerMap` likewise.
- `ReusableInstance` allocates its first instance eagerly, so the `Scratch` field must be initialised after the traits whose classes it instantiates; either a `lazy val` or construction at the end of `SymbolTable`'s initialiser. Pick whichever keeps `scratch` a plain field load in batch.
- Compiler-side `ReusableInstance`s (`Contexts`, `Implicits`, `SymbolLoaders`) stay as they are: `Scratch` is a `final class` in `src/reflect`. If they matter, `Global` can hold a second, compiler-side composite reached the same way; not in this spike.

What stays: everything per run (`perRunCaches`, `intersectionWitness`, uniques), the GIL and every `gilSynchronized`, `LOCKED`.

## Steps

- [x] 1. `Scratch` + `scratch` (val / `ThreadLocal`), move the subtyping group: `subsametypeRecursions`, `pendingSubTypes`, `knownFalseSubTypes`, `undoLog`; fetch once in `isSubType`/`isSameType`/`isDifferentType`.
- [x] 2. Move `skolemizationLevel`, `basetypeRecursions`, `pendingBaseTypes`.
- [x] 3. Move `lubGlbCaches`; add `globalGlbDepth` (F1).
- [x] 4. Move `indent`, `toStringRecursions`, `toStringSubjects`, `recursionTable`; delete the now-unused overrides.
- [x] 5. `ReusableInstance`s and `commonOwnerMap` into `Scratch`, always enabled.
- [x] 6. F2–F7, F9: call sites use per-thread instances of the collectors / maps held in `Scratch` (they already save and restore their state on reentrance, so one instance per thread suffices; F7 allocates per call, as `Erasure` is outside `SymbolTable`).
- [x] 7. Benchmark: JMH `ReflectionTypeOpsBenchmark` (1 and 4 threads) before/after; `compiler/compile` / `HotScalacBenchmark` for batch.
- [x] 8. Write-up.
- [x] 9. `findMember` takes its `FindMember` with `ReusableInstance.take`/`release` instead of the `using` closure (the batch allocation regression below).
- [x] 10. Kind D: `isConcurrentUniverse`; the `Scratch` holder, symbol ids (`util.IdCounter`), `synchronizeNames` and the uniques table (`PerRunUniques`, `SynchronizedPerRunUniques`, `SynchronizedWeakUniques`) chosen from it when the universe is built; the corresponding overrides in `SynchronizedTypes`/`SynchronizedSymbols`/`SynchronizedOps` removed. `intersectionWitness` shared (only touched under the GIL) and `ThreadLocalStorage` deleted.

Each step: `reflect/compile`, `compiler/compile`, `junit/testOnly scala.reflect.*`, partest `reflection-sync-*`, `t6240*`, `t7045`, `t6411*`, `lazy-concurrent`, `t6240-universe-code-gen` (regenerates `JavaUniverseForce`).

## Results

### What `Scratch` contains

`SymbolTable#Scratch` (`internal/SymbolTable.scala`) holds, for one thread: the subtyping guards (`subsametypeRecursions`, `pendingSubTypes`, `knownFalseSubTypes`), the `undoLog`, `skolemizationLevel`, the base type guards, the lub/glb caches and `globalGlbDepth`, the printing guards, `recursionTable`; the seven `ReusableInstance`s of `src/reflect` (now always enabled) plus a pool of `CommonOwnerMap`s; and per-thread instances of the stateful shared collectors (`ErroneousCollector`, `IsDependentCollector`, `IsRelatableCollector`, `UnrelatableCollector`, `wildcardExtrapolation`, the `maxTypes` collector). `SymbolTable` returns one lazily created instance; `SynchronizedTypes` returns one per thread (weak map owned by the universe, `ThreadLocal[WeakReference]` fast path). All twelve kind-A `mkThreadLocalStorage` overrides are gone; `intersectionWitness` (a cache) keeps the last one.

### What the inventory found

Nine places where per-operation state is shared between threads in runtime reflection, listed as F1–F9 under "Inventory". One, F9, is not hypothetical: four threads computing the same `lub` crash within a few dozen iterations (`ScratchTest.concurrentLubs`, and the baseline run of the benchmark). F1–F7 and F9 are fixed on the branch; F8 (registration into `perRunCaches`) is harmless in runtime reflection and left alone. F11/F12 (shared `Transformer`s) are outside type operations and left alone.

A second finding came from the implementation, not the inventory: the natural `ThreadLocal.withInitial(() => new Scratch)` leaks every universe a pooled thread has touched, because the value is an inner class of the universe that owns the `ThreadLocal`. The old `mkThreadLocalStorage` did not leak this way, as its map lives in the universe. The branch keeps that ownership and uses the `ThreadLocal` only as a weak cache.

### Cost in diff

Across `src/` and `project/`: 19 files, 286 insertions, 264 deletions (223/201 ignoring whitespace; `isSubType` is re-indented). `SynchronizedTypes` shrinks from 125 to 100 lines. Plus `ScratchTest` (56 lines) and the benchmark.

### Runtime reflection, before and after

`ReflectionTypeOpsBenchmark` (`test/benchmarks/src/main/scala/scala/reflect/runtime/`), JDK 21.0.12 (Temurin, the sbt server's), 3 forks × 5 × 1 s, average time per operation, before = `c803a64c44`, after = the branch tip:

```
bench/Jmh/run -t 1 ReflectionTypeOpsBenchmark
bench/Jmh/run -t 4 ReflectionTypeOpsBenchmark
```

"kind A" is the branch after step 8, "kind D" the tip after step 10.

| benchmark | -t 1 before | -t 1 kind A | -t 1 kind D | -t 4 before | -t 4 kind A | -t 4 kind D |
|---|---|---|---|---|---|---|
| `subtypeTrue` | 2009 ± 65 ns | 1612 ± 152 | 1474 ± 91 | 19853 ± 2130 | 9700 ± 620 | 10098 ± 765 |
| `subtypeFalse` | 3101 ± 170 | 2514 ± 211 | 2383 ± 135 | 21244 ± 902 | 16533 ± 868 | 15878 ± 696 |
| `asSeenFrom` | 18229 ± 2138 | 17140 ± 738 | 16614 ± 166 | 85824 ± 4511 | 84703 ± 4052 | 84615 ± 4609 |
| `members` | 4278 ± 234 | 4423 ± 619 | 4230 ± 195 | 28680 ± 1775 | 28828 ± 2496 | 25895 ± 1022 |
| `lubLeafOther` | 7.91 ± 0.39 ms | 8.59 ± 1.12 ms | 7.39 ± 0.36 ms | 52.6 ms, 1 of 3 forks (the others crashed, F9) | 54.0 ± 5.2 ms | 52.7 ± 3.0 ms |

Subtyping gains 20% on one thread and 2× on four, from replacing per-access monitor-guarded map lookups by one fetch per operation. `asSeenFrom`, `members` and `lub` don't move: they are dominated by the GIL. Even after the change, four threads are slower per operation than one; a stack profile of `subtypeTrue -t 4` shows 48% of samples `WAITING` in `ReentrantLock` (the GIL), entered from `SynchronizedSymbol.rawInfo`/`validTo` via `gilSynchronizedIfNotThreadsafe`: symbols reached by these operations are not marked thread-safe, so every `info` takes the GIL. `lub` creates fresh refinement symbols, which are never thread-safe, and is GIL-bound even on one thread (7.9 ms for one lub of two small class types).

### Batch compilation

At the tip (campaign `reflect-scratch-typer-2`, same setup as below), batch compilation pays nothing measurable:

| corpus | time, branch vs base | alloc/op, branch vs base |
|---|---|---|
| re2s | +0.72% ± 1.28 | +0.03% ± 0.88 |
| scalap | −0.15% ± 0.87 | +0.79% ± 0.33 |

That needed step 9. The history, after step 8:

`CompileBenchmark` in jvm-perf-lab's scala2 lab, typer only, the two builds paired within each fork (3 forks per corpus), JDK 21.0.12, campaign `reflect-scratch-typer`:

| corpus | time, branch vs base | alloc/op, branch vs base |
|---|---|---|
| re2s | +3.96% ± 2.81 (forks +1.1, +4.2, +6.7%) | +6.21% ± 1.69 (forks +6.8, +7.5, +4.3%) |
| scalap | +1.69% ± 2.20 (forks −0.4, +4.0, +1.5%) | +3.44% ± 0.80 (forks +4.2, +3.6, +2.6%) |

The lab's A/A control (one build against itself, scalap) moves by up to 4.7% in time and 4.4% in allocation per fork, so the time difference is within noise, but the re2s allocation difference is not. Batch compilation does not pay nothing in these builds.

An allocation profile of each build (campaign `reflect-scratch-typer-alloc`, `perflab profdiff --event alloc`) puts the whole difference in one class: `Types$Type$$Lambda`, from 0 to 5.5 MB per op (+6.7% of all allocation), allocated in `Type.findMember` by the closure passed to `ReusableInstance.using`. The base build has the same closure (`findMemberInstance.using { ... }`), but C2 scalar-replaces it there. `-XX:+PrintInlining` shows why this is fragile: `ReusableInstance.using` is 116 bytes and shared by every pool, so whether C2 inlines it into `findMember` (and can then eliminate the closure) depends on how it was compiled before; in the branch's run it is reported as "callee is too large" at some sites. Making `_scratch` a plain `val` instead of a `lazy val` did not change the result (the allocation stayed at 5.5 MB per op).

Both builds here are `quick` builds, which are compiled without Scala's optimizer. Release builds compile `scala/**` with `-opt:l:inline -opt-inline-from:scala/**`, which inlines `@inline def using` and removes the closure at compile time, so I expect the shipped compiler not to show this. I have not measured that. Step 9 takes `findMember` off the closure (`ReusableInstance.take`/`release`); the allocation profile then shows no `Types$Type$$Lambda` and 43.48 MB/op against the base's 43.34.

## Future work (not in this spike)

- Replace the GIL with an owner-and-barrier per completion root (the Scala 3 kind B design): a completer is owned by the thread that starts it; other threads wait on its barrier rather than on a global lock; cycles within one owner keep raising `CyclicReference`. This is where the remaining multi-thread cost is (see "Results"): `Scratch` removes kind A from the picture, so what is left contending is completion and the `info` of symbols that are not marked thread-safe.
- `intersectionWitness` as a concurrent weak map shared between threads instead of per-thread copies.
- A compiler-side composite for the `ReusableInstance`s in `Contexts`, `Implicits` and `SymbolLoaders`, if the presentation compiler is to run several readers in Scala 2 as well.
- `Erasure`'s boxing maps (F7) allocate per call in runtime reflection; a per-thread holder for components outside `SymbolTable` would fix that.
