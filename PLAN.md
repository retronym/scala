# Spike: one composite for per-thread scratch state in `SymbolTable`

Branch: `claude/reflect-scratch-4f66f7`. Scope: kind A only (per-operation scratch state that runtime reflection must keep per thread). The GIL, `LOCKED`, uniques, `perRunCaches` and the derived-type caches stay as they are.

## Problem

`scala.reflect.internal.SymbolTable` keeps the scratch state of its type operations (recursion counters, pending sets, the undo log, lub/glb memo tables) as fields scattered over the cake: `Types`, `TypeComparers`, `TypeConstraints`, `GlbLubs`, `TypeToStrings`, `Symbols`. Each field is reached through a `def` so that `scala.reflect.runtime.SynchronizedTypes`/`SynchronizedSymbols` can override it with `mkThreadLocalStorage(...)`, one override (and for `var`s, a getter/setter pair) per field. Two consequences:

- The set of "this must be per thread" state is defined only implicitly, by which accessors the runtime remembered to override. Fields added later without an override are silently shared between threads (findings F1–F6 below).
- `mkThreadLocalStorage` is a `Collections.synchronizedMap(new WeakHashMap[Thread, T])`: every access takes a monitor and hashes `currentThread`. `isSubType` touches `subsametypeRecursions` four times, `undoLog` twice and `knownFalseSubTypes`/`pendingSubTypes` on the slow path, each a separate locked lookup.

The Scala 3 presentation compiler (retronym/scala3#84) solved the same problem by gathering every such field into one object reached through a single accessor that is a plain field in batch and a `ThreadLocal` in the concurrent universe. This spike does the same for Scala 2.

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

- `internal.SymbolTable` holds one `Scratch` in a field initialised before any trait body can run a type operation; `def scratch` returns it. Batch (`Global`, toolbox, presentation compiler) pays a field load per fetch.
- `runtime.SynchronizedOps` overrides `scratch` with one `java.lang.ThreadLocal[Scratch]` (`withInitial`). The 12 kind-A `mkThreadLocalStorage` overrides go away. `ReflectGlobal` (compiler universe + runtime traits) inherits the `ThreadLocal`, as it inherits the overrides today.
- The old accessors (`undoLog`, `skolemizationLevel`, `lubGlbCaches`, ...) stay as non-overridden forwarders to `scratch.x` for source compatibility with the compiler, scaladoc, toolbox and third-party plugins. MiMa filters for the removed runtime overrides as needed.
- Hot paths fetch once: `val s = scratch` at entry of `isSubType`, `isSameType`, `isDifferentType`, `withLubGlbCaches`/`claimLubGlbCaches`, `baseTypeOfNonClassTypeRef`, the `glb` refinement branch, `typeToString`, `Symbol.lock`/`unlock`. The undo-log users in the typer (`Implicits`, `Analyzer`) keep `undoLog`; they are compiler-only.
- `java.lang.ThreadLocal` instead of the `synchronizedMap(WeakHashMap)`: the composite is reachable from the thread only, so it is collected with the thread; `ThreadLocalStorage` remains for `intersectionWitness`.
- Reentrant scratch objects are `ReusableInstance`s held in `Scratch`, always enabled: the seven `enabled = isCompilerUniverse` instances in `src/reflect` move in, so runtime reflection reuses them per thread instead of allocating per call. The shared singletons with mutable state (F2–F7) get the same treatment: call sites go through `scratch.erroneousCollectors.using(_.collect(tp))` etc. The public `object`s stay for binary compatibility and direct users. `commonOwnerMap` likewise.
- `ReusableInstance` allocates its first instance eagerly, so the `Scratch` field must be initialised after the traits whose classes it instantiates; either a `lazy val` or construction at the end of `SymbolTable`'s initialiser. Pick whichever keeps `scratch` a plain field load in batch.
- Compiler-side `ReusableInstance`s (`Contexts`, `Implicits`, `SymbolLoaders`) stay as they are: `Scratch` is a `final class` in `src/reflect`. If they matter, `Global` can hold a second, compiler-side composite reached the same way; not in this spike.

What stays: everything per run (`perRunCaches`, `intersectionWitness`, uniques), the GIL and every `gilSynchronized`, `LOCKED`.

## Steps

- [ ] 1. `Scratch` + `scratch` (val / `ThreadLocal`), move the subtyping group: `subsametypeRecursions`, `pendingSubTypes`, `knownFalseSubTypes`, `undoLog`; fetch once in `isSubType`/`isSameType`/`isDifferentType`.
- [ ] 2. Move `skolemizationLevel`, `basetypeRecursions`, `pendingBaseTypes`.
- [ ] 3. Move `lubGlbCaches`; add `globalGlbDepth` (F1).
- [ ] 4. Move `indent`, `toStringRecursions`, `toStringSubjects`, `recursionTable`; delete the now-unused overrides.
- [ ] 5. `ReusableInstance`s and `commonOwnerMap` into `Scratch`, always enabled.
- [ ] 6. F2–F7, F9: call sites use per-thread instances of the collectors / maps held in `Scratch` (they already save and restore their state on reentrance, so one instance per thread suffices; F7 allocates per call, as `Erasure` is outside `SymbolTable`).
- [ ] 7. Benchmark: JMH `ReflectionTypeOpsBenchmark` (1 and 4 threads) before/after; `compiler/compile` / `HotScalacBenchmark` for batch.
- [ ] 8. Write-up.

Each step: `reflect/compile`, `compiler/compile`, `junit/testOnly scala.reflect.*`, partest `reflection-sync-*`, `t6240*`, `t7045`, `t6411*`, `lazy-concurrent`, `t6240-universe-code-gen` (regenerates `JavaUniverseForce`).

## Future work (not in this spike)

- `intersectionWitness` as a concurrent weak map shared between threads instead of per-thread copies.
- Replace the GIL with an owner-and-barrier per completion root (the Scala 3 kind B design): a completer is owned by the thread that starts it; other threads wait on its barrier rather than on a global lock; cycles within one owner keep raising `CyclicReference`.
