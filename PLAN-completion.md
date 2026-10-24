# Replace the GIL with an owner and a barrier per completion

Branch: `claude/reflect-completion-barrier`, stacked on `claude/reflect-scratch-4f66f7` (retronym/scala#169). Scope: kind B (lazy facts with a black hole), and the kind C caches that the GIL also guards, in runtime reflection. The batch compiler must not change.

## Problem

Runtime reflection serialises every symbol operation through one lock, the GIL (`runtime.Gil`, a `ReentrantLock`). After #169, it is what limits several threads using one universe: on `ReflectionTypeOpsBenchmark.subtypeTrue -t 4`, 48% of samples wait for the GIL, entered from `SynchronizedSymbol.rawInfo` and `validTo`. Four threads are slower per operation than one.

The GIL is taken even for symbols that are fully completed. `gilSynchronizedIfNotThreadsafe` has a fast path for those, but it has been commented out since January 2014 ("turns off the gilSynchronizedIfNotInited optimization", after I reported that it was still causing trouble). Just before that, "SI-8131 fixes residual race condition in runtime reflection" found that completers call `setInfo` before they are done, so a symbol's info being set does not mean its completion has finished. It replaced that test with an explicit volatile mask (`_initialized`, `_initializationMask`, set by `markAllCompleted`/`markFlagsCompleted`). Some race remained, and was never found.

A global lock was the simple answer because completion is reentrant across symbols: completing one class forces the infos of others. One lock per symbol would deadlock when thread T completes A then B while thread U completes B then A.

## What the GIL protects today

| site | what it guards | kind |
|---|---|---|
| `SynchronizedSymbol`: `info`, `rawInfo`, `validTo`, `exists`, `typeSignature`, `typeSignatureIn`, `typeConstructor`, `typeParams`, `unsafeTypeParams`, `tpe_*` | symbol completion, and reads of a symbol's `infos`, `_validTo`, `_rawflags` (including `LOCKED`) | B |
| `getFlag`, `privateWithin`, `annotations` | call `initialize` when the symbol's mask says the flag may still change | B |
| `SynchronizedTypes.define*` (underlying of `SingleType`, base type seq and base classes of `CompoundType`, parents and base type seq of `TypeRef`, `normalize`) | lazily computed facts of types, which force symbol infos | B and C |
| `SynchronizedBaseTypeSeq` | lazy elements of base type sequences that contain refined types | B and C |
| `PackageScope.lookupEntry` (`syncLockSynchronized` is the GIL there) | creating class and module symbols on first lookup in a package | B |
| `FromJavaClassCompleter.completeRest` | completing a symbol from Java reflection | B |
| `TwoWayCaches` (`toScala`, `toJava`, `enter`) | Java to Scala symbol caches of each mirror | C |
| `runtimeMirror`, `makeScalaPackage`, `connectModuleToClass` | mirror creation, package creation, companion linking | B |

`SynchronizedScope` uses its own lock per scope and is not the GIL.

## Design

### Completion

Each pending completion gets an owner. A thread that forces a symbol whose completion is in progress on another thread waits for that completion to finish, instead of waiting for a global lock.

A symbol is in one of three states:

| state | meaning | a thread forcing it |
|---|---|---|
| not started | its info is a `LazyType` nobody is running | claims it (CAS), runs the completer, publishes |
| in progress (owner, barrier) | a thread is running its completer | the owner: today's behaviour (`LOCKED` → `CyclicReference`); any other thread: waits on the barrier |
| done | completed and published | reads without a lock |

- **Unit of ownership.** One completer can complete several symbols: `TopClassCompleter` completes a class, its module and the module class together. The owner and barrier belong to the completer (the `LazyType`), and each symbol it completes points to it while it runs.
- **Publication.** The completer's writes (`infos`, `_validTo`, flags, the decls it enters) must be visible to a thread that sees "done". `markAllCompleted` is already a volatile write, and the fast path's check a volatile read; the publication rule is that "done" is written last, after everything the completer writes. The 2014 race is most likely a site where that does not hold (see step 1).
- **Failure.** If a completer throws, its symbols go back to "not started" (or to an error state that every thread sees), and the barrier opens. Waiters retry or see the error. Today a failed completion can leave a symbol half set up, which is visible to other threads once they get the GIL.
- **Cycles across threads.** T owns A and waits for B; U owns B and waits for A. Each completion is on its owner's stack, so A's completion needs B and B's needs A: run on one thread, this is a cycle that `LOCKED` reports as `CyclicReference`. So a wait-for cycle is a dependency cycle, and the thread that closes it throws `CyclicReference` instead of waiting. This needs a wait-for graph: each waiting thread records the completion it waits for; before waiting, a thread follows owner → waited-for completion → owner until it reaches a thread that is not waiting, or itself.
- **Peeks.** Some code reads in-progress state without forcing completion: `rawInfo` (returns the `LazyType`), `hasCompleteInfo`, `isInitialized`, `lockOK`, `validTo`. Under the GIL these could not see another thread's in-progress state. With barriers they can, so a peek at a symbol in progress on another thread must wait, and only the owner may see the intermediate state. Step 2 lists every peek and gives each one of these two meanings.

The single-threaded universes keep today's code. Following #169, the choice is made once from `isConcurrentUniverse`: the compiler calls the completer directly, and runtime reflection goes through the owner and barrier. That also replaces the thirteen `createXSymbol` overrides that exist only to mix `SynchronizedSymbol` into every symbol.

### Lazy vals and objects

A Scala 2 lazy val, and a nested `object`, initialises under the monitor of its enclosing instance. That is a second family of locks, held during arbitrary code, which the GIL already interacts with and which a barrier will too:

- T owns completion C and, inside it, reads lazy val `o.x`, which U is initialising. T waits for `o`'s monitor.
- U, inside `o.x`'s initialiser, holding `o`'s monitor, forces a symbol whose completion is C. U waits on C's barrier (today: on the GIL, which T holds).

Neither thread can proceed, and a wait-for graph that only records barriers cannot see it. The same deadlock is possible with the GIL today, wherever the lock order is "monitor, then GIL" on one path and "GIL, then monitor" on another. `SynchronizedBaseTypeSeq.maxDepth` is one: a lazy val (monitor of the base type sequence) whose initialiser takes the GIL, read by code that may already hold the GIL.

`JavaUniverseForce` removes most of this for the universe itself. `JavaUniverse` calls `force()` while it is being constructed, before any other thread can see it, and that initialises about 540 lazy vals and objects of the universe and `definitions`. What it does not reach:

- lazy vals of each `JavaMirror`: `runDefinitions` (a `RunDefinitions`, itself mostly lazy vals), `RootPackage`, `RootClass`, `EmptyPackage`, `EmptyPackageClass`, `rootLoader`, `bytecodelessMethodOwners`, `bytecodefulObjectMethods`. A mirror is created per class loader, on demand, from any thread;
- lazy vals of instances: `SynchronizedBaseTypeSeq.maxDepth`, the lazy vals of `Symbols` and `Types` instances, and the member mirrors' `jfield`, `jmeth`, `jconstr`, `boxer`, `unboxer`;
- members it lists as inaccessible (one today, `posAssigner`), and private lazy vals of the universe, such as `scratches` from #169. (`scratches` is in fact initialised during `force()`, because forcing performs type operations, but nothing checks that.)

The design takes these rules:

1. A lazy val or object initialiser in runtime reflection must not force a completion, unless it is forced before its instance is visible to other threads. Step 2 checks every lazy val reachable from a completion or a type operation against this rule. A lazy val that breaks it becomes an eager val, is initialised before publication (as `JavaUniverseForce` does for the universe, and as a new force step would do for each mirror), or becomes a completion with its own barrier.
2. Barriers are `ReentrantLock`s that the owner holds while it completes, instead of latches. The JVM then knows who owns each barrier, and `ThreadMXBean.findDeadlockedThreads` reports cycles that mix barriers with lazy-val monitors. A thread that waits on a barrier for longer than a timeout asks it. If the thread is on a reported cycle, it fails with a diagnostic naming the threads and the locks, instead of hanging. The explicit wait-for check stays as the cheap path for barrier-only cycles.
3. No barrier is waited on while holding a monitor of the universe or a mirror, where the code can know that it does (`Thread.holdsLock` in assertions under a debug flag).

### Caches and tables

- `define*` facts of types: computed without a lock and published through one field, when the computation is idempotent (any thread computes an equal value). Where a computation creates symbols (base type sequences of refined types do), it is a completion and follows the protocol above.
- `PackageScope`: a lock per package, and symbols created once (`computeIfAbsent`-style), so two threads looking up the same class get the same symbols.
- `TwoWayCaches`: concurrent maps with one computation per key.
- `runtimeMirror`: a concurrent map from class loader to mirror.

## Steps

- [ ] 1. Reproduce the 2014 race. Enable the commented-out fast path, run the `reflection-sync-*`, `t6240*`, `t7045`, `t8946` tests and `ScratchTest` in a loop, and add a stress test: several threads forcing the infos of many classes in a fresh universe, compared with a single-threaded oracle (the printed signatures). Find the completer that marks its symbols done too early, or leaves one unmarked.
- [ ] 2. Inventory: every completer in runtime reflection (unpickler, `TopClassCompleter`, `LazyPackageType`, `FromJavaClassCompleter`, `PackageScope`), what each writes and where it calls `markAllCompleted`; every peek at in-progress state; every lazy val and object reachable from a completion or a type operation, checked against the lazy-val rule (`JavaUniverseForce` coverage, per-mirror lazy vals, lazy vals of instances).
- [ ] 3. Fix publication so that "done" is written last; enable the fast path. Completed symbols are then read without the GIL. Measure: this alone may remove most of the contention seen in #169's benchmark.
- [ ] 4. Owner and barrier (a `ReentrantLock`) per completer, with the wait-for check and the timed deadlock diagnostic, replacing the GIL for completion. A force step for each new mirror, alongside `JavaUniverseForce`. Completion strategy chosen from `isConcurrentUniverse`; remove the `Synchronized*Symbol` mixins and `createXSymbol` overrides.
- [ ] 5. `define*` caches, `SynchronizedBaseTypeSeq`, `PackageScope`, `TwoWayCaches`, `runtimeMirror` as above.
- [ ] 6. Remove `Gil`.

Each step: `reflect/compile`, `compiler/compile`, MiMa, the reflection JUnit tests and thread-safety partests, and the stress test from step 1. Before merging: full JUnit and partest.

## Measurements

- `ReflectionTypeOpsBenchmark` with `-t 1`, `-t 4`, `-t 8`, against #169's branch.
- A cold-start benchmark (new): a fresh universe per iteration, N threads completing different classes. The GIL serialises this completely today; it is where per-completer ownership should show.
- Batch: the paired `CompileBenchmark` A/B, typer only, to confirm the compiler pays nothing.

## Risks

- A completer that writes shared state outside its own symbols (for example, entering into a package's scope, or a mirror's caches) is not covered by its own barrier. Step 2 has to find these.
- `CyclicReference` from a wait-for cycle is new behaviour for runtime reflection, where today the same program deadlocks never (one lock) and reports a cycle only within one thread.
- A thread interrupted while waiting on a barrier must leave the wait-for graph consistent.
- Lazy vals in user code are out of reach: a user's lazy val whose initialiser uses reflection, read from two threads, can still deadlock against a barrier, as it can against the GIL today. The timed diagnostic at least turns that hang into an error.
