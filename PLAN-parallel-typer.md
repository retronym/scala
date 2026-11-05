# Parallel typer for Scala 2: plan and log

A spike: port Scala 3's `-Yparallel-typer` (retronym/scala3#84) to the Scala 2 compiler. `scalac -Yparallel-typer -Ystop-after:pickler` typechecks the units on several threads over one `Global` and must produce what the serial compiler produces. Built on retronym/scala#212 (`Scratch`, `Frame`, `IdCounter`, `SynchronizedPerRunUniques`, `synchronizeNames`). Compatibility (MiMa) may break where pragmatic; the presentation compiler, the REPL and the phases after the pickler are out of scope.

## Design

### The shape of a run

Parser, namer and packageobjects run serially as before. The typer phase (`Analyzer.typerFactory.TyperPhase.run`) hands its units to `ParallelTyper.typeCheckInParallel`, which on the main thread:

1. completes the signature of every member the sources define ("signatures first", serial, the default);
2. forces the lazy vals of `definitions`, `runDefinitions` and the private lazy vals of the cake (lock order, below);
3. types the bodies on a pool of workers, a unit per task, largest first; each worker sets its own phase, current unit and scratch state, and clears its undo log after each unit;
4. after the join, issues the buffered diagnostics in a fixed order; `finishComputeParamAlias` and `clearDelayed` then run on the main thread as before, and so do the phases after the typer.

System properties for measuring: `scalac.parallelTyper.lazySignatures` (no pre-pass), `.oneThread` (the parallel path on one worker: order effects without concurrency), `.threads=N`, `.log`, `.timeline=<file>`, `.watchdog=<seconds>` (thread dump of a phase that runs longer, default 120). `-Dscalac.debug.typeErrorStacks` keeps the stacks of type errors (for the false cycles that are thrown without one).

### Why signatures first is the default

A Namer completer for a symbol of unit B runs on whichever thread first asks for its info, with B's namer context: it types B's trees in place (an inferred result type types the body), writes B's per-unit maps (`unit.transformed`, `synthetics`, `toCheck`), enters symbols into B's scopes (members, default getters, a value class's companion) and mutates B's contexts (`implicitsRunId` in progress). The completion lock serialises completers against each other, not against B's own thread typing B's bodies outside any completion. Per-unit locks would order a unit lock against the completion lock in both directions (A's thread holds A and completes B's symbol; B's thread holds B and completes A's), so there is no cheap tighter argument. Completing every source signature serially before the workers start removes the cross-unit completer, as Scala 3 does.

What the pre-pass does not cover: definitions local to a body (confined to the thread typing that body, which is the unit's own); classpath completions (loaders and unpickling, which still run under the completion lock from any thread); members that the typer itself adds to a class while typing its template (`addSyntheticMethods`: the case class methods), see open problems.

The pre-pass completes, per unit in tree order, the imports at package level (an `import scala.Predef.{...}` excludes the root import for the rest of the unit, `Contexts.registerImport`), classes and objects with their type parameters, self type, members (from the scope, so that synthetic ones are included, to a fixed point) and annotations, the members' parameters, and the unit's `synthetics`. A type error that escapes a completion there is reported at the symbol.

Cost, measured on `src/library` (574 units): the pre-pass takes about 2.0-2.5 s of a typer phase whose serial total is about 4-5 s, because in Scala 2 an inferred result type types its body inside the completer. That is the serial fraction that bounds the speed-up (Amdahl), see Measurements.

### The completion lock (`SymbolTable.completionLock`, `util.CompletionLock`)

One reentrant lock, behind a small facade (`CompletionLock`: `acquire`, `release`, `await`, counters) so that a claim per completion root can replace it. Present only in a concurrent compiler (`isConcurrentCompiler`: `isCompilerUniverse && isConcurrentUniverse`; runtime reflection keeps its GIL); everywhere else `underCompletionLock(body)` is `body`.

Every completion runs under it: `Symbol.info` takes it on the slow path (`_validTo == NoPeriod`, or the symbol is completing on another thread) and re-reads the state under it; `typeParams` and `exists` (which call `LazyType.load`, a completion without `LOCKED`) do the same. Macro expansion runs under the same lock (not a lock of its own, which would be taken in both orders with completion).

### In-progress protocols: who may see what

The rule from Scala 3 holds: "in progress on another thread" is never a state to act on, only to wait for. Per protocol:

- `LOCKED` and provisional infos. `Symbol._completingThread` (volatile) is set while a thread completes the symbol, only while it holds the lock, before `LOCKED` is set (so a thread that sees the flag can tell whose it is), and restored afterwards. `info` and `rawInfo` treat a symbol completing elsewhere as not completed and wait for the lock; the completing thread sees its provisional infos as in a serial compiler (a class's info before the value class check, a method's inherited result type, `safeNextOverriddenSymbol`'s `ErrorType`). `info_=` clears `LOCKED` between completion steps, which is harmless because no other thread reaches `completeInfo` without the lock. Peeks at `LOCKED` (`Implicits.isCyclicOrErroneous`, `Infer.checkAccessible`, the alias checks in `typeRef`/`copyTypeRef`) go through `isLockedHere`, which waits for another thread's completion and reads the flag again. Publication: marker, store-store fence, provisional info; a reader loads `_validTo`, a load-load fence, then the marker.
- Stamped caches on shared types (`TypeRef.parents/baseTypeSeq`, `CompoundType.baseTypeSeq/baseClasses`, `SingleType.underlying`). Each definition stamps its period before it stores its value and marks it in progress (`undetBaseTypeSeq`, null, the initial `NoType`). The whole slow path, the read of the field after the definition included, is under the completion lock; the fast path accepts only a value that is not a marker. Under the lock a marker is seen only by the thread that set it, i.e. as a real cycle. Residual: the fast path can pair a new stamp with the previous value; within the typer's phases no info transformer changes base classes, so the previous value is the same value (`isValidForBaseClasses`). Found the hard way, I2.
- `TypeSymbol.tpeCache` (`NoType` as cycle marker, stamped first): a concurrent compiler takes the fast path only for a value that is neither null nor `NoType`, else updates under the lock.
- `BaseTypeSeq.pending` (per-instance bit set of elements being merged): a lazy element is evaluated under the lock; an element that is no longer lazy was stored after its mark was cleared.
- `LazyAnnotationInfo.forcedInfo` (a lazy val whose initialiser types the annotation): the lock is taken before the lazy val's monitor, never after.
- `MethodSymbol.typeAsMemberOf` (four fields written one by one): no cache in a concurrent compiler. `ModuleClassSymbol.implicitMembers` (three fields): one immutable holder, in both modes.
- `ClassInfoType.expansiveRefs`: not yet done (TODO; called from `typedClassDef` on classes of the unit, and on their parents).

### Lock order

Lazy vals hold the monitor of their enclosing object while the initialiser runs. Any lazy val whose initialiser completes a symbol (about 300 in `Definitions`/`RunDefinitions`) would deadlock against the completion lock: A holds the lock and wants the monitor, B holds the monitor and wants the lock. Rules: (1) force them all on the main thread before the workers start (`ParallelTyper.forceLazyVals`, a reflective sweep of `$lzycompute` and `$module` accessors, after the signatures: forced before, `Predef_???` met Predef mid-completion when compiling the library, I3); (2) a per-instance lazy val that completes symbols takes the completion lock before its monitor (`forcedInfo`); (3) no `synchronized` block in this work calls anything that can complete a symbol: maps that need a computation (`samCache`, `rootImportsCached`) read under the monitor, compute outside it, store under it.

### Ambient state, per thread

- `phase`/`currentPeriod`: in `Scratch` (`ph`, `per`) when `phasePerThread`, else the plain fields. TODO: a field on the worker thread instead of the `ThreadLocal` + `WeakReference` lookup, and a measurement of the serial cost.
- `currentRun.currentUnit`, `currentFreshNameCreator`: `ThreadLocal`s under the flag.
- `lockedCount` (a completer in progress disables the member type cache): in `Scratch`.
- Positions: `atPos` uses a `PosAssigner` per thread.
- Typer side channels and memo tables, one per thread (`perThread`): implicit search caches (`implicitsCache`, `infoMapCache`, `improvesCache`, `HasMember` cache), `Shadower`'s and `Contexts`'s `ReusableInstance`s, `startContext` (the root of every context chain; its `implicitsRunId = CycleMarker` would read as a cycle on another thread), `lastAccessCheckDetails`, `isTyperInPattern`, `Infer.instantiate` (its `excludedVars`), the right-associative operator maps, the macro bookkeeping (`openMacros`, `delayed`, `forced`, `undetparams`, `hasPendingMacroExpansions`), `substTypeMapCache` (none when concurrent), and `Typers.scala` now uses `scratch.wildcardExtrapolation` (the shared object was a leftover of #212, also a fix for runtime reflection).

### Shared, synchronised

`Run.symSource` (a `ConcurrentHashMap` under the flag; and `Typers` writes it with `context.unit`, not the global current unit), `superConstructorCalls`, `typerFreshNameCreators`, `rootImportsCached`/`excludedRootImportsCached`, `originalOwnerMap`, `samCache`, `perRunCaches.recordCache`, `addendums`, `ImportTracking` (`-Wunused:imports`, on with `-Xlint`), `Run.advanceUnit`. `Scope.createHash` fills the table before publishing it.

### Reporting

`PerRunReporting` is under its own monitor (suppressions, suspended messages, feature warnings, text edits). Messages are rendered before the monitor is taken (`featureWarning`'s `construct` and site name), and nothing under it completes symbols; the reporter's monitor nests inside it. While the workers run, `global.reporter` is a `BufferingReporter`: it counts (so that `reporter.hasErrors` stays meaningful), buffers, and afterwards issues the messages to the real reporter sorted by file, position and arrival, where duplicate filtering and `-Xmaxerrs` happen. A known difference: `featureWarning` explains a feature only on its first occurrence, which is now the first in arrival order.

### Rejected under the flag

`-Ymacro-annotations`, analyzer and macro plugins, `-Ytyper-debug`, `-Yprofile-enabled`, statistics, `-Ycheck`, `-Ylog`, scaladoc: a global error. `compileLate` (a source found on the source path while the workers run) is a fatal error.

## Oracle

`tools/parallel-typer/oracle.sh <library|reflect|compiler|argsfile> [threads] [repeats]` runs the serial compiler, the parallel path on one thread, and N parallel runs, each to the pickler with `-Ypickle-write`, and compares the exit status, the sorted diagnostics and the `.sig` files byte for byte. `tools/parallel-typer/SigText.scala` prints a directory of pickles as text with scalap's printer, to see what a byte difference is.

## Race and order log

I-numbers as in the Scala 3 notes; O for order effects (the same on one thread).

- I1. `Contexts.symbolLookupCache`, a `ReusableInstance` shared by all threads: `IndexOutOfBoundsException: -2` in `ReusableInstance.using` from `lookupSymbol`. Fix: one per thread.
- I2. The read of a stamped cache after its definition was outside the lock: a thread at another phase (`enteringPhaseNotLaterThan`) redefined `Predef`'s `ClassInfoType.baseClasses` between the definition and the read, and the reader saw its null in-progress marker: "illegal cyclic reference involving object Predef" (a `RecoverableCyclicReference` thrown out of `checkUnused`). Fix: the whole slow path under the lock.
- I3. Forcing `definitions`' lazy vals before the signatures, when compiling the library: `Predef_???` asked for `Predef`'s members while `Predef` was not ready, the cycle left `baseClasses` stamped in progress for the period, and every later reader on any thread saw a cycle. Fix: force them after the pre-pass.
- O1. Signatures first changes which unit first completes a member, and with it whether `String` in a signature of `StringBuilder` and `WrappedString` is `Predef.String` or `java.lang.String`: `StringBuilder.scala` has `import scala.Predef.{...}`, which removes the root `Predef` import for the rest of the unit once it is completed (`registerImport`), so the answer depends on whether a signature was completed before or after that import, and in the serial compiler on which unit asked first. The types are equivalent (an alias and its expansion); the pickles differ in 2 of 805 files as text and in a third (`Predef.sig`) only in entry order. Completing package-level imports first in the pre-pass changed which signatures differ, not whether. A deterministic fix would complete package-level imports in the namer, for the serial compiler too.

## Measurements

`src/library`, 574 units, M-series laptop, cold `scalac` processes (`-J-Xss64m`), so these are first impressions, not benchmarks: serial typer phase about 4-5 s; parallel path on one thread: pre-pass 2.0 s, bodies 3.6 s; on 8 threads: pre-pass 2.1-2.6 s, bodies 2.2-2.4 s, with about 478k outermost acquisitions of the completion lock, held 1.2-1.3 s in total and waited for 11-12 s in total across threads. The lock is taken mostly by the slow paths of the stamped type caches, not by completions.

## DONE

- Stage 0: `-Yparallel-typer`, `-Yparallel-typer-threads`; `Global.isConcurrentUniverse` from the setting; per-thread phase, current unit, fresh name creator.
- Stage 1: completion lock and the protocols above; driver with signatures first, lazy val forcing, watchdog, timeline, buffering reporter; the confinement and synchronisation above; oracle script.

## TODO (stage 1)

- Repeat the oracle on the library 20 times at 8 threads; reflect; compiler.
- `ClassInfoType.expansiveRefs` under the lock.
- `_rawflags` read-modify-write on shared symbols (`cookJavaRawInfo`'s `TRIEDCOOKING` and `rawToExistential` on classpath Java symbols): under the lock, or an atomic updater if the oracle shows lost updates.
- `addSyntheticMethods` enters case class members into decls while other threads look them up: confirm the publication argument (scope entries are prepended; the hash table is published filled), or a serial pre-pass.
- Cross-unit `toCheck` additions after the home unit drained them (only from completers, so covered by the pre-pass unless lazy signatures).
- Account for each section of the state map worklist (tools/state-map, merged at the milestone).
- Serial regressions: partest pos/neg slice, junit for the touched areas.

## Future work (stage 2)

- Timeline per worker; quantify lock waiting by caller (the type caches dominate the acquisitions).
- Phase register as a field of the worker thread.
- A claim per completion root (the `PerKeyClaims` of scala-sharing) instead of one lock; per-thread in-progress markers for the stamped caches so that their slow paths need no lock.
- Deterministic symbol ids per unit if `isLess` ties show in pickles.
