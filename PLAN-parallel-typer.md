# Parallel typer for Scala 2: plan and log

A spike: port Scala 3's `-Yparallel-typer` (retronym/scala3#84) to the Scala 2 compiler. `scalac -Yparallel-typer -Ystop-after:pickler` typechecks the units on several threads over one `Global` and must produce what the serial compiler produces. Built on retronym/scala#212 (`Scratch`, `Frame`, `IdCounter`, `SynchronizedPerRunUniques`, `synchronizeNames`). Compatibility (MiMa) may break where pragmatic; the presentation compiler and the REPL are out of scope. The phases after the pickler came later, see "Parallel late phases".

## Design

### The shape of a run

Parser, namer and packageobjects run serially as before. The typer phase (`Analyzer.typerFactory.TyperPhase.run`) hands its units to `ParallelTyper.typeCheckInParallel`, which on the main thread:

1. completes the class headers serially; a member's signature is then completed by the first thread that asks for it, under a claim per symbol, and its annotations by its own unit's task (the default since 6 Nov; `-Dscalac.parallelTyper.serialSignatures` restores the wholly serial pre-pass under the one lock, `poolSignatures` the members' own pool step);
2. forces the lazy vals of `definitions`, `runDefinitions` and the private lazy vals of the cake (lock order, below);
3. types the bodies on a pool of workers, a unit per task, largest first; each worker sets its own phase, current unit and scratch state, and clears its undo log after each unit;
4. after the join, issues the buffered diagnostics in a fixed order; `finishComputeParamAlias` and `clearDelayed` then run on the main thread as before, and so do the phases after the typer.

System properties for measuring: `scalac.parallelTyper.lazySignatures` (no pre-pass), `.oneThread` (the parallel path on one worker: order effects without concurrency), `.threads=N`, `.log`, `.timeline=<file>`, `.watchdog=<seconds>` (thread dump of a phase that runs longer, default 120). `-Dscalac.debug.typeErrorStacks` keeps the stacks of type errors (for the false cycles that are thrown without one).

### Why signatures first is the default

A Namer completer for a symbol of unit B runs on whichever thread first asks for its info, with B's namer context: it types B's trees in place (an inferred result type types the body), writes B's per-unit maps (`unit.transformed`, `synthetics`, `toCheck`), enters symbols into B's scopes (members, default getters, a value class's companion) and mutates B's contexts (`implicitsRunId` in progress). The completion lock serialises completers against each other, not against B's own thread typing B's bodies outside any completion. Per-unit locks would order a unit lock against the completion lock in both directions (A's thread holds A and completes B's symbol; B's thread holds B and completes A's), so there is no cheap tighter argument. Completing every source signature serially before the workers start removes the cross-unit completer, as Scala 3 does.

What the pre-pass does not cover: definitions local to a body (confined to the thread typing that body, which is the unit's own); classpath completions (loaders and unpickling, which still run under the completion lock from any thread); members that the typer itself adds to a class while typing its template (`addSyntheticMethods`: the case class methods), see open problems.

The pre-pass completes, per unit in tree order, the imports at package level (an `import scala.Predef.{...}` excludes the root import for the rest of the unit, `Contexts.registerImport`), classes and objects with their type parameters, self type, members (from the scope, so that synthetic ones are included, to a fixed point), the members' parameters, and the unit's `synthetics`. A type error that escapes a completion there is reported at the symbol. While doing so, `currentRun.currentUnit` is the unit being completed (as in the serial typer, a completion of another unit's symbol nested in it sees the outer unit, and so takes fresh names from it).

Annotations are forced in a second serial sub-pass, after every signature is complete, not interleaved with them: an annotation is lazy so that its arguments can refer to symbols still being completed. A `LazyAnnotationInfo` thunk types unit B's annotation trees with B's context, and is forced by whichever thread first asks for `sym.annotations` (`hasAnnotation`, `getAnnotation`: `@implicitNotFound`, `@compileTimeOnly`, `@deprecated`, `@nowarn`, ...), outside `Symbol.info`, so neither the lock nor a pass over `info` would cover it. A thunk that is still lazy after the sub-pass (local or synthetic symbols) is forced under the completion lock, taken before the lazy val's monitor. Annotations added during body typing (`annotate` in `typedDefDef`/`typedValDef`/`typedClassDef`, `@unchecked` on scrutinees, `T @ann` in an `AnnotatedType`) are on the unit's own symbols and trees, typed by the unit's thread; another unit can see them through a signature only after the pre-pass, and through a member's annotations only while the owning unit's typer adds them (the read-modify-write of `_annotations` on such a symbol is not atomic: noted, not seen). Deprecation and `@compileTimeOnly` checks at use sites run in refchecks, after the pickler, so serially. Classpath annotations are read by the unpickler and the classfile parser inside a loader's completion (the lock); Java annotation arguments in the classfile parser are not thunks over the shared buffer (TODO confirm for `-Ytasty-reader`).

The oracle has two small corpora of its own for this (`tools/parallel-typer/corpus/annotations`, `annotations-neg`): a deprecated member and class used from other files, annotation arguments that refer to the annotated class's own companion (a static and a constant annotation), `@implicitNotFound` consulted from two other files (errors), and a Java annotation with enum and array arguments, written in Java and used from Scala.

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
- `_rawflags`. Decision: atomic, not an argument about writers. Every read-modify-write of the flags (`setFlag`, `resetFlag`, `LOCKED` set and cleared) is a compare-and-set through a `VarHandle` (`SymbolFlagsHandle`) in a concurrent compiler; the serial compiler keeps the plain `|=`. The writers outside the lock are too many to argue about (the typer sets flags on symbols of other units and of the classpath: `TRIEDCOOKING`, `IS_ERROR`, `ACCESSED`...), and a lost update of `LOCKED` is either a missed cycle or a stuck lock. The blind stores (`flags_=`, `rawflags_=`, `initFlags`) stay plain: made by the creator, or under the lock on the symbol being completed (`safeNextOverriddenSymbol` restoring its flags). Stress corpus `corpus/stress-shared`: 48 units using raw Java collections and matching the sealed case class hierarchies of another unit, 50 runs at 8 threads, clean (after I6).
- Loaders. `SymbolLoader.complete` takes the completion lock itself and marks the roots it publishes together (the class, its companion module and module class) as completing, whoever calls it: `info`, but also `LazyType.load` from `Typer.isStale`, `typeParams`, `exists`. See I6.
- Scopes entered into during body typing (`addSyntheticMethods` adds `equals`, `hashCode`, `toString`, `productElement`... to a case class's decls while other units look its members up). Decision: a publication argument rather than moving the synthesis into the pre-pass (which would change when the synthetic bodies are typed relative to the user's, and needs the template's typer context). An entry's fields are written, then a release fence, then it is prepended to `elems` and to its bucket; a reader that follows the head or the bucket reaches a fully written entry and the rest of the chain behind it (dependent loads). `createHash` publishes a filled table. `elemsCache`/`cachedSize` are one holder keyed by the head they were computed from, so a list computed before an entry was added and stored after it is not taken for current. A reader may miss an entry being added: the same answer a serial compiler gives when the defining unit is typed later, and the members added this way all override members of `Any`/`Product`/`Equals` with the same signatures, so an inferred type or an overload resolution does not depend on which is found. Not covered: `unlink` and `rehash` in the middle of a chain (namer and package objects, serial) and `enterBefore` (serial). Corpus `corpus/case-classes`: 8 files of case classes and sealed hierarchies, 24 files using them (patterns, `copy`, `equals`, `hashCode`, `productIterator`, `productElementName`, `unapply`, `tupled`, inferred result types), 50 runs at 8 threads, clean.
- `ClassInfoType.expansiveRefs` (a state machine that propagates through other classes' states, `refs` reassigned while it runs): under the lock in a concurrent compiler.
- `cookJavaRawInfo` (sets `TRIEDCOOKING`, then rewrites the info of a shared Java symbol): under the lock, with the flag tested again.

### Lock order

Lazy vals hold the monitor of their enclosing object while the initialiser runs. Any lazy val whose initialiser completes a symbol (about 300 in `Definitions`/`RunDefinitions`) would deadlock against the completion lock: A holds the lock and wants the monitor, B holds the monitor and wants the lock. Rules: (1) force them all on the main thread before the workers start (`ParallelTyper.forceLazyVals`, a reflective sweep of `$lzycompute` and `$module` accessors, after the signatures: forced before, `Predef_???` met Predef mid-completion when compiling the library, I3); (2) a per-instance lazy val that completes symbols takes the completion lock before its monitor (`forcedInfo`); (3) no `synchronized` block in this work calls anything that can complete a symbol: maps that need a computation (`samCache`, `rootImportsCached`) read under the monitor, compute outside it, store under it.

### Ambient state, per thread

Rule: a per-thread object (`Scratch`, `Frame`, a `perThread` holder's value) is fetched into a local for the duration of an operation and never kept in a field of a `Typer`, `Context`, `Namer`, `ImplicitSearch`, completer or tree, nor captured by a closure that outlives the operation: completers, annotation thunks, deferred post-unit checks and delayed macro expansions capture typers and contexts and run later, possibly on another thread, where they must use that thread's state. All the state made per thread here is reached through `perThread(...).get` at the point of use (the implicit search caches, the shadower, `lastAccessCheckDetails`, `isTyperInPattern`, `instantiate`, the macro bookkeeping, `startContext`, the position assigner). `-Dscalac.debug.checkScratchOwner` records each `Scratch`'s thread and checks it where the phase register and the completion frame are written; the oracle's runs pass with it. Nothing was made "per typer" on the assumption that a typer is a thread.

- `phase`/`currentPeriod`: in `Scratch` (`ph`, `per`) when `phasePerThread`, else the plain fields. TODO: a field on the worker thread instead of the `ThreadLocal` + `WeakReference` lookup, and a measurement of the serial cost.
- `currentRun.currentUnit`, `currentFreshNameCreator`: `ThreadLocal`s under the flag.
- `lockedCount` (a completer in progress disables the member type cache): in `Scratch`.
- Positions: `atPos` uses a `PosAssigner` per thread.
- Typer side channels and memo tables, one per thread (`perThread`): implicit search caches (`implicitsCache`, `infoMapCache`, `improvesCache`, `HasMember` cache), `Shadower`'s and `Contexts`'s `ReusableInstance`s, `startContext` (the root of every context chain; its `implicitsRunId = CycleMarker` would read as a cycle on another thread), `lastAccessCheckDetails`, `isTyperInPattern`, `Infer.instantiate` (its `excludedVars`), the right-associative operator maps, the macro bookkeeping (`openMacros`, `delayed`, `forced`, `undetparams`, `hasPendingMacroExpansions`), `substTypeMapCache` (none when concurrent), and `Typers.scala` now uses `scratch.wildcardExtrapolation` (the shared object was a leftover of #212, also a fix for runtime reflection).

### Shared, synchronised

`Global.propagateCyclicReferences` (a counter that decides whether a cyclic reference propagates, now per thread), `BatchSourceFile.offsetToLine` (returned its cache field after storing it, so another thread's line; now a local), `Names._chrs` (volatile: a name made on one thread after the table grew is read on another), `loadMacroImplBinding` (under the completion lock), `Run.symSource` (a `ConcurrentHashMap` under the flag; and `Typers` writes it with `context.unit`, not the global current unit), `superConstructorCalls`, `typerFreshNameCreators`, `rootImportsCached`/`excludedRootImportsCached`, `originalOwnerMap`, `samCache`, `perRunCaches.recordCache`, `addendums`, `ImportTracking` (`-Wunused:imports`, on with `-Xlint`), `Run.advanceUnit`. `Scope.createHash` fills the table before publishing it.

### Reporting

`PerRunReporting` is under its own monitor (suppressions, suspended messages, feature warnings, text edits). Messages are rendered before the monitor is taken (`featureWarning`'s `construct` and site name), and nothing under it completes symbols; the reporter's monitor nests inside it. While the workers run, `global.reporter` is a `BufferingReporter`: it counts (so that `reporter.hasErrors` stays meaningful), buffers, and afterwards issues the messages to the real reporter sorted by file, position and arrival, where duplicate filtering and `-Xmaxerrs` happen. A known difference: `featureWarning` explains a feature only on its first occurrence, which is now the first in arrival order.

### Rejected under the flag

`-Ymacro-annotations`, analyzer and macro plugins, `-Ytyper-debug` (`typingStack`), `-Yprofile-enabled` (the profiler asserts the main thread; `ChromeTrace` is one JSON stack), statistics (`TimerStack`, plain counters), `-Ycheck`, `-Ylog` (the phase stack is shared), scaladoc (doc comment maps), `-Vimplicits` (splain's `ImplicitErrors` is one stack), `-Vcyclic` (the locking trace is one buffer): a global error. `-Wunused` is supported (`ImportTracking` is synchronised, `checkUnused` runs per unit on the unit's thread). `compileLate` (a source found on the source path while the workers run) is a fatal error.

## Oracle

`tools/parallel-typer/oracle.sh <library|reflect|compiler|argsfile> [threads] [repeats]` runs the serial compiler, the parallel path on one thread, and N parallel runs, each to the pickler with `-Ypickle-write`, and compares the exit status, the sorted diagnostics and the `.sig` files byte for byte. `tools/parallel-typer/SigText.scala` prints a directory of pickles as text with scalap's printer, to see what a byte difference is.

## Race and order log

I-numbers as in the Scala 3 notes; O for order effects (the same on one thread).

- I1. `Contexts.symbolLookupCache`, a `ReusableInstance` shared by all threads: `IndexOutOfBoundsException: -2` in `ReusableInstance.using` from `lookupSymbol`. Fix: one per thread.
- I2. The read of a stamped cache after its definition was outside the lock: a thread at another phase (`enteringPhaseNotLaterThan`) redefined `Predef`'s `ClassInfoType.baseClasses` between the definition and the read, and the reader saw its null in-progress marker: "illegal cyclic reference involving object Predef" (a `RecoverableCyclicReference` thrown out of `checkUnused`). Fix: the whole slow path under the lock.
- I3. Forcing `definitions`' lazy vals before the signatures, when compiling the library: `Predef_???` asked for `Predef`'s members while `Predef` was not ready, the cycle left `baseClasses` stamped in progress for the period, and every later reader on any thread saw a cycle. Fix: force them after the pre-pass.
- I6. `Typer.isStale` (from `reallyExists`, in identifier typing) calls `rawInfo.load(sym)` directly, a loader's completion outside `Symbol.info` and so outside the lock: two threads ran the one `ClassfileParser` on `java.util.Collections` at once, "unsatisfiable cyclic dependency in 'class Collections'", then broken class files everywhere (shared `readIndex`). Found by `corpus/stress-shared` (every run at 8 threads failed). Fix: `SymbolLoader.complete` takes the lock and marks its roots.
- I7. Body-local symbols complete without the lock (B.1), but "local" was decided from the owner chain alone, and the classpath has local symbols too: an unpickled existential or local class owned by a library method. Two threads completed `LazyTypeRef`s of one classfile at once: `IndexOutOfBoundsException` in `UnPickler.completingStack` (compiler corpus, 1 of 5 runs; re2s, 1 of 5). Fix: lock-free only for a symbol of a source being compiled (`completionRoot(sym)` is not the coarse root).
- I8. Not a race, a leak: the `ThreadLocal`s of the per-thread holders kept every `Global` a long-lived thread had used (their values hold the universe), so the warm benchmark ran out of a 6 GB heap after about thirty compilations at 8 threads. Fix: `PerThread` holds its values weakly from the thread, as `PerThreadScratches` does; the run's current unit and the position assigners use it.
- I9. Parallel signatures with a claim per unit: 48-62 false `CyclicReference`s per run on reflect, wait-for cycles between unit claims taken in opposite orders (see "Claims per root and parallel signatures"). Fixed by symbol roots with the namer's shared state made safe, and lending at a provisional info on a wait cycle (67f050d335).
- I12. Not a race, a serial regression found by `restarrFull`: the vendored scala-sharing was in package `io.github.retronym.sharing`, so scala-reflect.jar had a top-level package `io`, which shadows `scala.io` for code in the empty package or in a package clause nested in the root: scalap's `io.Codec` stopped compiling under the re-STARRed compiler (and for any user with scala-reflect on the classpath; partest did not meet it). Fix: relocated to `scala.reflect.internal.sharing`, with the relocation in the README's copy recipe.
- I10. A toolbox's `ReflectGlobal` is a compiler universe and (through runtime reflection's `SymbolTable`) a concurrent one, so it took the parallel typer's per-thread phase register, completion lock and per-thread holders: a toolbox used from a second thread would have started at `NoPhase`. Found reading the uniques' choice; partest's toolbox tests (which use one thread) did not show it. Fix: the parallel machinery keys on `isParallelCompiler` (the setting), not on `isConcurrentUniverse`.
- Not a race (an oracle artefact): in the 200-run oracle at a9e662c9a3, reflect runs 117-200 "differed" only in `CompletionLock.sig`, because the oracle read the corpus's sources (`src/reflect`) from the working tree, which I was editing during the run. The oracle now snapshots the sources of the in-repo corpora as it does the compiler. Runs 1-116 were clean.
- I11. "class HashMap cannot be instantiated because it does not conform to its self-type java.util.HashMap[String,Integer]" (stress-shared, 3-5% of cold runs at 8 threads, more with per-key claims; `ArrayList` and `TreeSet` too). Mechanism: a classfile loader run twice for one class. `Typer.isStale` (from `reallyExists`, on every selection of a classpath class) reads `sym.rawInfo` and, if it is a `ClassfileLoader`, calls `load` on it. Thread A reads the loader; thread B is loading the class under the completion lock; A's `load` goes to `SymbolLoader.complete`, waits for the lock, and then parses the classfile again, because `complete` did not look at the root again once it had the lock (`Symbol.info`, `typeParams` and `exists` do). The second parse sets a new info with new type parameter symbols and new members. A thread that read `sym.typeOfThis` (`tpe_*`, built from the first parameters) before and `sym.typeParams` after, in `TypeRef.relativize`, substitutes parameters that do not occur, so the self type stays `HashMap[K,V]` and `typedNew`'s conformance test fails; a moment later both are the second set and the same expression is right. Evidence, from `tools/parallel-typer/stress.sh stress-shared` (one JVM, a fresh `Global` per compilation, 65 failures in 300): at the failing check the arguments of `sym.typeOfThis` and `sym.typeParams` were different symbols of the same name (`E#30626` against `E#30824`, and a third set on another thread in the same run), and a probe in `SymbolLoader.complete` counted 425 loads of an already loaded root in 100 runs, every one from `isStale`. Fix: `SymbolLoader.complete` returns when, under the lock, the root's info is no longer this loader. After it: 0 in 1000 in-JVM runs with the coarse lock, 0 in 3000 with per-key claims, and the oracle 0 in 500 cold runs at 8 threads in each mode (`checkScratchOwner` on). The two earlier changes made under this number (symbols marked while cooked; `tpe_*` and `originalInfo` wait for a completion elsewhere and `tpe_*` caches only what it can stamp) were real peeks at a class in mid-load and stay.
- I13. Found by the same harness with per-key claims, 10 in 3000 runs: `NullPointerException` in `TypeRef.computeHashCode` (a null prefix) from `TypeSymbol.typeConstructor`. `ClassSymbol.thisType` stores its value, then its period, as plain fields, and returned the field after testing the period: on ARM another thread can see the period first and return null (the state map had this as "TODO verify on ARM"). Fix: read the value into a local before the period is trusted and recompute when it is null; the same in `ModuleClassSymbol.typeOfThis`. 0 in 3000 after.
- I14. Not a race, a leak (as I8): the per-key claimant of a thread was the value of a `ThreadLocal` of the `CompletionLock`, and refers to the lock and so to the `Global`; a thread that outlives the compiler kept every one. The stress harness ran out of a 4 GB heap after about 300 compilations with per-key claims. Fix: the claimants are in a map of the lock.
- I15. Not a race, but only reached past the oracle: every `-Yparallel-typer` mode failed a full compile of `src/reflect` with "illegal cyclic inheritance involving class Universe", from the serial phases after the pickler, since the type caches came off the lock (7ae25083a2). The in-progress markers treated any re-entry as a cycle; the serial protocol stamps the period first, so a re-entry is a cycle only if that period is valid for base classes at this phase. `Fields.transformInfo`, running inside a base type sequence computation, asks for the same type at uncurry, across a phase that changes base classes. Fix: the markers carry their period. All modes then match serial byte for byte on reflect. The oracle should compare classfiles of a full compile too.
- I16. Found with `foldSignatures` on the compiler, 1 in 4 oracle runs and 3 in 80 in one JVM: "illegal cyclic reference involving value NoParams", `variable localSymbols`. A `val`'s field, getter and setter had separate claims while their completers type one tree, so two threads could start on them at once and close a false wait cycle. Fix: they share a completion root. 0 in 150 after, on each of compiler, reflect and library.
- I17. Not a race, the same on one thread and on 8: pekko's javadsl compiles serially and failed under the flag with "found: _$16(in value errorConsumer), required: (some other)_$16" (4 errors). The member type of `c.apply` with `c: P[_ >: T]` skolemizes the wildcard when it is computed, and overload resolution types the argument twice, so the two typings agree only because `MethodSymbol.typeAsMemberOf` caches the result. That cache had been switched off in a concurrent compiler, since its four fields were not written atomically. One entry per symbol shared between threads would not do: another thread's `pre` could replace it between the two typings, a false error that depends on timing. Fix: the same cache per thread, in `Scratch`, cleared after each unit. Corpus `existential-overload`. Serial has a form of this too: the cache is off while a completer runs, so `def go(c: P[_ >: Throwable]): Int = m(c.apply)` compiles and the same method with its result type left out does not.
- I18. "not found: type Long" on valid programs: pekko, 3 of 40 oracle runs at 8 threads, always `Long`, and an immediate second lookup succeeded. A probe in the lookup showed the `scala` package's scope transiently without `Long`, and a probe on writes to that scope showed `TypeDiagnostics.TypeDiag`: to render a mismatch between `scala.Long` and `java.lang.Long` it renamed both symbols for the length of the message, and `name_=` moves a symbol to another bucket of its owner's scope. That happens for silent mismatches too, so a thread trying an overload could make `Long` vanish for all the others. Fix: under the flag the symbol is not renamed, only shown under another name on this thread (`Scratch.shownNames`, read by `nameString` and `decodedName`). Corpus `name-clash`: 299 of 300 in-JVM runs failed before the fix, 0 of 500 after, with also "type Long is not a member of package java.lang" and "Symbol 'type scala.Long' is missing from the classpath". Corpus `name-clash-neg` compares the rendered messages with serial.
- I19. False cycles in concurrent builds and on the compiler corpus: "illegal cyclic reference involving value log", `constructor FileReader`, `method logName`, or a fatal "wait-for cycle through CoarseRoot"; 5 of 40 multi-module builds. Every dump (a probe at the point where `PerKeyClaims` reports a cycle, with full stacks and the site of the coarse claim) had the same thread at the head: holding the coarse root and waiting for a source symbol's claim, while the holder of that claim waited for the coarse root. The coarse claim always came from `ImportInfo.qual`: `compiles` finds no enclosing class for an import at the top of a file, so `completionRoot` gave it the coarse root, and its completion types the qualifier under that root. Fix: an import claims itself. 0 cycle reports in 40 builds after, against 7 to 14 reports per 40 before. The eager imports that hid this (`-Dscalac.parallelTyper.eagerImports`) completed all imports in the class headers step, before any thread could hold a claim. Test: `ImportCompletionRootTest` checks the root.
- I20. "missing argument list for method log in class Global" at `import log.logreplay`, after I19 was fixed. At the failure the getter `log` had the info `ErrorType` with no error reported, so overload resolution preferred the inherited method `log`. A probe on `setInfo(ErrorType)` showed 500,000 writes per 40 builds from `safeNextOverriddenSymbol`, which pretends a symbol is erroneous while it looks for what the symbol overrides. The completion mark hides that from other threads, but a field's completion asks this of its getter, which is not marked. Fix: mark the symbol for that window. 0 failures in 80 builds, 1,200 compiles, against 5 per 40 before; no test, as it depends on timing.
- I21. The pickler's "bad type: <error>" crash, reflect 1 of 200 in one JVM, still 2 of 200 after I19 and I20. A probe in `putSymbol` showed `method apply in object ValDef` with `IS_ERROR`, in the object's scope. To decide whether to suppress a companion's synthetic `apply`, the namer asks if a member of that name is `LOCKED`, taking that to mean a user-defined `apply` whose completion it is inside (`scopePartiallyCompleted`). In a concurrent compiler the lock may be another thread's. The synthetic was then suppressed and, on this path, not unlinked. Fix: `HasMember` with `LOCKED` required counts only a member being completed on this thread. 0 of 1200 on reflect after. Test: `HasMemberTest`.
- L4. Not a race, a source incompatibility with no flag: `Global.typer` per thread was a `def`, so a macro's `import g.typer.infer.InferErrorGen._` no longer compiled ("stable identifier required"; `neg/t10700-message`, found only when `--neg` was added to the runs of this work). Fix: `object typer` is back, and the compiler's own callers use `typerHere`.
- O3, and a race it caused. `NamesDefaults` fills in a missing argument of an annotation either with the default expression, if the annotation class's parameter has `@defaultArg`, or with a call to the default getter. `typedValDef` adds `@defaultArg` when the class's own body is typed, so the form depends on whether that has happened yet. The two pickle differently, and `AnnotationInfo.argIsDefault` treats them alike. The serial compiler is order-dependent here (`DeterminismTest` fails with `class ann(cause: String = "")` and a user in another file, no parallel flag). The old pre-pass hid this, since it forced all annotations before any body; with headers only it became a race: `scala/MatchError.sig` differed in 1 of 3 library runs on the merge with the late-phases branch (0 of 20 on this branch alone), through `@throws`. Evidence: on one worker in source order with `throws.scala` first, the compiler before the fix gives that odd pickle byte for byte. Fix: under the flag, a class compiled in this run always gets the getter, so the parallel path is stable but differs from serial where serial has already typed the class. Classes from the classpath still get the default expression, so a batch and a separate compilation can differ, as they do in serial.
- O2. Which feature warning carries the explanation of its feature ("This can be achieved by adding the import clause...") was the first to arrive, so it varied from run to run once two threads could warn about one feature. The oracle sorts diagnostics by line and never saw it; the stress harness compares messages and differed in 200 of 200 runs of better-files. Fix: while workers run every feature warning is explained, and the buffering reporter drops the explanation from all but the first of each feature in the order in which it issues them.
- L1. Not a race, a false cycle: a full compile crashed under `-Yparallel-typer` with no late phase on the pool ("illegal cyclic inheritance involving class Source"). The per-thread in-progress lists of the stamped caches had the type alone. An info transformer running inside the computation of a type's base type sequence can ask for the same type at its own, earlier phase, which the serial protocol recomputes (its marker is under a stamp that is not valid there). Found independently by the signatures work as well. Fix: an entry carries its period, and the innermost entry for a type decides.
- L2. By argument, before anything ran on the pool: past the pickler a stamped cache's stale pair is a wrong answer. A new stamp with the previous value was harmless up to the pickler (no transformer changes base classes there); past erasure it is a type from another phase, and two threads at different phases (phase travel) write the same fields. Fix: value and period as one immutable object (`Stamped`) in a concurrent compiler. `rawInfoHere` read `infos` before `_validTo`, which could pair a covering `_validTo` with an older history; the concurrent path reads them the other way round, with a fence, and `info_=` no longer resets a node in place.
- L3. Lock order, by argument: the back end's `frontendLock` and the completion lock would be taken in both orders (a `LazyVar` initialiser under the first completes symbols; a class BType created inside a completion takes the first). Fix: under `-Yparallel-typer`, `frontendSynch` is the completion lock. `coreBTypes` and the other back end components have their lazy vals forced before the step, for the rule of "Lock order".
- O3. A dependence on unit order in the serial compiler, which the pool turns into a race. A call is rewritten to a specialized overload only if the info transform of the member's class has filled `overloads` by then. `new IntBox(i).get` compiles to `IntBox.get$mcI$sp` if the unit defining `IntBox` comes first and to `IntBox.get` if it comes last, on one thread, with no flag. On the pool: 1 in 200 and then 8 in 800 in-JVM runs of `late-phases` differed, in 5 to 13 callers at a time; 50 cold oracle runs did not show it, which is why both harnesses are used. Fix under the flag only (`overloadsOf`): the owner's info is forced before the read, which gives the answer of the serial compiler when the defining unit comes first. Serial is unchanged, so a program whose defining unit comes later compiles differently with and without the flag (better code with it). A single-threaded repro in `DeterminismTest` is being made on its own branch.
- I4. `Global.propCnt`, the depth of `withPropagateCyclicReferences`, was one counter for all threads: a thread's cyclic references propagated or were reported according to another thread's state, and a lost update fails `assert(propCnt >= 0)`. Found in the state map, not yet seen failing. Fix: per thread.
- I5. `BatchSourceFile.offsetToLine` stored its search result in the `lastLine` cache field and returned the field, which another thread could have changed in between: a wrong line number in a message. Found in the state map. Fix: return the local.
- O1. Signatures first changes which unit first completes a member, and with it whether `String` in a signature of `StringBuilder` and `WrappedString` is `Predef.String` or `java.lang.String`: `StringBuilder.scala` has `import scala.Predef.{...}`, which removes the root `Predef` import for the rest of the unit once it is completed (`registerImport`), so the answer depends on whether a signature was completed before or after that import, and in the serial compiler on which unit asked first. The types are equivalent (an alias and its expansion); the pickles differ in 2 of 805 files as text and in a third (`Predef.sig`) only in entry order. Completing package-level imports first in the pre-pass changed which signatures differ, not whether. Completing them in the namer, behind the flag only (`eagerImports`, tried and reverted), leaves the same three files different: the serial result is the order-dependent one, so no fix confined to the parallel path can match it. The proof that O1 is order and nothing else: with `-Dscalac.parallelTyper.order=source` (units in source order rather than largest first), `lazySignatures` (no pre-pass) and `noForceLazyVals`, the parallel path on one thread is byte-identical to serial on the library; any one of the three reorders completions and gives O1's three files. A fix would make the serial compiler order-independent here (complete package-level imports in the namer for everyone), a change to the stock compiler.

## State map accounting

The state map (`tools/state-map/worklist.md`, 656 fields reachable from the typer with no protection, extracted at fd95c5c1c1) by section. "Pre-pass" means the field is touched only by completers of source symbols, which run serially before the workers start; "lock" means under the completion lock; "confined" means per thread or per unit-thread.

- Ambient globals (20). `ph`/`per`: per thread. `currentUnit` (Run), `curFreshNameCreator`: per thread. `lastAccessCheckDetails`, `propCnt`: per thread. `globalPhase`, `curRun`, `curRunId`, `currentSettings`, `currentReporter`, `infoTransformers`, `nextFrom`, `keepPhaseStack`: written only by the driver between phases (serial), read by workers: safe. `phStack`: only with `-Ylog`, rejected. `lastSeenContext`, `lastSeenSourceFile`, `lastTreeToTyper`: diagnostics for crash messages only, a racy store of a reference is harmless (left; a crash report may name another thread's tree). `nodePrinters.lastPrinted*`, `typingStack.depth`: printing/`-Ytyper-debug` only. `SplainData.ImplicitErrors.stack`: `-Vimplicits` only, rejected (written only when the setting is on).
- Scratch on a shared object (3). `Infer.instantiate`: per thread. `BoxingErasure.boxPrimitives`: erasure (not reached by the typer). `JavaScannerConfiguration.kwOffset`: parser, serial.
- Context state reachable from completers (13): the pre-pass; contexts made by a worker stay on it; the shared root `startContext` is per thread.
- Per-instance state reached through a universe singleton (6). `global.typer` (`gen.mkAttributedCast`): not called by the typer phase (erasure and later). Singleton `Transformer`/`Traverser` objects (`currentOwner`): those listed belong to the async phase, the tree checker, `SymbolTracker` and `focuser` (range positions validation), none run by the workers. `wildcardExtrapolation`: per thread (the leftover in `Typers`). `TypeNode.nameInfo`: `typeDeconstruct` printing for debugging.
- Sentinels and cycle detection (12): the protocols above (`LOCKED`/`_completingThread`, stamped caches, `tpeCache`). `infos`/`_rawflags`: see TODO. `_lockingTrace`: `-Vcyclic` only, rejected. `relativeInfoCache`: value written before its stamps; the residual is a stale value with a new stamp, which within the typer is the same value (left, noted).
- Black-hole protocols (81). Classfile parser, unpickler `Scan`, `PickleBuffer`, `ReusableDataReader`, TASTy reader, `SymbolLoaders.parentsLevel/pendingLoadActions`: run only inside a loader's completion, so under the lock. Scope fields: `createHash` publishes a filled table; `enter` prepends; `elemsCache` race noted (TODO). `Pickler` state: the pickler phase is serial. `ClassSymbol.childSet`: written by the namer for children in the same file (pre-pass) and by Java permitted subclasses (loader, lock).
- Non-atomic read-modify-write (42). `perRunCaches` registry: synchronised. `Reporter` counts: the buffering reporter counts under its monitor. `Run.unitc`: `advanceUnit` synchronised. `PerRunReporting.reportedFeature`: under its monitor. `_openMacros`: per thread. `rawatt` on symbols: attachments of source symbols are written by the namer (pre-pass) and by the typer of their own unit; shared classpath symbols' attachments are written by loaders (lock); left. `Names.nc`: under the name lock. Profiling/statistics/tracing fields: rejected options. `SymbolPairs.Cursor`, `Importers`, `TableDef`, `Printers`: per-instance, confined.
- Stamp and value in separate fields (12): `TypeRef.parents`, `tpeCache` under the protocols above; `tyconCache` value then run id, idempotent (interned type); `thisTypeCache`/`typeOfThisCache` value then stamp, idempotent (interned), a stale stamp-before-value on a weak memory model would return null: TODO verify on ARM or holder; `SubComponent.ownPhaseCache`: set before the parallel phase (`typerPhase` is asked for by the driver); `mtpe*`: no cache when concurrent; `relativeInfoCache`: above.
- Unsynchronised mutable collections (74): the typer's maps above (confined or synchronised); per-unit `transformed`, `synthetics`, `toCheck`: written by the unit's own thread and by completers of its symbols, i.e. the pre-pass; `Run.symSource`: concurrent map; `symData`, `compiledFiles`: pickler and parser (serial); `DocComments`: scaladoc, rejected; `ZipArchive` entries: read-only after the archive is opened (lazily, by loaders under the lock); `BaseTypeSeq.pending`: lock; `ClassInfoType.refs`: lock; `Names` tables: name lock for writes, reads of a bucket can miss a name being entered, which then is entered twice? No: entering is under the lock and looks again (`newTermName` holds the lock for lookup-or-enter).
- Lazy-val monitors that force completion (302): `definitions`, `runDefinitions` and the cake's private lazies are forced before the workers start; per-instance ones that complete symbols take the completion lock first (`LazyAnnotationInfo`); `OverloadedArgProto`'s lazies are per call. Not yet verified by a sweep that nothing was missed: the watchdog's thread dump is the detector (no hang seen so far).
- Other unprotected fields (91): not yet gone through one by one (TODO), beyond `BatchSourceFile.lastLine` (fixed) and the ones named above.

## Stage 2a: the cheap scaling steps

`src/library`, warm JVM (`tools/parallel-typer/bench.sh library <out> 6 5`, i.e. 6 warm-up and 5 measured compilations per configuration), medians. The machine was loaded throughout (load average 11-13, other agents' benchmarks running: no quiet machine was available overnight), so the 8-thread configuration, which ran last and contends with them and with the efficiency cores (6 performance and 12 efficiency cores), is not reported: it ran out of heap (I8, fixed since) and before that its timings were dominated by the other load. Each variant is the same build with the earlier steps' system properties.

| variant | serial ms | one thread ms | 2 ms | 4 ms | 6 ms | bodies at 1 / 2 / 4 / 6 threads ms | lock held at 1 thread ms (acquisitions) | lock waited at 4 threads ms |
|---|---:|---:|---:|---:|---:|---|---:|---:|
| stage 1 (before B, earlier run) | 2211 | 2169 | 1601 | 1545 | | 1500 / 937 / 878 / - | 488 (98k) | 1596 |
| B.3 + B.4 (caches and locals locked) | 1874 | 1682 | 1419 | 1393 | 2487 | 1118 / 861 / 723 / 1797 | 362 (75k) | 1231 |
| B.2-B.4 (locals locked) | 2009 | 1736 | 1355 | 1222 | 1955 | 1211 / 769 / 631 / 1388 | 344 (18k) | 771 |
| B.1-B.4 | 1953 | 1759 | 1254 | 1148 | 2085 | 1187 / 651 / 524 / 605 | 48 (8.5k) | 25 |

Reading: with all four steps the lock is no longer a bound (held 48 ms of a 1.2 s one-thread body phase; 25 ms waited in total at 4 threads), and the bodies go 2.3x faster on 4 threads and 2.0x on 6 (605 ms against 1187 ms; the 6-thread total is noisy because the serial parts ran beside other load). The one-thread path is at or below serial (1759 against 1953) in every variant: the signatures pre-pass and the bodies together take what the serial typer takes, and the rest of the run is the same. The next bounds, in order:

1. The serial parts: the signatures pre-pass (about 300 ms), then parser, namer, superaccessors, extmethods and pickler (about 800 ms together of a 1.2 s run on 4 threads, by the x-ray recording of the stage 1 build): Amdahl's law caps the whole run near 2x however fast the bodies get. Parallel signatures with claims per root (2b.4) and a unit per task for the parser, superaccessors, extmethods and pickler are the next steps.
2. The efficiency cores: 6 threads are no faster than 4 for the bodies' share of the work and the totals vary, which fits work landing on efficiency cores beside other load; measure on a quiet machine with `-Yparallel-typer-threads` at the performance-core count.
3. Uniques: one synchronized interning table (`SynchronizedPerRunUniques`) on every type construction; not yet measured, Scala 3 striped it.

### The x-ray after stage 2a (3c72e64ddf)

`tools/parallel-typer/xray/run.sh library "1 2 4 6 8"` (one warm JVM, 12 warm-up compilations, then one recorded compilation per configuration; the machine is an Apple M5 Pro, 6 "super" and 12 "performance" cores, and was loaded by other agents' benchmarks). The recording is `timeline.data-3c72e64ddf.js` in the scratchpad (copy it beside `tools/parallel-typer/xray/index.html` as `timeline.data.js` to view).

| config | total ms | speed-up | signatures pre-pass ms | other serial ms | bodies ms | tail ms | lock held ms | lock waited ms |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| serial | 2009 | 1.00 | | 296 (setup, parser, namer, post-typer phases) | 1741 (typer) | | | |
| 1 thread | 1759 | 1.14 | 302 | 326 | 1160 | 0 | 397 | 0 |
| 2 threads | 1234 | 1.62 | 296 | 330 | 632 | 0 | 409 | 4 |
| 4 threads | 1089 | 1.84 | 285 | 357 | 456 | 108 | 397 | 19 |
| 6 threads | 1146 | 1.74 | 322 | 357 | 512 | 127 | 486 | 73 |
| 8 threads | 1093 | 1.83 | 292 | 350 | 508 | 134 | 488 | 182 |

(The x-ray counts every acquisition through its hook, 31k, where the lock's own counter counts the outermost ones; its "held" includes the holds of local completions that the recorder sees before they skip the lock.)

The next bound, in order of size at 8 threads:

1. The serial parts: the signatures pre-pass (≈300 ms, the largest single serial step), then setup ≈65, parser ≈65, superaccessors ≈60, pickler ≈37, namer ≈34, extmethods ≈27, the annotation sub-pass ≈22: ≈640 ms of a 1090 ms run. Amdahl caps the run near 2.3x at 8 threads. Parallel signatures (2b.4) is where the pre-pass would go: it works on the library and the compiler but not on reflect (I9); the phases after the typer and the parser, a unit per task, are smaller, simpler steps.
2. The tail: ≈110-134 ms at 4-8 threads. Largest first by source length is a poor proxy for cost: `SeqView.scala` (250 ms of bodies, the costliest unit at 8 threads, through `SeqView.Sorted`'s inference) starts at 371 ms because it is not among the largest files; `ArrayOps.scala` 285 ms, `Vector.scala` 211 ms. A cost estimate from the pre-pass (time spent in the unit's signatures) or a finer task (a top-level class rather than a unit) would shorten it.
3. The bodies themselves scale 2.5x to 4 threads and not beyond (456 → 508 ms at 8): with the lock waited 182 ms in total at 8 threads it is not the lock; the remaining suspects are the uniques' single monitor (`SynchronizedPerRunUniques`, every type construction), allocation and GC, and the loaded machine.

### After striping the uniques (0a4b168aa9)

`timeline.data-28e6e634fc.js` in the scratchpad. Bodies at 4 / 6 / 8 threads: 408 / 327 / 316 ms (from 456 / 512 / 508); totals 1013 / 902 / 892 ms, speed-up 1.83 / 2.06 / 2.08x against a serial run of 1853 ms; the one-thread run 1916 ms (0.97x, one sample; earlier runs had it at or below serial). The bound is now the serial steps (≈570 ms: pre-pass 278, parser 64, superaccessors 60, setup 53, pickler 38, extmethods 28, namer 27, annotations 23) and the tail (≈120 ms: `SeqView.scala`, `ArrayOps.scala`, `Vector.scala` take 210-285 ms of bodies each, a third of the bodies' wall time, whatever the order). Ordering the units by parsed tree size rather than source length was tried (tail 105 against 118 ms in one sample each, and counting the trees costs ≈40 ms serially): not kept. A finer task than a unit (its top-level classes) is what would shorten the tail.

## Build wiring for scala-sharing (2b.2)

Vendored at 33614c2 (`src/sharing`, `src/sharing-macros`, README beside them). The `sharingMacros` project (sources `src/sharing-macros`) is compiled by STARR with STARR's library and reflect as provided dependencies, and `reflect` and `compiler` depend on it at compile time only (`compile-internal->compile`); `reflect` compiles `src/sharing` into scala-reflect.jar, its lint warnings silenced (`-Wconf:src=src/sharing/.*:s`). `Publish.java` compiles with reflect's `-source 1.8 -target 1.8` because javac compiles against the running JDK's class library (21), where `VarHandle` exists; a JDK 8 runtime would fail on the classes that use it (only reached in a concurrent universe), as it would on the fences this branch already uses in `Symbols`. `dist/mkQuick` works; partest passes; `restarrFull` then `dist/mkQuick` with the re-STARRed compiler builds every project (de26c4b717, after the relocation of I12; before it, scalap failed).

The completion lock is now scala-sharing's `CoarseLock` behind the `CompletionLock` facade, and `-Dscalac.parallelTyper.claims=perKey` switches it to `PerKeyClaims`: a source symbol's completion claims the symbol (a module and its module class one root), everything else claims the coarse root (`CompletionLock.CoarseRoot`: the classpath, macro expansion and the sites still under the lock), a claimant per thread that cannot abort, and a wait-for cycle reported as the `CyclicReference` a serial nesting would have met. The library's operations (`Sharing.claim` with `Holds` evidence, `Once`, `Stamped`, `Guard`) are not used yet: the facade calls the registries directly, which is the simpler shape for the few sites the compiler has (decision 8).

## Claims per root and parallel signatures (2b.3, 2b.4)

`-Dscalac.parallelTyper.claims=perKey` (a source symbol's completion claims the symbol, everything else the coarse root) passes the oracle on case-classes, stress-shared, library and compiler (5 runs each at 8 threads), the same results as the coarse lock: with the signatures done serially, the workers only ever claim the coarse root (classpath, the shared caches still under it) and lock-free local completions, so per-key claims change nothing in the bodies. Their use is parallel signatures.

Parallel signatures (written as the experiment `-Dscalac.parallelTyper.parallelSignatures`; the default since 6 Nov, see below; `=unit` still selects unit roots): the class headers serially, then the members' signatures a unit per task on the pool, each source symbol's completion claiming its unit (its source file: the Namer's completers of a unit share its namer contexts, whose implicit cache has an in-progress marker and whose context mode a typing flips, its per-unit maps and its classes' scopes, so a root per symbol would let two threads type in one context), then the annotations serially. Results:

- library (3 runs), compiler (5), stress-shared (5), annotations (5), case-classes (3): agree with the one-thread run (and the one-thread run with serial, O1 apart). On the library at 8 threads, cold: headers 551 ms serially, member signatures 631 ms on 8 threads, against 1.3-2.0 s for the serial pre-pass in the same cold setting.
- reflect: every run differs, 48-62 "illegal cyclic reference" errors a run (`FindTypeCollector`, `AnyValClass`, `DeprecatedAttr`, `TreeCopier`...): the cake's traits are units that refer to each other's members everywhere, so two threads each holding one unit's claim and wanting the other's close a wait-for cycle that a serial nesting would not meet (a lock order, not a dependency: Scala 3's I54 and §5.7 at a coarser root). Answered as a `CyclicReference`, it is wrong.

So unit roots are too coarse. The second attempt (67f050d335, the default value of the property; `=unit` keeps the first): symbol roots, with what the completers of one unit share made safe for two writers. A member completion already gets a fresh child context (`createNamer` makes a new scope context per member); what it shares is up the chain, and three things there were written: a context's implicits cache with its in-progress marker (now per thread: `Contexts.implicitsInProgress`, value before run id), the unit's `transformed` and `synthetics` maps (concurrent), and the class scopes that default getters and synthetic companions are entered into (writes under the scope's monitor). And lending at a provisional info: a wait or claim on a symbol that would close a wait-for cycle, when the symbol's owner has published a provisional info, reads that info as a serial nesting would (`CompletionLock.WaitCycle`); without one it is the serial cycle error. One more lock-order deadlock surfaced on the way (the `definitions` object's monitor, held by a lazy val forced from a completion while it waited for a claim, the others blocked on the monitor; the claims cannot see a monitor): the lazy vals are now forced after the class headers and before the members run in parallel.

Results, 3 runs each at 8 threads: reflect, library, compiler and case-classes agree with the one-thread run, with no cyclic reference errors at all (reflect had 48-62 with unit roots), and their one-thread runs agree with serial (O1 apart). stress-shared fails 1 run in 3: I11, which per-key claims make more frequent (also without parallel signatures). Cold, on reflect: class headers 525 ms serially, member signatures 646 ms on 8 threads. Warm, library, with parallel parsing too (`timeline.data-f7da445081-parsig.js`): 838 ms at 8 threads against 892 ms with serial signatures, the gain eaten by the tails of three pool steps instead of one (≈270 ms of "tail" in the x-ray's terms). Not the default: I11, and 50 clean runs per corpus are the bar.

## Parallel signatures as the default (6 Nov)

With I11 fixed, member signatures on the pool are the default under `-Yparallel-typer`, and with them a claim per completion root (`PerKeyClaims`) instead of the one lock. `-Dscalac.parallelTyper.serialSignatures` goes back to the serial pre-pass and the coarse lock (`claims=perKey` still selects per-key claims with it).

Evidence. Oracle at 8 threads with `checkScratchOwner`, pickles compared: 5 runs on each of the 15 corpora (library, reflect, compiler, annotations, annotations-neg, case-classes, stress-shared, better-files, re2s, scalap, induction, map-seq-inference, patmat-exhaust-huge, vector, re2j), no run differs from the one-thread run, and the library differs from serial by O1 only; 3 more runs on library, reflect and compiler at the commit that flips the default. The in-JVM harness (`stress.sh`, diagnostics and crashes only): 0 of 500 on each of the four small corpora, 0 of 100 on each of the eight benchmark corpora, 0 of 40 on library, reflect and compiler. stress-shared, cold, 500 runs at 8 threads: 0 differ with the coarse lock and 0 with per-key claims. The bar of 50 cold oracle runs per corpus was not run: dropped on Jason's call, in favour of integrating and measuring; what is left should show up there. JUnit `scala.tools.nsc.typechecker.*`, `scala.tools.nsc.symtab.*`, `scala.reflect.internal.*`: 308 pass.

Speed: see "Only the class headers serial" below. A first table here, taken at load average 34-41, showed the run at 4 threads going from 0.66 to 0.35 of serial with members on the pool. That was noise: in one sitting at load 20 the two modes are equal at 4 threads.

Class headers on the pool (`-Dscalac.parallelTyper.parallelHeaders`, off). The lazy vals of `definitions` must be forced before anything runs on two threads (the monitor against claim deadlock) and after the headers of what they name (I3), so headers can only go to the pool when no source is in package `scala`, where the lazy vals are forced first. On such corpora (the four small ones, better-files, re2s, re2j, vector) 100-200 in-JVM runs each were clean, with one `NullPointerException` in 200 runs of annotations-neg (a null receiver of `asSeenFrom`) that did not recur in 5500 more; `TypeSymbol.typeConstructor` returning its cache field after another thread's `info_=` cleared it is the likely cause and is fixed, but the link is not shown. When the package test was wrong and the headers of reflect and compiler ran on the pool, 16 of 20 and 11 of 20 runs failed with "not found: type Namer", "not found: value rootMirror", "value echo is not a member of Reporter". Whether that is the lazy vals forced too early or a lookup in a class whose header another thread is completing was not separated. Recommendation: headers stay serial. The order that keeps results equal to the serial pre-pass is units in source order, each unit's imports and classes in tree order; on the pool that order is lost between units, and the cake is where it matters.

Annotations on the pool (`-Dscalac.parallelTyper.parallelAnnotations`, off). A lazy annotation is forced under the claim of the symbol it was first attached to (`LazyAnnotationInfo.claimWith`, set in `Symbol.setAnnotations`) rather than the coarse root, the claim still taken before the lazy val's monitor. 0 of 100 in-JVM runs on the small corpora, 0 of 20 on library, reflect and compiler; the oracle was not run with it, so pickles are unchecked. On the library the step goes from 49-107 ms to 9-17 ms at 8 threads under load. Worth turning on after an oracle pass.

Superseded by the next section: the pool step for members and the one for annotations both go if the folded mode becomes the default.

## Only the class headers serial (`foldSignatures`, 6 Nov)

The default (it was the experiment `foldSignatures`; `-Dscalac.parallelTyper.poolSignatures` keeps the previous default for comparison): the class headers are completed serially and nothing else. A member's signature is completed by the first thread that asks for it, under the symbol's claim, and its annotations by the body typing of its own unit, so the typer has one pool step and one tail. The direction is Jason's (the serial pre-pass does too much); the profile behind it is in the notes of the session that asked: of the pre-pass, 38% is typing right-hand sides for inferred types, 32% `methodSig`, 11% class headers.

Serial time left in the typer phase, `src/library`, warm, 6 warm-up and 5 measured compilations, one sitting at load average 19-23, medians (ms):

| mode | serial run | total 4 / 8 threads | serial signature step 4 / 8 | bodies, or members and bodies, 4 / 8 | min total 4 / 8 |
|---|---:|---:|---:|---:|---:|
| serial pre-pass, coarse lock | 2525 | 1282 / 1118 | 372 / 358 | 490 / 398 | 1123 / 910 |
| members on the pool (`poolSignatures`) | 2240 | 1281 / 1054 | 285 / 218 | 665 / 502 | 1099 / 951 |
| headers only (the default) | 2617 | 1138 / 1018 | 82 / 74 | 716 / 592 | 1121 / 974 |

The serial step falls by a factor of about 4.5. The total gains less: the work moves into the bodies, and the medians improve by 11% at 4 threads and 9% at 8 while the minimums do not improve at all. The serial runs alone vary by 17% across the sitting, so these totals cannot separate the modes; the serial step can be read, the totals need a quiet machine or perflab. In this table the pool step gains nothing over the serial pre-pass at 4 threads, which is the argument for folding it rather than keeping it as a stage.

### 1. Foreign completions: what they write

A thread typing unit A that needs `B.m` completes it itself, under `m`'s claim. There is no affinity to B's thread, so `f -> B.m -> A.g` chains that are not cycles do not wait on each other. `cycles-neg` has such chains: no false error on them in 80 cold runs. What such a completion writes of unit B:

- `transformed`, `synthetics`: concurrent maps, keys private to the member or the companion.
- `toCheck`: appended under the deque's monitor, and the drain now takes it too (it did not). Residual: a check added after B's thread has finished draining would be dropped by `clearAndShrink`. B's thread waits for each member it reaches, so this needs a symbol B's typer never reaches; none is known, none was seen.
- Fresh names. `freshNameCreatorFor` was per class or method, so numbers in a class's val and var right-hand sides followed typing order, and they reach classfiles (`eta$0$9$1`, a parameter's `x2`, the local class `$typecreator2$1`) but not pickles, so the oracle cannot see them. Under the flag the creator is now per member. Names are local to one right-hand side, so restarting at 1 cannot collide before lambdalift gives them a unit-wide suffix.
- `currentUnit` stays the demanding thread's, as in a serial nested completion. Messages take their position from the context, and the buffering reporter sorts them.
- Contexts: a member gets a fresh child; the implicit markers up the chain are per thread.
- `addSynthetics` iterating a scope while a foreign completion enters a companion or default getter is covered only by the scope publication argument; not tested specifically.

### 2. Determinism

Measured by a new comparison of full compiles (the oracle stops at the pickler), each mode at 1 and 8 threads, repeated, on scalap (197 classes), re2s (44), reflect (1537) and compiler (2491):

- A program that compiles: the same classfile bytes in every run and at every thread count, in both modes. Pickles equal the one-thread run on every corpus. Four corpora is a small sample for "every run", and library has not been through the classfile comparison.
- Against serial: identical on re2s and reflect; 1 class differs on scalap and 7 on compiler, all in fresh numbers. Read individually: parameter local names (`x2` against `x1`) in two anonfuns, `eta$` locals, and `FormatInterpolator$$typecreator2$1` becoming `$typecreator1$2`, which renames 4 local classes. The bodies of those 4 were not compared line by line. Making the per-member creator unconditional would remove the difference by changing the serial compiler's numbers. O1 is unchanged.
- A program with a genuine cycle (`corpus/cycles-neg`): rejected in every run (0 of 80 cold runs exit 0), but which symbol is blamed and the wording vary, because the reporting party of a wait-for cycle is chosen by thread identity, and a nesting on one thread blames wherever that thread started. Runs differing from the one-thread run: in one JVM, 35 of 100 folded and 56 of 100 with members on the pool, against 0 of 100 with the serial pre-pass; cold, 2 of 40 and 0 of 40. Some say "illegal cyclic reference involving method inh" where serial says "recursive method inh needs result type". So `cycles-neg` can fail the oracle in the default and with `poolSignatures` until this is settled (it passed the last 3-run oracle, and 2 of 40 runs differed before that).
- A lent provisional info is read only when a wait would close a cycle, as a serial nesting would read it. No pickle differed, which is evidence and not an argument: a lent info in a valid program could in principle change an inferred type.

Options for stable cycle messages: choose the reporter by source position (reduces it, does not end it, because of nesting on one thread); or, on any cycle error, type the run again serially in a fresh `Global`, which the buffering reporter makes possible and which costs only builds that fail. Recommendation: the second, later; it is not needed to measure.

### 3. Annotations

They can move to the owning unit's task, and in this mode they have: `typedTemplate` (classes and modules), `typedValDef`, `typedDefDef` and `typedTypeDef` already force them, and the driver's sub-pass is gone. A thread in another unit that reads `sym.annotations` forces the thunk under the claim of the symbol it was first attached to (`claimWith`), before the lazy val's monitor. Tested by the annotation corpora in the oracle, 3 runs each at the tip.

### 4. Class headers

They stay serial, as in Scala 3's PR 84: 63-99 ms warm on the library, against the 74-82 ms step above. On the pool they fail on cake-style code: "not found: type Namer", "not found: value rootMirror" in 16 of 20 runs of reflect and 11 of 20 of compiler. The cause was not separated (lazy vals forced before the headers they name, I3, or a lookup in a class whose header another thread is completing), since the experiment excludes such units once its package test works. The corpora where it passes save too little to justify a second code path, so `parallelHeaders` is removed, and `parallelAnnotations` with it, since annotations now belong to the unit's task.

## Lazy-signatures mode (A.4)

`-Dscalac.parallelTyper.lazySignatures` (no pre-pass: completers run where the bodies reach them, under the lock, on any thread) passes the oracle on library, reflect, compiler, case-classes and stress-shared (3 runs each at 8 threads, same results as signatures first, O1 included). It is not the default for two reasons. Correctness: the cross-unit completer hazard (a completer of unit B typing B's trees and writing B's maps on thread A while B's thread works on the same unit outside any completion) is real in principle; it did not show because B's thread meets most of that state through `sym.initialize`, which waits, but nothing guarantees it (e.g. `unit.transformed` read by `typedImport`, `addSynthetics` iterating a scope a completer enters into). Speed: on the compiler at 8 threads the bodies take 13.5 s with the lock held 11.0 s (against about 2 s with the pre-pass): inferred result types are typed inside their completers, under the lock, so the parallel phase is mostly serial.

## Measurements

Oracle (cold processes, `tools/parallel-typer/oracle.sh`): `src/library` (574 units, 805 pickles) 20 runs at 8 threads, all agree with the one-thread run, and differ from serial only by O1; `src/reflect` (213 pickles, 7 warnings) 5 runs and `src/compiler` (480 pickles, 4 warnings) 5 runs at 8 threads agree with serial byte for byte, diagnostics included; the annotation corpora 10 runs at 4 threads each agree. No crash or hang in these runs once I1-I3 were fixed.

Speed, `src/library` to the pickler, warm JVM (`tools/parallel-typer/Bench.scala`: 6 warm-up and 5 measured compilations per configuration in one JVM, `-Xss64m -Xmx6g`, M-series laptop with other work running, so a first look, not a benchmark), medians:

| config | total ms | signatures pre-pass ms | bodies ms | lock held ms | lock acquisitions | lock waited ms (sum over threads) |
|---|---:|---:|---:|---:|---:|---:|
| serial | 2211 | | | | | |
| one thread | 2169 | 384 | 1500 | 488 | 98k | 0 |
| 2 threads | 1601 | 401 | 937 | 430 | 102k | 374 |
| 4 threads | 1545 | 366 | 878 | 569 | 104k | 1596 |
| 8 threads | 2616 (min 1602) | 470 | 1800 | 1510 | 105k | 6529 |

Reading: the parallel path costs nothing on one thread (2169 against 2211), the bodies speed up 1.6-1.7x on 2-4 threads, and 8 threads are slower than 4. Two serial fractions bound it: the pre-pass (about a fifth of the typer phase, because inferred result types type their bodies there) and the time under the lock during the bodies (about a third of the one-thread bodies time). The acquisitions by call site (`-Dscalac.parallelTyper.lockSites`, 4 threads): `Symbol.info` slow path 35k (completions, mostly of local symbols, which need no lock as they are confined to their unit), `TypeSymbol.tpe_*` 24k, `cookJavaRawInfo` 24k, `TypeRef.baseTypeSeq` 18k, `SingleType.underlying` 9k, `CompoundType.baseClasses` 7k, `CompoundType.baseTypeSeq` 3k, annotations 2k, `expansiveRefs` 2k, macro bindings 1k. A first version took the lock in `Symbol.exists` unconditionally: 395k acquisitions, bodies 9.4 s on 4 threads; it now takes it only on the slow path. At 8 threads the time held grows (1510 ms against 430-570), a sign of convoying: the holders are descheduled while waiters spin and park.

Final oracle at 8277581c87 (default mode, 8 threads, `-Dscalac.debug.checkScratchOwner=true`): annotations, annotations-neg and case-classes 200/200 clean; better-files, re2s, scalap, induction, map-seq-inference, patmat-exhaust-huge, vector and re2j 50/50 clean; stress-shared 7/200 differ, all I11 (self-type errors on `java.util.ArrayList` and `TreeSet` as well as `HashMap`). No scratch owner violation anywhere. library and compiler had 200/200 at a9e662c9a3, reflect 116/116 (the rest an oracle artefact).

## The serial baseline under `-Ystop-after` (found after the overnight run)

Every measurement above that stops at typer or pickler had a pessimised serial compiler, and to a lesser degree a pessimised parallel one. `isValid` and `isValidForBaseClasses` ask `nextFrom(pid)` for the first info transformer at or after a phase. A run that stops before the first of them installs none, `nextFrom` answers with the head of the list (pid `NoPhase.id`, "changes base classes"), and both checks read that as a transformer at phase 0 and fail. A completer runs at the namer's phase and a body at the typer's, so each switch between the two invalidated the base type sequence and base classes of every class it touched: `src/library` through typer computed 195863 compound base type sequences for 10475 types (`scala.Any`'s 20724 times); with uncurry in the run the count is 11084, one per type. The signatures pre-pass removes most of the switches, which is all of the "one thread at or below serial" result and of the perf agent's -35% on the `scala` corpus. It is not a property of completion order in a full compile.

Fixed by treating the head as the end of the list unless the period is at `NoPhase` itself. Through typer, cold, total allocation: library 1520 to 967 MB, compiler 2175 to 1291 MB, pickles unchanged; the one-thread parallel run is now slightly above serial (1008 and 1377 MB). Library, warm, after the fix (medians, loaded laptop): serial 1381 ms, one thread 1472 ms, 4 threads 813 ms, 8 threads 777 ms (1.78x); bodies 987 / 349 / 276 ms at 1 / 4 / 8 threads, signatures about 240-260 ms. The earlier speed-up figures (2.08x, 2.28x) were against the pessimised serial run. The perflab campaigns that used `-Ystop-after:typer` predate the fix and should be rerun before their serial-path and one-worker percentages are quoted. The fix is independent of the parallel typer and could go to 2.13.x by itself.

## Parallel parser and the index (namer's enter step)

Both behind system properties, under `-Yparallel-typer`: `scalac.parallelTyper.parallelParser` (a unit per task on the pool) and, with it, `scalac.parallelTyper.loadPackages` (the packages the units name are loaded while they are parsed). The index itself stays serial; the reasons and the numbers are below.

### What the parser shares, and what was done about each

- Scanner, parser, tree builder, XML parser and builder, brace patches, `smartParse`'s buffered errors: per parser instance, one per unit. The keyword tables (`Scanners.kwArray`/`kwOffset`) are filled when the `SyntaxAnalyzer` is constructed, on the main thread; `JavaScannerConfiguration` is a nested object (a volatile module field, initialised under the analyzer's monitor by whichever parser first scans Java; its initialiser only makes names).
- Fresh names: `unit.fresh`, per unit. `currentRun.currentUnit` and the global fresh name creator are per thread under the flag, and the pool sets the unit.
- The name table. It was the bound: every `newTermName`, `toTypeName` and `toTermName` took the one name lock, and a parser makes a name per identifier, so the parser took as long on eight threads as on one (about 80 ms on the library). Names are only added, at the head of a bucket, with final fields: the lookup now reads the bucket without the lock (an acquire fence after the head, pairing with a release fence before a new name is linked, so that the name's characters are in the table for a thread that sees it), and only a miss takes the lock and looks again. The typer's threads use the same path.
- Positions: `atPos` has an assigner per thread already. Position validation (`-Yrangepos`) runs after the phase, on the main thread, as before.
- The reporter. Only errors reach it during parsing; the pool's buffering reporter can now issue in the order of the units and, within a unit, of arrival (`parallelPhase(..., unitOrder = true)`), which is exactly what the serial parser prints (the default order, by file path and position, is for the typer, where a unit's thread reports on other units). The oracle compares the order of the diagnostics, not only their sorted set, for the two corpora that are about it.
- Warnings (deprecations, `-Xsource:3` migration, lints). `runReporting` suspends them per source until the typer has seen the unit's `@nowarn`s; the map of suspended messages is kept in the order of the units while they are parsed (`suspendInOrderOf`), because a run that ends before the typer reports them in that order (`runFinished`).
- One order dependence in the scanner: at the end of an XML literal with a token in hand, `Scanner.resume` reports "possible missing '}' in XML block" unless `reporter.hasErrors`, which counts the errors of the units before this one, and the choice changes the token stream. A unit that reaches that test while other units are being parsed is abandoned (`RetryInUnitOrder`) and parsed again on the driver thread after the workers, in the order of the units, with its buffered diagnostics dropped, its suspended warnings cleared and its fresh names reset; the reporter then answers from the errors of the units before it. Corpus `parser-neg` has two such units.
- Symbols. The parsers read a few members of `definitions`, and a lazy val there is initialised under the monitor of `definitions`: one whose initialiser completes a symbol would take the completion lock inside the monitor, the reverse of a completion that reads a lazy val. Those of the root classes (`AnyTpe`, `ObjectClass`, `UnitTpe` and the primitive types of the Java parser, `TupleClass` for `mkTuple`) are forced before the workers start, which changes nothing a serial run sees, since `definitions.init` has completed those classes. The ones that can complete a class of the class path or the source path are read under the completion lock where they are used, so that the lock comes first: `Predef` for a Java class literal, `String` for an enum's `valueOf`, the `native`/`transient`/`volatile`/`strictfp` annotations, `Nil` for an empty XML attribute. Forcing those up front instead would complete classes that a serial run leaves alone.
- Not run in parallel: `-Ymemberpos` (prints as it goes), the REPL's incomplete-input handler, a single unit, and whatever the parallel typer rejects (profiler, statistics, scaladoc: `parallelPhasesSupported`).
- Harmless: `lastSeenSourceFile` (a crash report may name another thread's file), `Run.advanceUnit` (synchronised), `informUnitStarting` in arrival order.

### The index: what the enter step shares

The namer phase enters only top-level symbols (members are entered when their class is completed). Per unit it:

- finds or creates the package symbols (`createPackageSymbol`: a lookup in the owner's scope, then create and enter: a check-then-act on a scope that every unit of the package shares);
- for each top-level class and object looks the name up in the package scope and either overwrites the symbol a loader put there (`updatePosFlags`: `reset`, new flags and position, `invalidateCaches` over the old info, and `setInfo NoType` on the companion, which may belong to another unit) or creates and enters a new one (`enterInScope`, with the double definition check: the error names the earlier symbol and is reported at the later, and the earlier is unlinked);
- links companions through the same scope (`companionSymbolOf`, `ensureCompanionObject` for case classes and constructor defaults, `validateCompanionDefs`, whose error goes to whichever of two files comes second);
- writes `currentRun.symSource` and the class's associated file;
- allocates symbol ids in entry order. `Symbol.isLess` breaks ties between classes whose base type sequences have the same length by id, and that order decides the order of the parents of a lub, so the relative ids of top-level classes can reach a pickle;
- for Java sources, records the unit's classes (`patmat.javaClassesByUnit`) and types the `permits` clause of a sealed class, which completes other symbols at the namer's phase;
- creates the unit's root context, whose chain ends in the creating thread's `startContext` (per thread under the flag, I1's family): contexts made on a pool thread would outlive it and be shared by the typer's threads.

Package objects are opened by the next phase, in the order in which their packages were loaded or met (`deferredOpen`), which the above also decides.

A fully parallel enter would have to make each of those deterministic: ids in unit order (blocks of ids per unit), scope entry order, double definitions and split companions resolved as if in unit order, root contexts made on the driver. The alternative, per-unit work in parallel and a serial commit in source order, has nothing to put in the parallel half: the symbols need their owner and the scope lookup first, and what follows (completers, type parameters, companion trees) is small.

### Where the namer's time goes, and the decision

Timers in the namer (temporary, not committed), warm, serial compiler, three samples each:

| corpus | namer phase ms | of which package loading ms (loads) | root contexts ms | entering proper ms |
|---|---:|---:|---:|---:|
| library (574 units) | 37.5 / 40.4 / 42.9 | 30.7 / 33.6 / 35.8 (28) | 1.2 | about 6-7 |
| compiler | 44.5 / 53.0 / 60.0 | 35.4 / 43.6 / 43.7 (38) | 3.6-9.7 | about 6-9 |
| reflect | 26.9 / 33.8 / 57.8 | 20.0 / 22.2 / 23.2 (13) | 3.8-30.9 | about 3-4 |

About four fifths of the phase is one expression, `sym.info.decls` in `enterPackage`: the first time a unit names a package, its `PackageLoader` lists the class path and source path entries and enters a symbol and a loader for every class, about 1 ms a package. The enter step proper, everything in the list above, is 3-9 ms (on the library: lookups, creation and overwrite 2.5 ms, companions 2.8, completers 0.8, the rest of `enterClassDef`/`enterModuleDef` about 3).

Decision: the index stays serial. Eight threads could take 6-9 ms to perhaps 2-3, half a percent of a 780 ms run, less the start and join of a pool step, and would put ids, scope order and double definitions at risk for it. Scala 3's PR 84 kept indexing serial as well.

What pays is the package loading, and it needs no concurrency in the symbol table: the loads depend on nothing in the other units, and the driver thread is idle while the workers parse. With `loadPackages` the driver walks the units in their order as they finish parsing and makes the lookups that `enterPackage` will make (`namerFactory.loadPackagesOf`: the same `info.decls.lookup` chain per `PackageDef`, at the namer's phase, creating nothing; a package that does not exist yet is left to the namer). The loads are the same, in the same order, with the same `deferredOpen` order, as in the serial namer, which then finds every package complete; symbol ids of the loaded classes keep their relative order. For the walk to make progress the parser's tasks start in the order of the units, not largest first. A unit that is to be parsed again (`RetryInUnitOrder`) stops the walk, and the namer loads the rest as it always did. The loader runs under the completion lock on the driver; the parsers take that lock only at the rare sites above.

### Checks and numbers

Oracle at 8 threads with both properties and `-Dscalac.debug.checkScratchOwner=true`, 50 runs each (before the fence of I13): library 50/50 agree with the one-thread run (O1 against serial, as before), reflect and compiler 50/50 identical to serial, parser-neg, parser-warn, annotations, annotations-neg and case-classes 50/50 (order of diagnostics included for the two parser corpora). stress-shared: I11 at its usual rate (7 in 200 with the properties, 4 in 200 without, on the same build) and I13. junit `scala.tools.nsc.typechecker.*`, `scala.reflect.internal.*`, `nsc.parser`, `nsc.reporters`, `nsc.symtab` (381) pass.

- I13. stress-shared: `NullPointerException` in `Type.computeMemberType` from `ImplicitInfo.tpe` during an implicit search in a body, the same stack twice: `sym.tpeHK`, i.e. `Symbol.info`, was null. Seen 2 times in 250 runs with the two properties and 0 in 200 without, which is not enough to say that they make it likelier (they act long before, so only through timing). Mechanism, from reading: `info_=` gives a symbol its first `TypeHistory` (plain fields, stored in the plain field `infos`) and then a valid `_validTo`; a thread on `info`'s fast path, without the lock, that sees the valid `_validTo` can read the history before its `_info` on ARM. `rawInfoHere`'s assert that `infos` is not null passes, and it returns null. Fix: a store-store fence in `info_=` before `_validTo` is written, pairing with the load-load fence that `info` already has after its read of `_validTo` (`isCompletingElsewhere`). After it: 70 runs of stress-shared with 4 I11 failures and no crash, reflect 3 runs clean; at about 1 in 125 that shows nothing yet, so this is a fix by argument until a longer soak. It is the suspect I11's entry lists ("a field of a symbol made on the loading thread read without a happens-before edge"), and may be part of I11 as well.

Parser phase on the library, warm, wall time (the machine at load average 15-17 from the sibling sessions, so only the shape is to be trusted): one worker 93 ms, four 36 ms, eight 28 ms, after the name table change; before it 79 / 87 / 89 ms. With `loadPackages` the driver loaded the packages of all 574 units in 34-50 ms beside the parse. Whole-run medians at that load were too noisy to rank the variants (8 threads: 1023 ms without, 909 ms with both, one sample of nine runs each); `bench.sh` at 1, 4 and 8 threads on a quiet machine is still owed.

Recommendation, pending the benchmark and a soak of stress-shared for I13: make the parallel parser the default under `-Yparallel-typer` (it is exact in its diagnostics and takes about 50-60 ms off the serial path at 8 threads), and `loadPackages` with it (another 30-40 ms, by the driver's own timing); keep the index serial.

## Parallel post-typer phases (superaccessors, extmethods, pickler)

`-Dscalac.parallelTyper.parallelPostTyper` (under `-Yparallel-typer`): each of the three phases runs a unit per task on the pool, instead of serially over all units.

### Steps, not fused

Decision: each phase is its own pool step over all units (`=steps`, the default value). The alternatives are behind the same property for measurement only: `=fused` (superaccessors, extmethods and pickler of a unit back to back on one worker) and `=fusedTransforms` (the two transforms fused, the pickler its own step). Fusing with the typer's body step is not on the table: superaccessors reads `sym.alias` of parameter accessors, which `finishComputeParamAlias` sets after the last unit is typed; it looks members up in classes of other units whose typers would still be entering case class synthetics; and the run loop decides after the typer, on the whole run's errors, whether the later phases run at all.

Why steps:

- A step keeps the serial invariant "phase N has finished on every unit before phase N+1 starts on any". What is left to argue is only what two units do to each other within one phase, which is the order dependence the serial compiler already has (it runs the units in command-line order).
- The pickler of unit A writes an external reference to a symbol of unit B as its name and owner. Superaccessors and extmethods rename symbols: `private[C]` members of a class get an expanded name (`expandName`, superaccessors' `ClassDef` case), and so do private members selected in an `@inline` method and the private parameter accessors of a value class (`makeNotPrivate`). Such a symbol can be named from another unit through a type: B has `class C { private[C] val x: Foo = ...; val y: x.T = ... }`, A has `class D extends C { val z = y }`, and A's pickle refers to `x` as `C$$x` or `x` depending on whether B's superaccessors has run. Fused, that is a race on the bytes of a pickle. The corpus `post-typer` has this case.
- The run loop stops after a phase with errors. Fused, extmethods and the pickler would have run on some units (and written `.sig` files) before another unit's superaccessors reported an error.
- `-Ystop-after`, `-Yskip`, `-Vprint:<phase>`, `-Yshow`, plugin phases that sit between two of the three: the phase list and the run loop are untouched, a phase is parallel or serial on its own, so they all behave as before. A fused group would have to detect each of these and fall back (the experiment only fuses phases that are adjacent in the phase list).
- The cost of a step is its tail, set by its slowest unit. For the typer that is 200-285 ms (three units); for these phases the largest unit is a few ms (the whole of superaccessors on the library is about 55 ms serially over 574 units), so three joins cost little. Measured below.

### Phase travel

Each worker sets its own phase register to the step's phase. There is no info transformer between the typer and refchecks, so `info` at any of the three phases is the typer's `TypeHistory` entry: a read at a later phase only advances `_validTo` (the same value from every thread) and transforms nothing. Scopes are not versioned by phase: an entry made by superaccessors is seen by a reader at any phase, which is why the inter-phase order matters and steps keep it. Flags are masked by phase (`notPRIVATE` takes effect after superaccessors, per reader, from its own register). The stamped type caches (`baseTypeSeq`, `baseClasses`) are revalidated for the new period under their existing protocols; no base class changes.

### Shared state, per phase

Superaccessors:

- Accessor symbols (`super$m`, `protected$m`, `protected$m_=`) are entered into the scope of `clazz`, which is always a class enclosing the tree (`Super`'s and `This`'s class; `hostForAccessorOf` walks the enclosing classes), and the definition is stored in `accDefs(clazz)`, which only has the templates open in this transformer. So a unit enters only into its own classes' scopes, in its own tree order: no ordering problem between units, and one writer per scope. Readers in other units (member lookups below) are covered by the scope publication argument of the typer.
- `private[C]` expansion: `decls.unlink(s); s.expandName(C); decls.enter(s)` on the unit's own class. `unlink` bypasses the entry without touching its `tail`/`next`, so a reader standing on it goes on; `name_=` finds nothing left to rehash; `enter` prepends. A cached element list computed across the unlink is keyed by the old head and dropped by the enter.
- `makeNotPrivate` (private member selected in an `@inline` method): atomic flags; `expandName` then rehashes the symbol's entry in place, rewriting the `tail` of an entry that another thread may be standing on, which diverts that reader into the other bucket and makes it miss the rest of its own (a lookup of an unrelated name in the same class answers "no member"). Fix: in a concurrent compiler `Scope.rehash` leaves the old entry as it is and links a fresh entry into the new bucket and in the old one's place in the element list.
- Reads of other units' classes: `overridingSymbol` in intermediate classes (scala/bug#4989), `hasClassOverride`, `info.member(name).exists`, the ancestors' decls for `-Xlint:private-shadow`, `isSubClass`/`<:<` for protected accessors, Java-defined owners (classfile completion and `cookJavaRawInfo`, under the lock). These are lookups by name of members that are not being renamed, except a lookup of a `private[C]` member by its unexpanded name, whose answer in the serial compiler already depends on whether the defining unit comes first. Not made deterministic (it would take renaming in a pass of its own); not known to matter: such a member cannot be accessed from another unit, and an override with a qualified-private modifier is an error in refchecks.
- Name freshness: accessor names are derived (`super$`, `protected$`), not fresh; fresh names made by the local typer come from the unit's creator (the current unit is per thread).
- Per transformer, so per unit: `accDefs`, `typers`, `validCurrentOwner`, the local typer and its contexts (`rootContextPostTyper`, as the typer's workers build them), the tree copier. `restrictionError` and the lint warning go to the buffering reporter and `PerRunReporting`.

Extmethods:

- An extension method symbol is entered into the companion's scope (`newExtensionMethodSymbol`). The companion of a value class is in the class's unit (a synthesised one is made by the namer in that unit), so again one writer per scope, in tree order. `extensionDefs` is a field of the unit's transformer.
- `makeNotPrivate` on the primary constructor and the parameter accessors (the rename above). `removeAnnotation(TailrecClass)` and `setAnnotations` on the unit's own symbols.
- The forwarder and the extension method bodies are retyped by the unit's local typer; retyping stops at subtrees that have a type, so it does not redo lookups in other units.
- Nothing here uses `markUsed`; `extensionMethod` (the lookup by erasure) is not called before erasure.

Pickler:

- `Run.symData` (a `HashMap`): read and written under its monitor in a concurrent compiler. `shouldPickle`'s test-then-store is not atomic across units, which is fine: a symbol has one defining unit (companions in two files are an error in the typer), so two units never pickle the same root. The back end only does `get` and `-=`, no iteration, so insertion order is not observable.
- The reusable `_index` and `_entries` of `Pickler`: per thread.
- `-Ypickle-write`: a directory is written by the workers as they go (`DirEntryWriter` is the back end's concurrent writer). A jar's entry order is its write order, so for a jar (or a virtual file) the workers keep each unit's files and the phase writes them in unit order after the join. The `sigWriter` lazy val is forced by the first worker to get there; its initialiser completes nothing.
- `-Ypickle-java`: Java units are pickled like the others; their signatures were completed in the pre-pass.
- What a `Pickle` forces: `info` of the symbols local to its root (complete after the typer; one that is still lazy is the unit's own and completes under the usual rules), `annotations` of those (a thunk that is still lazy is forced under the completion lock, lock before monitor), `children`, `companionSymbol` (a lookup in the owner's scope, which no one writes now). Of an external symbol it reads the name, the owner chain and `isModuleClass`, and completes nothing. `newTermName` for string constants takes the name lock. The traversers and the buffer are per `Pickle`.
- Entry order in a pickle is the order of `decls`, each written by one unit before the pickler step starts: byte-identical by construction.

Diagnostics go through the buffering reporter of the pool step, as for the typer. Units are queued largest first; nothing observable depends on the order.

### Findings

- The fused experiment fails as predicted. `=fused` on the `post-typer` corpus, 20 runs at 8 threads: 9 runs differ from the one-thread run and the one-thread run differs from serial, always in `Q<n>.sig` (the subclass of `Qualified`, which refers to `x` or `pt$Qualified$$x`); one run crashed in the pickler (`NoSuchElementException: key not found: pt$Qualified$$x` in `writeRef`: the name changed between the pickle's two passes). `=fusedTransforms` passed 20 runs on that corpus; it was not taken further.
- P1, contention, not a race: the pickler step did not scale at first (48 ms serial, 53 ms on 8 threads; the units' wall times summed to 284 ms with 175 ms blocked on a monitor). JFR's monitor events put all of it on the name table's lock in `TypeName.toTermName`, called by `Pickle.isRootSym` for every owner of every symbol a pickle meets. Fixes: `isRootSym` compares the two names' position and length in the table (a type name and its term name share their characters) after the owner, and `TypeName.toTermName` reads its bucket without the lock first and looks again under it if it does not see the entry. After: 11 ms on 8 threads.
- Not ours: `scalac -Ystop-after:pickler` (no `-Yparallel-typer`) throws a `NullPointerException` in superaccessors on a super call to a default method of a Java interface (`super[JGreeter].greet(..)`): `transformSuperSelect` asks erasure's `accessibleOwnerOrParentDefiningMember`, which runs `exitingErasure`, and with no erasure phase in the run that is `enteringPhase(NoPhase.next)`, and `NoPhase.next` is null. The corpus avoids it. A candidate for 2.13.x by itself.

### Measurements

Oracle, `=steps`, 8 threads, `-Dscalac.debug.checkScratchOwner=true`, 50 runs each, at the final commit of this work (and the same result on the commit before the name-lock fix): post-typer, annotations, annotations-neg, case-classes, reflect, compiler 50/50 agree with serial byte for byte; library 50/50 agree with the one-thread run, which differs from serial in the three O1 pickles (`Predef`, `WrappedString`, `StringBuilder`) as before; stress-shared 1/50 and 2/50 in the two batches, all the I11 self-type error (7/200 and 10/200 before). post-typer also 5/5 with `-Ypickle-java`. `-Ypickle-write` to a jar, 5 runs: the same entries in the same order with the same bytes as serial. A full compile of the corpus (no `-Ystop-after`) with the switch on gives the same 371 class files as serial. junit `scala.tools.nsc.typechecker.*`, `scala.reflect.internal.*`, `scala.tools.nsc.symtab.*`: 308 pass.

X-ray, `src/library`, warm (12 warm-up compilations, one recorded per configuration; `timeline.data-off.js` and `timeline.data-on.js` in the scratchpad). The machine had a load average of 17-30 throughout from the other sessions' work, so totals from different JVMs are not comparable; the per-phase wall times are:

| phase | serial compiler | 1 thread, off / on | 4 threads, off / on | 8 threads, off / on |
|---|---:|---:|---:|---:|
| superaccessors | 53-73 | 67 / 78 | 57 / 30 | 62 / 17 |
| extmethods | 31-42 | 35 / 36 | 33 / 17 | 37 / 13 |
| pickler | 42-59 | 52 / 45 | 47 / 19 | 48 / 11 |
| the three | 126-174 | 154 / 159 | 137 / 66 | 147 / 41 |

So about 105 ms of an 8-thread run of about 1100 ms on this loaded machine, and no cost on one thread beyond starting a worker three times. `bench.sh library` (8 warm-up, 9 measured, same JVM, medians, same load): 8 threads 1291 → 1171 ms and, repeated at the end of the same JVM, 1050 → 937 ms with the switch (serial 2402 and 2242 ms); 4 threads 1191 → 1185 ms and one thread 1843 → 2229 ms (minima 1790 and 1803), both inside the noise of this session. These need repeating on a quiet machine before they are quoted.

The tail question: at 8 threads the largest unit of each step is 5-10 ms (`Vector.scala` and `FunctionWrappers.scala` in superaccessors, `FunctionWrappers.scala` and `ArrayOps.scala` in extmethods, `FunctionWrappers.scala` in the pickler), against 200-285 ms for the typer's. One recorded run each: steps 17 + 13 + 11 = 41 ms, `fusedTransforms` 19 + 10 = 29 ms, `fused` 29 ms. Fusing would save about 10 ms of 1100 and gives up byte-identical pickles, so steps stay.

What serial time remains at 8 threads with the switch on (x-ray, ms): signatures pre-pass 361, tail of the bodies 161, parser 98, setup 89, namer 49, annotation sub-pass 24, superaccessors 17, extmethods 13, pickler 11, lazy-val forcing 6, packageobjects 2. The post-typer phases are no longer among the bounds; the pre-pass, the tail and the parser are (the siblings' work).

### Recommendation

Make it the default under `-Yparallel-typer` (keep the property as an off switch, `=false`): it has the same oracle record as the typer's own path on every corpus, it changes nothing in the phase list or the run loop's decisions, and the state it adds to the argument is small (one writer per class scope; `symData`; the pickler's reusable buffers; `Scope.rehash`). Before flipping it: a rerun of the timings on a quiet machine, and the two residues below looked at once.

Residues: a lookup, from another unit, of a `private[C]` member by its unexpanded name while its unit's superaccessors renames it (order-dependent in the serial compiler too; not known to be reachable in a program that compiles); the remaining `toTermName` calls in the pickler (module class names of external references, `companionModule0`) now take the lock only when the lock-free read misses. The fused modes should be deleted with the experiment, they exist to show the race.

## Parallel late phases (refchecks to jvm)

`-Dscalac.parallelTyper.parallelLatePhases` (under `-Yparallel-typer`; no value or `all`: every phase below that is known to work; otherwise a comma-separated list of phase names, for bisection) runs each named phase a unit per task on the pool. The goal is a full compile to class files with every unit-wise phase on the pool, byte-identical to the serial compile.

### Decisions

Steps, as for the post-typer phases: each phase is a pool step over all units, so a phase has finished on every unit before the next starts on any. What is left to argue is only what two units do to each other within one phase. That matters more here than before the pickler, because a good deal of the serial compiler's correctness rests on "phase N is done everywhere": scopes are not versioned by phase, so lifted methods, bridges and mixed-in members appear in a class's `decls` for every reader at every phase from the moment they are entered. Fusing is a later measured experiment and nothing in the design depends on it.

Units start in their command-line order and diagnostics are issued in unit order (`parallelPhase(..., unitOrder = true)`). So the one-thread path is the serial compiler apart from the typer's own changes, and a difference between them points at the pool rather than at an order effect.

The lazy info transformers are the central problem, and they get the existing protocol: a transform of a symbol's info runs under the completion lock, with the symbol marked as completing, so that another thread waits rather than reading a half-advanced history. No transformer is assumed to be pure. They enter members, make new classes, set flags and fill per-run maps, and the lock covers all of it, nested transforms of other symbols included.

Pre-pass, considered and not adopted as the first step: forcing every source symbol's info at each phase on the driver before its step would remove the concurrency from the transforms and fix their order, at the cost of a serial step per phase and of transforming symbols a serial run leaves alone (which changes when specialization's members appear). Kept in reserve for a phase whose transforms turn out to be order-dependent.

### What is common to every phase

- `Symbol.rawInfoHere`/`transformInfos`. `_validTo` is advanced stepwise "to enable reads from same symbol during info-transform", and `infos` is appended to. Unlocked, two threads run a transformer twice on one symbol (two specializations of one class), and a reader sees an intermediate info. Plan: above. Readers on the fast path need the history's fields published before `this.infos` and `_validTo`.
- Stamped caches. Up to the pickler no transformer changes base classes, so the residual "a new stamp with the previous value" was harmless. It no longer is: past erasure (and specialize, mixin, explicitouter's successors) a stale pair is a wrong answer, and phase travel means two threads at different phases write the same fields. Value and stamp must be read as one: an immutable holder in a concurrent compiler for `TypeRef.parents/baseTypeSeq/relativeInfo`, `CompoundType.baseTypeSeq/baseClasses`, `SingleType.underlying`, `TypeSymbol.tpeCache`, `typeOfThis`.
- `global.typer`, one `object` with one `Context`: used by `gen.mkCast`-style paths and by erasure, uncurry, lambdalift and tailcalls through `typedPos`. Per thread.
- Lazy vals of the phase objects whose initialisers complete symbols (`Erasure.interceptedMethods`, `LambdaLift.refCreateMethod`, `SpecializeTypes.specializableTypes`, the duplicator's analyzer): the lock order rule, forced before the first step.
- `perRunCaches` maps at phase level: synchronised, or moved to the per-unit transformer when they are in fact per unit.
- Absolute symbol ids now vary from run to run, and `Symbol.hashCode` is the id. Iteration over a hash collection of symbols that reaches a class file is a difference; the oracle is the detector, and the fix is at the site (a linked collection, or a sort).
- Cross-unit reads of a scope that another unit's tree transform is entering into: the scope publication argument of the typer, with the order-independence argued per phase below.

### Per phase

| phase | info transformer | phase-level mutable state | cross-unit writes | fresh names | expected difficulty |
|---|---|---|---|---|---|
| refchecks | modules become methods (flags) | `separatelyCompiledScalaSuperclass` | bridge for a structural member and `makeNotPrivate`, own classes | none at phase level | low |
| patmat | none | translators are per match, `javaClassesByUnit` read-only | none | translator's `ctr`, per instance | low (exhaustivity results must not depend on ids) |
| uncurry | yes, repeated and by-name types | none | own classes | `unit.fresh` | low |
| fields | yes, adds trait fields and setters | `moduleOrLazyVarOf` | `makeNotPrivate`, setters on the class being transformed; a subclass's transform reads the trait's | none | medium |
| tailcalls | none | per-transformer `perRunCaches` maps | none | none | low |
| specialize | yes, creates classes and members, unlinks from owners | `specializedClass`, `overloads`, `info`, `typeEnv`, `anyrefSpecCache`, `wasSpecializedForTypeVars`, `concreteSpecMethods`; `duplicator`'s `oldClassOwner`, `newClassOwner`, `envSubstitution`, `invalidSyms` | owner's scope, from the info transform; `makeNotPrivate` from a call site | none | high |
| explicitouter | yes, outer accessors | none | `makeNotPrivate` on a member of an enclosing class, own unit | none | low |
| erasure | yes | `requiredDirectInterfaces`, `boxingErasure.boxPrimitives`; bridges in `root.info.decls` | own class's scope, read by subclasses' bridges in other units (filtered by `isBridge`) | none | medium |
| posterasure | yes, derived value types | none | none | none | low |
| lambdalift | yes, adds free variable parameters | per-transformer | moves locals into their enclosing class, own unit; `iface.info.decls` for a getter in a trait, own unit | `unit.fresh` | low |
| constructors | none | `guardedCtorStats`, `ctorParams` (read for a specialized subclass, same unit); `currentUnit.fresh` | unlinks omittable members, own class | per unit | medium |
| flatten | yes, lifts classes into the package's scope | `liftedDefs` | package scope, from the info transform, read by every unit | none | medium: `rehash` and `unlink` on a shared scope |
| mixin | yes, none on classes but `publicizeTraitMethods` and `mixinTraitMembers` change a trait of another unit from the subclass's transform | `treatedClassInfos`, `_bitmapInfo`, `_slowPathFor` | a trait's symbols and scope, from any subclass | none | high |
| cleanup | none | `entryPoints` (order of a set, read by the back end only for the inliner's entry points) | none | `unit.fresh`; `symbolsStoredAsStatic` per transformer | low |
| delambdafy | none | per-transformer | enters lifted methods into its own class | none | low |
| jvm | none | `frontendSynch` around each class; `BTypes` caches, `CoreBTypes`, `bCodeAsmCommon`, `symData -=` | none | none | see below |

`jvm` is not a transform but a walk of the trees that reads symbols through phase travel, and it already serialises the front-end part by taking `frontendLock` around every top-level class, while `-Ybackend-parallelism` covers writing only. Its class-level state (`PlainClassBuilder`) is per instance, so the work is the `BTypes` caches and the order in which `generatedClassHandler` receives units. To be attempted last; recorded here as serial with a concrete obstacle if it does not fit.

### Where it ended

Every phase in the table is on the pool with `parallelLatePhases`, jvm included. What is still serial or coarse, and why:

- A class's mixed-in members and `publicizeTraitMethods` run under a claim per class (`Mixin.mixinClaim`), since they change a trait of another unit from a subclass's transform. It is a separate key from the completion roots: on a shared key a real wait-for cycle appeared.
- Info transforms are published optimistically, one transformer at a time, with a compare-and-set on the history (`transformInfosClaimed`). A per-symbol claim was tried first and removed: its holder could need the coarse root for an unrelated lagging class while the coarse root's holder wanted the claim. A transformer that is not local to the symbol (`transformIfLocal` returns null) takes the coarse root for that step only.
- Specialize takes the coarse root only for a class with real specialization work (`freeOfSpecialization`).
- A jar output keeps `UnitOrderGeneratedClassHandler`, so the entries are in unit order. Writing to a directory or a virtual directory is done on the workers.

Writing class files behind the pool on a separate thread was measured and reverted: 1145-1245 ms for the library against about 1100, and 981 ms with writes discarded. The cost on this machine is file I/O under JumpCloud and ESET, which is why compile timings are taken in memory.

Races are in the race log (L1-L4, I19-I21) and "Two races in the info protocol". Three more, each a pair of plain fields read as one:

- `ThisType(sym)` is `sym.tpe_*` past erasure, so a stale value and period pair for `thisType` gave a wrong class info and `typedTemplate`'s parent check at erasure wiped the history (a missing class signature in 1-2% of runs). Fix: `thisType` and `typeOfThis` in immutable holders.
- `Type.normalize` read its cache field twice around another thread's invalidation: a null. Fix: one read.
- `cookJavaRawInfo` marked a symbol under the coarse root over a completion under the symbol's own root, then restored the saved mark, leaving a finished thread's mark behind and hanging later readers. Fix: a set of marks with a compare-and-set, and symbols forced before the lock is taken.

Results at the end of this work, in one JVM per corpus with a fresh `Global` per compile (`stress.sh`), 8 threads, every pool switch on: reflect 0 of 1200, library 0 of 200, compiler 0 of 40, pekko 0 of 100, scalap 0 of 100, re2s 0 of 100. Slow cores (`taskpolicy -b`), reflect, before I19 to I21: 0 of 800. Process per run with class files byte-compared to the one-thread run: pekko 0 of 20, scalap 0 of 50, re2s 0 of 50; the one-thread run differs from serial in 5 pekko files and 1 scalap file, recorded by the oracle as order effects and not examined here. Multi-module builds: 0 failures in 80.

Full compile to class files in memory, every step on the pool, one warm JVM per corpus (OpenJDK 21, ParallelGC), one recorded compile after 12 warm-up compiles of all configurations, load average about 4:

```
            serial     1      4      8 threads
library       3485   3872   1093    735 ms    4.74x at 8
compiler      6152   6552   1810   1249 ms    4.92x at 8
```

The last recording before these fixes, on the coordinating branch at load 9-13, had 3.96x and 3.92x at 8 threads. The load differs, so the two are not a controlled before and after.

What still takes the coarse root after the typer, as total hold per compile at 8 threads, with `-Dscalac.parallelTyper.lockSites` (a stack walk per acquisition, so the compile itself is slower, 1517 ms and 1774 ms):

| site | compiler | pekko |
|---|---:|---:|
| jvm, `frontendSynch` | 53 ms in 3776 | 139 ms in 5956 |
| info transforms | 50 ms in 11787 | 45 ms in 15088 |
| macro expansion | 58 ms in 1854 | 26 ms in 908 |
| `expansiveRefs` | 1 ms | 10 ms |
| `cookJavaRawInfo`, loaders | 10 ms | 6 ms |

These are holds, an upper bound on what a thread can queue for behind them: about 170 ms on the compiler and 225 ms on pekko, across all phases. Waits are not recorded with per-key claims. Specialize no longer shows as a site of its own.

`frontendSynch` was the largest of these on pekko, nearly all of it creating a `ClassBType` from a symbol (65 ms of holds, with 8 ms in `LazyWithLock`). A class's own monitor was held across the computation of its info, and the coarse root around that, since two threads could otherwise block on each other's monitors. Now nobody waits: a thread that wants an unfinished info computes it as well (`ClassBType.pendingInfo`), and the first result is kept. The computation only reads symbols, so the results are equal. With `-opt` the old path stays, as the byte code repository is not safe for two threads. That leaves `frontendSynch` at 10 ms on pekko. Stress 0 of 1060 on seven corpora; class files against the one-thread run: compiler 0 of 10, pekko 0 of 10, scalap 0 of 30, re2s 0 of 30.

The gain in wall time is small and not separated from noise. Four alternating pairs at 8 threads, load 4-7, medians in ms:

| corpus | locked | off the lock | pairs (off minus locked) |
|---|---:|---:|---|
| pekko | 1531 | 1505 | -23, -72, +45, -52 |
| compiler | 1332 | 1301 | +68, -27, -34, -50 |

So holds were not where threads were losing most of their time, and the coarse root is no longer a limit worth chasing here: the remaining sites total about 90 ms of holds on pekko. "What limits each corpus now" above is the better list.

### Order of attack

1. Oracle for class files, and a corpus. Serial against serial must be clean first.
2. The common machinery, with no phase on the pool: the info transform protocol, holders for the stamped caches, `global.typer`. Each of these must leave the existing oracle clean and be free for the serial compiler.
3. The phases with no or trivial transformers: tailcalls, patmat, uncurry, explicitouter, posterasure, lambdalift, delambdafy, cleanup, refchecks.
4. The ones with phase-level state: constructors, fields, erasure, flatten.
5. Mixin, specialize.
6. jvm.

## Profiles in the x-ray

The x-ray says when each thread worked and when it waited, but not on what. `tools/parallel-typer/xray/profile.sh <corpus> [threads]` adds async-profiler data, and the page's *Where the CPU went* section shows a flame graph, reversible, and top frames for any window, step, unit, span or completion. The README there has the format.

Spans. Each x-ray span is also two nested async-profiler spans, the step and then the unit, opened in `ParallelTyperTimeline.begin` and `end`, which every x-ray span goes through. This is the nesting perflab's agent gives an unmodified compiler (phase, source path, definitions), so `jfrconv --spans` and its span panel work the same way. `ProfilerSpans.java` calls `one.profiler.Span` through `static final` method handles, so the compiler needs no jar, and a recording off costs nothing beyond the existing test of `current`. Tags are precomputed: steps are literals or a phase's name, and a unit's is `CompilationUnit.spanLabel`, its path, a field set when the unit is created. The annotation span's label used to be built from a count and is now the literal `all symbols`. Completions are a third level, tagged with the symbol's name and recorded only if a sample fell in them (`endIfProfiled`), which is off unless `-Dscalac.parallelTyper.xray.completionSpans=true`.

The name table. Taking a name's string must allocate nothing for completion spans. retronym/scala#204's `string` store has that property, but I did not use it. Merging it conflicts in 16 places in `Names.scala` with the spike's lock-free reads, since it moves storage behind a `NameStore`, and its `string` store would need that thread-safety work again; it is also unbootstrapped, and it would change the default path the serial compiler takes. `Name` already has what is needed: `cachedString`, which `toString` returns when set. With the property above, `newTermName` fills it for every new name, at one test on the path that enters a new name. A check on the quick build: `toString` returns the same `String` object on term and type names with the property on, and a new one with it off. If #204 lands, this test goes away.

CPU and wall clock are separate recordings. On macOS `event=cpu` is the wall clock engine sampling running threads, which async-profiler refuses to run beside `wall`. `event=itimer` can, but its signal arrives at the next kernel trap: 14% of its samples landed in `mach_msg2_trap`, under the x-ray's own `getCurrentThreadCpuTime`, and 10% in a bare `_sigtramp`. With `event=cpu` those disappear, and the samples per thread agree with `ThreadMXBean`.

Repeats. One compilation gives about 90 samples in the library's class headers at 1 ms, too few to read. A profile is therefore 10 compilations under one recording, and each sample is projected onto the compilation of median length: from the span around it to the span of the same step and unit, proportionally, on that span's thread. Filters by step, unit and span are exact, and a window means the same work across compilations. Instantaneous overlap between threads is only true of the compilation shown. This gives 557 samples in the library's class headers and 1468 in the compiler's, and 11.6k and 24k in the bodies. The data is larger than planned, 13 MB for both corpora, against the few MB aimed for. The page stays responsive (a filter takes about 100 ms) and `REPEATS` trades it off.

Overhead, 4 threads, medians of alternating compilations in one warm JVM with completion spans on, with other sessions' load on the machine (load average 15-30):

```
                   off       timeline   + CPU profile   + wall profile
library, 12 rounds  806.5 ms  792.3 ms   853.4 ms        772.7 ms
                              -1.8%      +5.8%           -4.2%
compiler, 8 rounds  1118.0 ms 1130.2 ms  1156.9 ms       1130.4 ms
                              +1.1%      +3.5%           +1.1%
```

Only the CPU profile's cost shows above the noise. The serial compiler with nothing enabled: on the library, its pickles and diagnostics are identical to the base commit's, and the one-thread run's three pickles that differ from serial (O1) are the same as there. Six alternating forks of 8 compilations each had medians of 1.7 to 4.5 s on either build, so that comparison would not resolve a small cost and I report no number from it. What it adds is a static read and a null test per completion, the one test above per new name, and a field per unit.

The oracle at this commit, 5 parallel runs at 8 threads: compiler clean; on the library no parallel run differs from the one-thread run, which differs from serial by O1 alone, as at the base commit.

First things the profiles show: the compiler's largest serial stretch, class headers, spends 23% of its CPU samples opening and statting files for classfiles (`__open`, `stat64`, under `ClassfileParser`'s input). In the bodies, `SymbolTable$PerThreadScratches.get`, the per-thread state the spike added, is the top self frame on the library (3.1%) and second on the compiler (3.5%), with `SymbolTable.scratch` beside it.

A hang seen once, on the library at 4 threads under the abandoned `itimer` configuration and not again in about 150 compilations: a signatures worker had exited while another spun in `awaitMarker` on a `_completingThread` still naming it, with the others waiting on that worker's claim. `markCompleting` and `unmarkCompleting` save and restore the field non-atomically, which could leave such a marker if two threads' marks interleave. That is a hypothesis; I did not reproduce it.

## Why it stopped scaling at 2.4x, and what was done (6 Nov)

A full compile to class files with every step on the pool (`parallelParser`, `loadPackages`, `parallelPostTyper`, `parallelLatePhases`) levelled off: library 4430 ms serially, 1991 / 1820 / 1847 ms at 4 / 6 / 8 threads; compiler 7015 ms, 3047 / 2908 / 2838 ms. Thread CPU summed over the spans of a step grew with the thread count in every step (typer bodies of the compiler corpus 2305 ms serially, 3824 ms at 8).

How it was taken apart:

- A control. Eight independent serial compilations side by side in one JVM, each in its own `Global`, cost 34% more CPU each than one alone (20% at 4 and 6). That much is the machine (6 fast cores and 12 slower ones, shared caches) and no change to the compiler removes it. The parallel compiler at 8 threads cost 116% more, so the rest was ours.
- HotSpot. JDK 25, which has the fix for the secondary supers cache that makes interface casts scale badly, gives the same times as JDK 21 (1421 against 1431 ms at 8 threads). The cake's casts are not the limit.
- Profiles. `event=cpu` with the patched async-profiler over several warm compilations, serial against 8 threads, compared by frame.

What the profiles showed, largest first, and what became of each:

1. The hand-off of the coarse claim. 21% of all samples at 8 threads were `Unsafe.park` and `unpark`, 95% of them from `Symbol.transformInfosUnderLock`: every info transform of a late phase takes the coarse root. The time running under the claim was 3%; the cost was that a waiter parked on the claim's latch at once, every waiter was woken at the release, one won and the rest parked again. `PerKeyClaims.acquire` now looks again up to 2000 times before it parks (`-Dscalac.parallelTyper.claimSpins`; 20000 measures the same). Library at 8 threads 1828 to about 1500 ms. The transforms are still serialised by the coarse root: this is the late-phases work's to change.
2. The same claim in the front end. A completer runs at the namer's phase and the body at the typer's, so the first read of every symbol's info in the typer had `_validTo` behind the period, and went to `transformInfosUnderLock` to move it on with no transformer to run: 6.8% of front-end CPU at 8 threads. `_validTo` is now advanced with a compare-and-set when no transformer lies in between (`Symbol.advanceValidTo`). A plain write would not do: it could move `_validTo` back behind a transform another thread had just finished, which would then run twice.
3. Fields of shared objects written on the hottest paths. `Trees.nodeCount` at every tree made, `analyzer.lastTreeToTyper` at every `typed`, `Global.lastSeenContext` at every context: each a field of the universe or the analyzer, which every thread reads constantly, so the line it is on moved from core to core. They are per thread now, in `Scratch`. The sign was `SymbolTable.NoSymbol`, a lazy val whose bitmap is such a neighbour: 342-424 samples against 112 serially, 125 after. The parser gained most (CPU summed over its workers at 8 threads from about 270 to 165 ms).
4. The lookup of the thread's phase. `Symbol.needsFlatClasses` (behind every `name` and `owner`), `Scope.lookupEntry` and `Symbol.flags` asked for the phase to learn that it is before flatten and has the initial flag mask. The universe notes when a thread first reaches such a phase (`flatClassesSeen`, `lateFlagsSeen`), and until then they do not ask. Scratch lookups 5.2% to 3.2% of front-end CPU at 8 threads.
5. `stat`. Listing a package of a directory classpath asked the file system two or three times about each entry. 45% of the serial class headers step was file system calls. Names are tested first now and the `.tasty` sibling is looked up in the listing. This is serial compiler time too (2% of its CPU on the compiler corpus).

6. Imports in the class headers step. It completed every import of every unit, which loads the classes they name: 79% of the step. Imports were there because an import of a root import module at the top level (`import scala.Predef.{...}`) excludes it from the unit's root imports when it is completed, so those have to complete in tree order before any body runs. Only such imports are completed with the headers now: one at the top level whose qualifier ends in the name of a module among the root imports, or is a bare identifier (an earlier renaming import may have made it a name for one). The rest complete when a name is first looked up through them, by whichever thread does, as in the serial compiler (`-Dscalac.parallelTyper.eagerImports` for the old way). The step roughly halves (compiler 160-230 ms to 90-130 ms). The loading it moves into the bodies is still under the coarse root, so the whole gains less: compiler corpus through the pickler at 8 threads 754 against 728 ms, library 529 against 467 ms (best of alternating averages of 14 compilations). The oracle is clean on every corpus, the compiler-benchmark ones included.

Found on the way, in the late phases, and passed to that work: a hang (a completion mark left by a thread that has gone: the info transform marks under the coarse root, a completion under the symbol's own, and the two do not exclude each other), and an NPE (`TypeRef.normalize` reading its cache field twice while another thread clears it).

7. Reading the sources. `Run.compile` read them one after the other before the first phase: 20 ms of the compiler corpus's 45-51 ms of setup. With the parallel parser they are read on a few threads (`Global.readSources`): 26-29 ms.

The late phases' own answer to 1 came in with a merge of that work: an info transform of a symbol that is not a class runs without any claim and publishes its history with a compare-and-set, and mixin's changes to a class run under a claim on that class. (A claim per symbol for the transform, tried first, closed wait-for cycles with the coarse root.)

After all of it, the same full compile, every step on the pool, load average 9-13:

```
            serial     1      2      4      6      8 threads
library     4163    4530   2552   1359   1119   1051 ms    3.96x at 8 (was 2.40x)
compiler    7229    7804   4137   2522   2026   1845 ms    3.92x at 8 (was 2.47x)
```

It no longer levels off at 6. One thread is 7-8% slower than the serial compiler (it was 11-12%). On the compiler corpus at 8 threads, by step, wall with the serial compiler's in brackets: typer 571 including 77 of class headers (2474), jvm 310 (953), erasure 200 (939), specialize 125 (148), refchecks 57 (309), mixin 42 (148). Specialize does not scale and jvm only 3x; they are the next in the late phases.

The class-file oracle on this state with every switch on: 10 runs each of library, reflect and compiler and 20 each of late-phases and post-typer at 8 threads all agree with the one-thread run, without a crash or a hang. Against serial the one-thread run differs in the library's three files of O1 and in 11 class files of the compiler, in fresh-name numbers (`$$typecreator1$2`, `$$anonfun$3`).

Where it stood before the merge. Front end of the compiler corpus (through the pickler) at 8 threads, interleaved runs on a loaded machine: 777-1206 ms before the changes of 2 to 5, 674-934 ms after. A profile of it now has 4% more CPU samples than the serial compiler's, where it had 13%.

What still limits the front end at 8 threads, in order: the bodies' CPU, which is the machine's 34% and little else; the class headers, serial, 90-170 ms of about 700; class file loading, all of it under the coarse root (5% of CPU, and every other thread that needs the root waits for a load to finish); setup, 45-70 ms. Tried and dropped: no serial class headers at all (`lazySignatures`) fails with wait-for cycles through the coarse root, because what holds the root (lazy elements of a base type sequence, `cookJavaRawInfo`, a macro expansion) asks for the info of a class that another thread is completing while waiting for the root. The next step with a real return is class file loading under a claim of its own, per class, which also makes headers on the pool worth another look.

Measuring. The machine was shared with other sessions' soak runs throughout (load average 10-18), and wall times of one compilation differ by 10% and more from run to run. The changes above were judged by CPU samples in the frames concerned, which do not depend on the load, and by interleaved before and after runs. None of the wall times here is fit to quote as a result; a quiet machine or perflab is owed.

## Two races in the info protocol, and a quiet-machine recording (7 Nov)

Two latent bugs, both a reader taking a state another thread had published half way for a finished one.

1. `Symbol.initialize` tested `isInitialized`, which is true once another thread's completion has published a provisional info. `typedDefDef` calls `ddef.symbol.initialize` and went on to type the right-hand side that the completer was still typing, so two threads wrote one tree. Seen as an inferred `Any` with a cast in about 7% of compiles of reflect (`Types$TypeApiImpl.erasure`, `Symbols$TypeSymbol.resolveOverloadedFlag`), as a spurious "not found: value tparam", and as "Byte does not take parameters" in 13 of 700 compiles of small modules. `initialize` now also reads the info when the symbol is completing elsewhere, which waits. After it: 0 of 200 compiles of reflect differ, 0 of 700 small-module compiles fail.
2. `adaptInfo`, which rebuilds a class path symbol's history at its first read in a new `Run`, writes `_validTo` in the new run before it replaces the history. A second thread between the two writes took the path without the lock, found a later node in the old history, took it for another thread's publication and moved `_validTo` past a step that the new history then lacked. Seen only on a `Global` kept across runs with the late phases on the pool: Mixin crashed with "head of empty list" on a forwarder whose alias had missed uncurry's transform (81 of 82 compiles of scalap). A history whose run is not that of `_validTo` now goes to the completion lock, and adapting happens alone under it. After it: 0 of 82.

The lock around info transforms is now taken for one transformer at a time, and only by a transformer that needs it: a transformer answers `transformInfoIfLocal` when it can do a symbol without shared writes, which erasure, posterasure, lambdalift and explicitouter can for a class, and specialize can for a class with nothing to specialize. Before, every class's transforms ran under the coarse root, and at the start of specialize every worker queued about 45 ms behind them.

A recording of the whole of it, full compile to class files in memory, every step on the pool, patched JDK 26, ParallelGC, each corpus alone on the machine (load average 5-7 from idle sessions), mean of 2 after 10 warm-up compiles:

```
            serial     1      2      4      8 threads
compiler     5595   6218   3303   1904   1337 ms    4.2x at 8 (3.8x the night before)
library      3402   3912   2063   1109    781 ms    4.4x
pekko        5031   5999   3045   1861   1455 ms    3.5x (3.2x)
junit        4091   4974   2491   1520   1232 ms    3.3x
scalapb      2363   2654   1415    807    584 ms    4.0x
re2s-x32     2866   3205   1689    921    647 ms    4.4x
```

The same recording after the late-phases and one-thread-parity branches were merged (the three fixes of I19 and I20, the stable `Global.typer` restored, and the one-thread work), same setup, load average 5-12:

```
            serial     1      2      4      8 threads
compiler     5448   5933   3159   1767   1291 ms    4.2x at 8
library      3459   3892   2041   1116    769 ms    4.5x
pekko        5255   5854   2991   1764   1430 ms    3.7x
junit        3461   3964   2136   1249    946 ms    3.7x
scalapb      2459   2698   1453    820    614 ms    4.0x
re2s-x32     2868   3085   1654    915    619 ms    4.6x
```

One thread is now 7-15% slower than serial (compiler 9%, library 13%, junit 15%), from 11-22%. The class-file oracle on the merge at 8 threads with every switch on: reflect 0 of 30 and 0 of 10, compiler 0 of 10, library 0 of 10 and pekko 0 of 10 differ from the one-thread run; against serial the one-thread run differs in the known fresh-name files (compiler 11, library 3) and in 5 of pekko's, not inspected.

The pekko corpus's two longest units were `FanInShapeN.scala` and `FanOutShapeN.scala` (353 ms each in typer and 285 each in refchecks at 8 threads): twenty-one constructors each build a port list as a cons chain of `Inlet[T0] :: Inlet[T1] :: ... :: Nil`, whose element type is inferred as a lub at every `::`. The corpus now passes `List[Inlet[_]](in0, in1, ...)` instead (the originals are beside the files as `.orig.txt`). pekko serial 5255 to 4251 ms; at 8 threads 1430 to 1035 ms (3.7x to 4.1x), with typer 530 to 341 ms and refchecks 317 to 42, measured at load 13-29 from other sessions' file indexing and builds, so the serial figure is firmer than the parallel one. Typer's longest unit is now `UnzipWithApply.scala`, 144 ms.

### The GC pause, and the allocation profile (7 Nov)

A GC log of the compiler corpus at 8 threads (12 GB heap, ParallelGC, so a 3.3 GB eden): a compile allocates 3.5-4.3 GB, so one young collection lands in nearly every compile and copies what is live in the middle of it, 0.5-1.3 GB, at 30-100 ms (`3660M->846M 60 ms`, `4358M->1337M 102 ms`). The 444 MB survivor space overflows, so part of that is promoted and the old generation is later collected in full. The pause is the live set being copied, not a misconfiguration; the levers are a larger young generation (the benchmark could, a build less so), allocating less per compile, and retaining less mid-compile.

The x-ray's CPU recording now samples allocation as well (`alloc=256k` in the same JFR, a third profile button, "allocation", and jfrconv's `--alloc --total` graph). Per compile at 8 threads: compiler 3.7 GB, pekko 3.0 GB, library 2.3 GB, the same on one thread to within 1%. The largest self frames, compiler corpus, per compile:

```
 489 MB  13.2%  DirectMethodHandle.allocateInstance   closures: lambda captures through invokedynamic
 206 MB   5.6%  TypeRef.apply
 180 MB   4.8%  Context.make                          makeSilent 45, ValDef.transform 44, makeNewScope 35, DefDef.transform 20
 150 MB   4.0%  Arrays.copyOf                         newNestedScope copies the hash table, 56; ByteArrayOutputStream 37; BTypeExporter 19
  84 MB   2.3%  List.::
  74 MB   2.0%  ListBuffer.addOne
  72 MB   1.9%  Type.asSeenFrom
  59 MB   1.6%  TypeHistory.apply                     info_= 52, publishStep 7
```

Who makes the closures (nearest caller outside `java.lang.invoke`): `TypeHistory.<init>` 35 MB, `Typer.typedSelect` 31, `Scope.enter` 29, `Infer.checkAccessible` 26, `ValDef.transform` 22, `ImportInfo.importedSelectedSymbol` 13, `SymbolLookup.searchPrefix` 10, `cloneSymbols` 10, `Symbol.suchThat` 9. Two of those are by-name arguments that the build's inliner did not remove: `assert(cond, this)` in `TypeHistory`'s constructor (stock code), and the spike's `Scope.writing(body)` around every `enter`. Both are rewritten as plain code; the rest are stock and worth the same treatment in 2.13.x on their own (`sym.filter(context.isAccessible(_, ...))` in `checkAccessible`, the `silent` and `withMode` closures in `typedSelect`).

Specialize on the compiler corpus at 8 threads went from 125 ms of wall to 38. What limits each corpus now, from the per-phase tables (`phases.js` over the recording):

- One thread against serial: 11-22% slower (compiler 11%, pekko 19%, junit 22%), spread over every phase. The one-thread-parity work has it at about 40% per-thread phase and period lookup, 15-30% GC from extra allocation, and the rest `Stamped` holders, a `SubstTypeMap` per call and class headers.
- The longest unit. pekko's refchecks takes 317 ms at 8 threads because one unit takes 301 (261 serially), and its typer has one of 366 ms in 530. Only tasks finer than a unit help.
- jvm: 211 ms at 8 threads on the compiler corpus against 443 serially, with one span of 112 ms; pekko's delambdafy 107 ms with one span of 99 against 63 ms serially. Both are a pause of the whole JVM: every worker has a span of 98-112 ms there with 1-7 ms of CPU and no lock wait. With a 12 GB heap and ParallelGC that is a young collection copying the compiler's live graph, about 100 ms once in a compile, which is 2% of the serial compile and 7% of the one at 8 threads. Not looked at further: a GC log to say which collection it is, and whether a larger young generation or less retained per run shortens it.
- Typer CPU summed over workers at 8 threads is 44% above serial on the compiler corpus (2818 against 1958 ms) and 39% on pekko, diffuse in the profile.

The false cycles of about 1 in 160 small-module compiles and the pickler's "bad type: <error>" crash came down to three bugs, I19, I20 and I21. Eager imports are not needed for any of them.

## The one-thread gap: where it goes, and what was taken back (7 Nov)

A full compile with every step on the pool, one worker, against the serial compiler. The gap is paid at every thread count, so it bounds the speed-up.

How it was taken. Patched JDK 26, ParallelGC, 12 GB heap, classpath, sources and output in memory, alone on the machine in the timing slot (`slot.sh`; load average 3-15 from other sessions' background and idle processes). `ROUNDS=<n> run.sh <corpus> 1` alternates recordings of serial and one thread in one JVM, 8 warm-up compilations first, and `rounds.py` gives medians per step. `FRAMES=1 REPEATS=4 profile.sh <corpus> 1` alternates CPU profiles at 1 ms, and `frames.py` compares them by frame. Baseline: 7731edde06, 6 rounds, 2 profiles. Result: the tip, 4 rounds, 1 profile.

The machine's speed moved by up to 25% between runs during the night, on the serial compiler as much as on the other (the serial compile of the compiler corpus took 5.4 to 7.2 s at no change to its path, and an older build ran slower than a newer one in alternating JVMs). So only a comparison within one JVM means anything: the ratio of the two columns, not a column against another run's. Differences of a point or two between runs of that ratio are still noise, which is below what most of the single changes are worth.

Result, compile median:

```
                 before (7731edde06)                  after
              serial     one        gap        serial     one        gap
library      3338 ms  3713 ms  +374 ms +11.2%  3521 ms  3796 ms  +275 ms +7.8%
compiler     5446 ms  5970 ms  +524 ms  +9.6%  5790 ms  6147 ms  +357 ms +6.2%
```

Per phase, ms of wall time, medians, serial and one:

```
                    library before    library after     compiler before   compiler after
typer                1028   1108       1074   1120       1890   2040       2053   2144
class headers           0     38          0     39          0     44          0     47
erasure               563    614        588    636        767    821        807    851
refchecks             460    511        491    535        269    295        279    299
patmat                104    110        104    107        614    657        604    636
jvm                   276    307        300    322        456    527        472    507
mixin                 140    157        148    162        110    124        112    133
specialize            107    123        117    130         98    139        103    146
uncurry               104    116        108    118        227    220        259    290
parser                 57     62         60     63         79     83         81     89
all others            477    531        507    528        919    988       1001    967
sum of steps         3316   3677       3496   3758       5427   5937       5770   6111
```

A single small phase swings by 30 ms in these tables (`uncurry` and `fields` on the compiler), so read the sums.

Self samples per compilation (about ms of CPU) on one thread, against serial, by kind of frame:

```
                                        library before  after   compiler before  after
phase, period, per-thread lookups          +125          +51        +200          +61
stamped type caches                         +37          +41         +60          +52
class headers (step, moved work)            +39          +40         +45          +45
info read: rawInfo, marker, fence           +17          +15         +45          +42
claims, locks, ConcurrentHashMap            +20          +25         +35          +28
outside any span                            +23          +28         +28          +20
pool, thread hand-off, spans                +17          +22          +1           -3
allocated, one against serial, MB          +144          +93         +97         +102
```

GC threads are left out of that table: with 2-4 GB allocated per compilation and a heap of 12 GB there are few young collections, and how many fall into a profile is chance (the library had no pause at all in the recordings and +100 samples or more in the profiles). What allocation costs is its share: 2-4%, of a GC that is an eighth of the CPU at most. JIT threads run the other way, by 20-200 samples, and were not looked into. The profile of the compiler after the changes fell in one of the machine's slow spells for its serial half, so its serial column is high and these differences flatter it by a little.

What was done, one commit each:

- A worker's phase and period are fields of its `ScratchThread`, read in a type test, an owner test and a field load that are inlined at every site. `Symbol.flags` needs no phase for a symbol with no late or anti flags. Lookups fell by 60-70%: the only change of the night that is visible by itself, and about all of the improvement.
- `Type.subst`'s most recent map is cached per thread, as the serial compiler caches one. 50 MB less on the library, 30 MB on the compiler.
- The stamped caches' fast paths test the serial field for empty first, so a concurrent compiler asks for its period once, not twice; `lockedTypeCaches` is no longer a lazy val with a volatile read; `isValid` reads the phase once. Below the noise.
- `info` loads `_validTo` once and uses one load-load fence, not two, the marker's volatile load ordering what the second one did. Below the noise.
- `Run.compiles` asks the `ConcurrentHashMap` directly, without the wrapper's `Option`. Below the noise.

Tried and dropped: no `Stamped` rewrite when a period changes, with the stamp tested for validity each time (equivalent by transitivity of `isValid`). It removes writes to shared types and 10-14 MB, and costs the same in stamped samples, since the slow path is then taken at every read. At 8 threads on the compiler, in two alternated pairs, 1272 and 1243 ms with the rewrite, 1248 and 1248 ms without: nothing.

Not worth doing, on these numbers: `CompletionLock.claim` with one compare-and-set and no allocation. Claims are about 0.5% of a compile, and most of that bucket is `ConcurrentHashMap` lookups (`BTypes`, which the serial compiler does as well, and `CompilationUnit.synthetics`). Itable stubs are another session's.

What is left of the gap, 275 ms on the library and 357 ms on the compiler, and why it is not cheap to remove:

- Lookups, 50-60 samples. A worker reads its phase through a test of the current thread; the serial compiler reads a field. What is left is that test, at every read of the phase, the period and `Scratch`. Passing the period down would be the next step, and it would mean new signatures through `Symbol` and `Type`'s hot paths.
- Stamped caches, 40-50 samples. A holder object and a pointer chase per read are the price of reading a value and its period as a pair without a lock, and that is not optional past the pickler.
- Class headers, 40-45 samples. Moved work, most of which the serial compiler does inside the typer; the parallel path needs the headers done before the bodies.
- The info read, 15-40 samples. One fence and one volatile load per read: the publication protocol.
- Claims and maps, 25 samples, and the pool and what is outside any span, 25-50: `patternsIn` weighing the units is 8 of that. On one thread its weights are not needed to balance anything, but they do set the order of patmat's units (`largestFirst`), and the one-thread run is the oracle's reference, so it stayed.
- Diffuse, the rest: specialize stands out at +30-40% on the compiler (+40 ms) with no frame above 5 samples, and every late phase is 3-10% slower with nothing in particular to blame but the above.

The serial compiler without the flag against the merge-base with 2.13.x (d32d907524), at the state before the last commit (which does not touch its path beyond one null test), alternating JVMs, 2 each, 8 warm-up compilations and the median of 6, the same arguments and the same classpath for the compiled code: library 3835, 3747 ms at the merge-base and 3881, 3787 ms here (+1.1%); compiler 5586, 5476 ms and 5720, 5506 ms (+1.5%). CPU times agree. That is at the edge of the noise, and there is no frame to take back: the serial profile has none of the concurrent paths in it above sampling skid.

Checks at the tip. Pickle oracle after the first change and again after the fence, library, reflect and compiler at 5 runs and stress-shared at 40: no parallel run differs from the one-thread run, the one-thread run agrees with serial but for O1 on the library. Class-file oracle with every switch on, library and compiler at 5 runs: no parallel run differs from the one-thread run; against serial the one-thread run differs on the library in the three classes of O1 (`Predef`, `WrappedString`, `StringBuilder`) and on the compiler in the fresh-name numbers of 11 class files (`$$typecreator1$2`, `$$anonfun$3`), the same as before this work. Junit `scala.reflect.internal.*` and `scala.tools.nsc.typechecker.*` pass. Not verified: wall times on a machine with nothing else running at all, and whether the improvement holds beyond one thread (the gap is paid at every count, but the 8-thread numbers above were not taken against the baseline).

## DONE

- Stage 0: `-Yparallel-typer`, `-Yparallel-typer-threads`; `Global.isConcurrentUniverse` from the setting (and `isParallelCompiler`, so that a toolbox's `ReflectGlobal` stays serial, I10); per-thread phase, current unit, fresh name creator.
- Stage 1: completion lock and the protocols above; driver with signatures first (class headers, members, then annotations), lazy val forcing, watchdog, timeline, buffering reporter; the confinement and synchronisation above; oracle scripts (`oracle.sh`, `oracle-all.sh`), `SigText`, corpora of its own (annotations, annotations-neg, case-classes, stress-shared).
- Stage 1 loose ends (A): atomic flags; loaders under the lock with their roots marked (I6); scope entries published safely; the state map's work list accounted for (annotations.tsv, 656 → 324 fields, the rest lazy-val monitors forced up front); lazy-signatures mode measured; O1 explained; the compiler-benchmark corpora pass.
- Stage 2a (B): type caches off the lock, local completions off the lock (I7 found on the way), `cookJavaRawInfo` and `exists` fast paths, the phase register in the worker thread, striped uniques, a leak of compilers through `ThreadLocal`s (I8).
- Stage 2b, first steps: scala-sharing vendored (relocated to `scala.reflect.internal.sharing`, I12) and wired into the build, `restarrFull` included; the completion lock on its `CoarseLock`; `PerKeyClaims` behind `claims=perKey`; parallel member signatures behind `parallelSignatures`, with symbol roots and lending (I9 fixed; I11 open on stress-shared).
- X-ray profiles: async-profiler spans for the x-ray's spans, `profile.sh`, and a flame graph per window, step, unit, span or completion on the page (section "Profiles in the x-ray").
- Serial regressions: partest `--pos --neg --run` (5429 tests) and junit `scala.reflect.internal`, `nsc.typechecker`, `nsc.reporters`, `nsc.symtab` (378) pass without the flag (partest last run at 0a4b168aa9, with `JavaUniverseForce` updated after).

## TODO

- Measure the cost to the serial compiler (no flag) with a paired A/B against fd95c5c1c1 (the profiling agent's perflab campaign); see "Hot-path costs".
- The oracle: compare classfiles of a full compile as well as pickles, library included.
- Stable messages for genuine cycles (see "Only the class headers serial", 2).
- Parked precursor for 2.13.x: annotation defaults depend on batch order (O3).
- Parked precursor for 2.13.x: the serial compiler's classfiles depend on batch order through `freshNameCreatorFor`. `DeterminismTest` fails, with no parallel flag, for class-level vals using named arguments (`qual$`, `x$`), a `collect` partial function (parameter `x1`/`x2`) and `typeTag` (`$typecreator` class names), when another file demands the later val. A creator per member, made unconditional, cures all three, at the cost of changing stock numbering (`.check` files not yet looked at). O1 fails the same test through `ScalaSignature` and is not cured by it.
- The 50-run oracle bar per corpus and a quiet-machine rerun of `bench.sh` and the x-ray are owed.
- The tail: tasks finer than a unit.
- The parser a unit per task: done behind `-Dscalac.parallelTyper.parallelParser`; owed before it becomes the default: `bench.sh` at 1, 4 and 8 threads on a quiet machine, and a soak of stress-shared after I13's fence.
- Superaccessors, extmethods and pickler a unit per task: done behind `-Dscalac.parallelTyper.parallelPostTyper` (see its section); make it the default after timings on a quiet machine.
- `-Ystop-after:pickler` NPE on a super call to a Java default method (stock compiler; see the post-typer findings).
- `elemsCache` residue: `unlink` in the middle of a scope while another thread reads it (serial phases only today).
- `_annotations` read-modify-write on a symbol of another unit during body typing (not known to happen).
- The state map's 91 "other" fields were annotated by reading, not by a test each.

## Decisions made overnight

Each is reversible (a system property, or its own commit).

1. `_rawflags`: atomic compare-and-set in a concurrent compiler rather than an argument that every cross-thread writer holds the lock (the writers are too many to argue about; a lost `LOCKED` update is a missed cycle or a stuck lock). Cost to the serial compiler: one test per `setFlag`.
2. Case class synthetics entered during body typing: safe publication of scope entries (release fence, head-keyed element cache) rather than moving `addSyntheticMethods` into the pre-pass (which would change when synthetic bodies are typed and needs the template's typer context). Argument and residue in "In-progress protocols".
3. O1: leave it. The serial compiler is the order-dependent one (proved with `order=source`, `lazySignatures`, `noForceLazyVals`); the namer-level fix (`eagerImports`) does not make the parallel path match serial and was reverted. Units stay largest first by default; `-Dscalac.parallelTyper.order=source` keeps source order.
4. Lazy-signatures mode stays off: it passes the oracle, but the cross-unit completer hazard is unbounded in principle and the bodies run mostly under the lock.
5. Stamped type caches off the lock (B.2) with per-thread in-progress markers: two threads may compute the same value, and the re-entry answers mimic the serial protocol at that point. `-Dscalac.parallelTyper.lockedTypeCaches` restores the locked serial protocol.
6. Symbols local to a body complete without the lock (B.1), claimed with a compare-and-set on `_completingThread`; the confinement argument is that no other thread reaches them (inferred member types are free of local symbols). Waiters on a marker park and look again, or wait for the lock if it is held. `-Dscalac.parallelTyper.lockedLocalCompletions` restores the lock. Known risk: a thread that reached another unit's local symbol while holding the completion lock would wait for a local completion that needs the lock (a deadlock the watchdog would show); no path to it is known.
7. The phase register for worker threads (B.4) is a field of the thread (`ScratchThread`), found with a type test; other threads keep the `ThreadLocal`.
8. scala-sharing is used through its registries (`CoarseLock`, `PerKeyClaims`) behind the existing facade rather than through the macro API at each site: the compiler's sites are few, keyed by symbols or one coarse root, and the evidence types would mean threading `Holds` through the completers' signatures for no check the facade does not already make. The macro subproject is wired and builds, ready for `Once`/`Stamped` if a site wants them.
9. Parallel signatures use symbol roots (with lending at a provisional info on a wait cycle) rather than unit roots: unit roots gave false cycles on reflect (I9). The default since I11 was fixed (6 Nov), without the 50-run bar.
10. The parallel parser stays behind `parallelParser`, off: it saves ≈50 ms of 890 on the library and adds a pool step's tail; not worth the default before the tail is shortened. Superseded: it did not scale then because of the name lock; see "Parallel parser and the index".
11. Unit order stays largest first by source length; ordering by tree size cost ≈40 ms serially for ≈13 ms of tail.
12. The vendored scala-sharing is relocated to `scala.reflect.internal.sharing` (I12): the vendoring README records the relocation so that a re-copy repeats it.

## Hot-path costs

Sites where the serial compiler (no flag) now pays a test or more, for the profiling agent to check against fd95c5c1c1:

- `SymbolTable.phase`, `phase_=`, `currentPeriod`: a test of a final field (`phasePerThread`) and a branch.
- `Symbol.info`: `isConcurrentCompiler && isCompletingElsewhere` after the `_validTo` test (one field load and branch when serial). `rawInfo`: the same test at entry.
- `Symbol.setFlag`/`resetFlag` and the `LOCKED` updates: a test of `isConcurrentCompiler` (an outer field load).
- `TypeSymbol.tpe_*`: a test of `isConcurrentCompiler`.
- `BaseTypeSeq.apply`: a test of `isConcurrentCompiler`.
- `SingleType.underlying` fast path: `(cache ne NoType) || !isConcurrentCompiler`.
- The slow paths of the stamped caches: a test (`lockFreeTypeCaches`) and `underCompletionLock` (a lazy val read and a null test).
- `Scope.enter`, `unlink`, `rehash`, `enterIfNew`, `enterBefore`: a test (`writing`, `publishEntry`); `Scope.size` and `toList`: a holder object per cache refill (allocation), and `size` no longer reuses a size cached separately from the list.
- `Names._chrs` volatile: every read of a name's characters is a volatile load (cheap on ARM: `ldar`).
- `atPos`: a test of `isConcurrentCompiler`.
- `MethodSymbol.typeAsMemberOf`: a test.
- `ModuleClassSymbol.implicitMembers`: an allocation per refill.
- `Global.withPropagateCyclicReferences`/`propagateCyclicReferences`: a `perThread` holder's `get` (a field load and a branch when serial, plus an array).
- The typer's per-thread holders (`perThread(...).get`): a field load and a branch when serial, on `isAccessible` (`lastAccessCheckDetails`), every implicit search (caches, shadower), every pattern (`typingInPattern`), every polymorphic application (macro `undetparams`), every `lookupSymbol` (`symbolLookupCaches`), `instantiate`.
- `CompilationUnit`/`Run`: `currentUnit` and `currentFreshNameCreator` read a field and branch.
- `Context.implicitssImpl`: a test of `isConcurrentCompiler`.
- `Scratch.checkOwner` in `phase_=` and `completeInNextFrame`: a test of a final field.

In a concurrent compiler, `phase` costs a `scratch` lookup: a lazy val read, then for a worker thread a type test and two loads (`ScratchThread`), for any other thread a `ThreadLocal` and a weak reference.

## Overhead

Measured by the profiling agent with perflab at 3c72e64ddf (re2s, typer only; campaigns paired per fork with an A/A noise floor; dashboard https://perflab.localhost/p/scala2-b7bb3d/ab/index.html; rerun with `tools/parallel-typer/perf/compare.sh <commit>`; microbenchmark `test/benchmarks/.../SerialPathBenchmark.scala`):

- No flag, spike against fd95c5c1c1: +2.5 to +5.7%, of which the completion lock's commit +2.1% and the flag's plumbing +1.2%.
- One worker against no flag: +17 to +24%. Attribution: scratch lookups 10.4%, `forceLazyVals` 7.8% (and all of the +9% allocation), `ThreadLocal`s 3.8%, the name table's locked paths 2.0%, monitors 1.6%, `isCompletingElsewhere` tested twice per `info` 1.5%. `_completingThread` adds 8 bytes to each term and method symbol.

Integrated (one commit each, after 7465c8e730): the scratch state of a serial universe in a field (2539db69d3); `ScratchThread.scratchOwner` not volatile (4c316d022e; its "last scratch" field for the main thread dropped, the driver thread's field path covers it); `PerThread` values in a slot of the `Scratch` (b2c1813c0a, -3.8% on one worker alone); `SerialPathBenchmark` and `perf/compare.sh`; the name table's array read without a volatile load, re-read under the name lock past the stale array's end (0aef07c337); `phase` and `currentPeriod` as one load and a test, markers in the shared registers when they are per thread (881a1ce0e6); the workers' scratch lookup split so that it inlines, with the driver thread's field in the inlined part (6f2732993d); `lockedIfConcurrent` on the typer's shared maps (4b35b8a56d). Then: the cake's lazy vals forced only for two workers or more (eec7a6e439: the deadlock they prevent needs two), and `info` tests the marker once (6d66610ab1: `rawInfoHere` after its own test, `rawInfo` keeps its wait). The profiling agent measured about +18% for one worker against no flag with its fixes, before the last two.

Check after integrating (6d66610ab1): oracle at 8 threads with `checkScratchOwner`, library, reflect, compiler 10 runs each clean (library O1 as before); stress-shared 10 in 200 (I11 only, `ArrayList`/`TreeSet` self-type errors; 7 in 200 before, within noise); junit `scala.reflect.internal.*`, `scala.tools.nsc.typechecker.*` (285) pass. `bench.sh library` (6 warm-up, 5 measured, load average 5-6): serial 1803 ms, one thread 1520, 4 threads 880, 8 threads 791 (2.28x); bodies 1045 / 338 / 318 ms, pre-pass ≈250 ms. Not an A/B (no same-session run of the build before): perflab is for that.

Left as future work: a side table for `_completingThread` (the 8 bytes per symbol); the lock-order alternative to forcing lazy vals with several workers (take the `definitions`/`runDefinitions` monitors before the completion lock), not simple, since the monitors are taken inside the lazy vals' generated code.

## Claims per completion root: design sketch (stage 2b)

Read first: Scala 3's `parallel-signatures.md` (§5.7 class headers first, §5.8 the classpath root) and the FAQ of retronym/scala3#84 (wait-for cycles, I74 a Namer completion cannot be unwound, I87/I98 stale wait-for graphs).

**What a root is in Scala 2.** The smallest unit whose mutable state one completion writes.

- The classpath, as one root (`ClasspathRoot`, coarse, reentrant). Every loader (`SymbolLoader.complete`: class, module, module class), every unpickler completer (`LazyTypeRef` reading the classfile's `Scan` and the shared `PickleBuffer`), the classfile parser (one instance per `Global`, with `busy`, `in`, `pool`, the `ReusableDataReader`), and the TASTy reader share state across files, so a root per file would need a parser per load first. Scala 3 went per-file and came back to one classpath root after a deadlock between two classpath files that refer to each other (§5.8). A classpath completion never waits for a source completion (it reads no source symbol's info), so this root is always last in the lock order and closes no cycle.
- A Namer symbol: the root of a source symbol's completer is the symbol, except that a module's completer also sets its module class's info (`moduleSig`), so a module and its module class share the module's root, and a class whose value-class check enters a companion (`ensureCompanionObject`) writes the owner's scope (a package's: serial in practice, since it happens in the class's own completion; the scope publication argument covers readers).
- Annotations: a lazy annotation's root is its symbol's root (it types with the symbol's context).
- Local symbols: no root (confined to their unit's thread, B.1).
- Macro expansion: a coarse root of its own, taken before the classpath root and never inside a Namer completion that another claim waits on (with signatures first, Namer completions run serially, so no worker holds one).
- The stamped type caches: no root once B.2 takes them off the lock.

**`_completingThread` becomes the claim.** Today a symbol records the thread completing it and a reader that sees another thread waits for the one lock. With claims, the symbol records its root's claim (or the reader finds it from the symbol: the classpath root for a `SymbolLoader`/`LazyTypeRef` info, the symbol itself for a Namer completer); `isCompletingElsewhere` becomes "the root is claimed by another claimant", and the wait is on that claim's latch (`PerKeyClaims`), reentrant for the owner. When the owner releases, the waiter re-reads the symbol: completed, or left lazy by an unwound owner, which it then completes itself. `LOCKED` keeps its meaning (re-entry on one thread is a cycle), and a reader never acts on another thread's `LOCKED` (`isLockedHere`). The peeks (`typeParams`, `exists`, `isLockedHere`, the scope-publication readers) wait the same way.

**Which waits can cycle, and what each waiter does.**

- During the bodies, with signatures first, the claims a worker can hold nest in one order: macro root, then classpath root; Namer roots are not taken (their completions ran in the pre-pass) and local symbols have none. A strict order, so no wait-for cycle. This is the case to build first: it should equal the current lock in behaviour, with completions of different classpath files still serial but body work no longer blocked behind them.
- With parallel signatures (2b.4), Namer roots are taken on several threads. A wait-for cycle between two Namer roots A and B: on one thread, B's completion nested in A's would reach A while `LOCKED`, a `CyclicReference`, the same error serially, unless the nested read is satisfied by a provisional info that A published before it asked for B (a method's inherited result type, `classSig`'s early class info): that is the Scala 2 form of Scala 3's unstable read. So: a waiter whose wait would close a cycle, and whose waited-on symbol has a provisional info published by its owner, reads that info (lending restricted to provisional infos, as the serial nesting would); with no provisional info it is a genuine cycle and the waiter raises the `CyclicReference` a serial run would raise. Order matters: on one stack only one of the two can be the nested one, so the choice of which waiter reads and which errors must follow a fixed rule (by unit order: the completion that the serial pre-pass would have started first is the outer one), or the result differs from serial. A cycle through the classpath root cannot occur (it waits for nobody). A cycle through the macro root: a macro expanded inside a Namer completion (an inferred result type whose body expands a macro) waits for the macro root while another thread's expansion waits for that Namer root: the macro root must then be released while waiting, or macros inside Namer completions expanded only on the main thread; first measure how often it happens.
- Detection: the library's `PerKeyClaims` keeps the wait-for graph with the edge checks Scala 3 needed (I87: stop at a released claim; I98: every claim still held and every edge still standing after the walk).
- A Namer completion cannot be unwound and retried (it consumes the namer's tree attachments, I74 in Scala 3, and in Scala 2 `unit.transformed`/`synthetics`), so no batch reader backs off: every cycle is answered in place, by lending or by the serial error.

**Lock order against monitors and the reporter.** Unchanged rules: lazy vals that complete symbols are forced before the workers, or take the claim before their monitor (`forcedInfo`); no `synchronized` block calls anything that can complete; messages are rendered before the reporter's monitor, and `PerRunReporting`'s monitor is never held while claiming.

**Build wiring.** scala-sharing at 33614c2 vendored with its package (`io.github.retronym.sharing`): its runtime (`src/`, `src-2/`) compiled into `reflect` (and so shipped in scala-reflect.jar), its def macros (`macros-2/src/`) as an sbt subproject compiled by the reference compiler (STARR, the build's `scalaVersion`) with STARR's scala-reflect, a compile-only dependency of `reflect`. That works for quick (compiled by STARR); for a bootstrap (`restarrFull`, locker), the macros are expanded by the new STARR, which is the previous quick: the macro subproject must be republished with it, or depend on the STARR the bootstrap step uses, which the build already arranges for `library`/`reflect`. `Publish.java` uses `VarHandle`: reflect's javac options target 1.8, so that file needs `-source 9 -target 9` (or the class moves to the runtime-only Scala side with the fences we already call); `library` cannot use the library (it is compiled without scala-reflect). Expected friction: the macros must not be in the same compilation run as their expansions (they are not: a separate subproject), and MiMa will report the new classes (acceptable for the spike).

## Future work (stage 2)

- Timeline per worker (`-Dscalac.parallelTyper.timeline` writes per-unit spans with lock wait and hold; not yet looked at).
- No lock for completions of symbols local to a body (confined to the unit's thread): the largest single site.
- The stamped type caches without the lock: per-thread in-progress markers, value then stamp (or one immutable holder), so that only a real recomputation contends.
- Phase register as a field of the worker thread.
- A claim per completion root (the `PerKeyClaims` of scala-sharing) instead of one lock; per-thread in-progress markers for the stamped caches so that their slow paths need no lock.
- Deterministic symbol ids per unit if `isLess` ties show in pickles.
