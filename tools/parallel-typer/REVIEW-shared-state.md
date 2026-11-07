# Shared state under -Yparallel-typer: a review

A review by reading of what the worker threads share, with confirmation runs where a corpus could provoke the interleaving. The stress harness and the class-file oracle are clean on reflect, library, compiler, pekko, scalap and re2s without the fixes here.

## Summary

Nine findings are confirmed by a run, and all are fixed. Four more are fixed on the argument alone. Twenty-three are plausible and unreproduced, or have a window too narrow to show, and are left as they are. The rest of what the workers reach was examined and is protected or confined; the table at the end has every field.

Most confirmed findings are of one kind, which the race log does not treat as a family. The serial compiler has an order dependence between units: a fact about a symbol changes when its own unit reaches some step, and another unit may read it before or after. Serially the answer is stable for a given command line. On the pool it varies between runs. The value is written safely, so this is a race on the order of the program's facts, not on memory. These are `Serializable` on companions, the parents of a generic class, the annotations of a getter, the accessor an `unapply` selects, and implicit eligibility. O3 is of this family. The harnesses do not see them on the benchmark corpora because they depend on rare shapes of program: each corpus here fails in most runs once the shape is present.

The fixes for that family follow O3: under the flag, the answer is the one serial gives when the defining unit comes first. So a parallel compile can differ from a serial one whose command line has the user first, and the one-thread run differs from serial on some of these corpora. Making that same choice in the serial compiler would remove the difference and is a change to stock behaviour.

## Confirmed and fixed, by consequence and likelihood

Rates are runs that differ from the one-thread run, with `tools/parallel-typer/stress.sh`, 8 threads, every pool switch, class files compared.

**R8. Implicit eligibility depended on whether another thread had completed the implicit.** `ImplicitSearch.isValid` (`Implicits.scala:1098`) keeps an implicit with no explicit type that is defined later in the same file out of the search unless it has been completed. Both of its tests read the completion state: `sym.isInitialized`, and `hasExplicitResultType`, which answers true once `rawInfo` is no longer a completer. So in `object O { def a: Foo = implicitly[Foo]; implicit val foo = new Foo }`, `a` compiled if a thread typing another file had already completed `foo` through `import O._`, and failed with "could not find implicit value" if not. Corpus `implicit-later-neg`: 60 of 60 before. After the fix, one run in 300 differed, its output not kept, and none of 5000 more in fresh JVMs, on slow cores and at one long stretch; what that run was is not known.

Fix: `-Yimplicit-eligibility-by-position`, implied by `-Yparallel-typer` and off otherwise. Such an implicit is not eligible before its definition in its own file, whatever its state. The namer records at enter that the type is inferred, as the completer's tree is gone once the symbol completes. Local symbols keep the old test: only their own unit's typing reaches them, in tree order, and that test is what makes one eligible after its definition in a block. This rejects a program that serial accepts because an earlier statement forced the implicit (`val y = foo; def a = implicitly[Foo]; implicit val foo = new Foo`); `-Yimplicit-eligibility-by-position:false` restores the old test, with its dependence on timing under the flag. No such program is in library, reflect, compiler, pekko, junit, scalapb, re2s, re2s-x32, re2j, scalap, better-files, induction, map-seq-inference or vector: each compiles under the rule, with the same pickles as before relative to serial. The test is `ImplicitEligibilityByPositionTest`.

**R1. A companion's `Serializable` changes when the module is typed.** `typedModuleDef` calls `clazz.makeSerializable()` on the companion of a serializable class, which is every case class's. So `List(A, B)` of two case class companions is inferred as `List[AbstractFunction1[Int, Product with Serializable] with Serializable]` or without the last part, according to whether they have been typed. Output: the pickle and generic signature of the use. The `setInfo` also ran under readers on the pool. Corpus `serializable-companion`: 100 of 100, then 0 of 100; `-neg`: 200 of 200, then 0 of 200. Fix: `serializableCompanions` after the class headers, on the driver, driven from the `ClassDef`, because a synthetic companion is not in the tree yet.

**R3. A generic class's specialized parent.** `SpecializeTypes.transformInfo0` forces its parents before reading `specializedClass` only when the class has no type parameters. `class G[U] extends Foo[Int]` extends `Foo$mcI$sp` or `Foo`, in the class file's super class. The read is on the path without the lock, so it could also see a specialized class between its publication in the map and its first `setInfo`. Corpus `spec-parents`: 200 of 200, then 0 of 500. Fix: such a parent is forced under the flag.

**R4. `c.parse` replaces the shared reporter.** Any other worker's report during the parse went to the macro's `StoreReporter`: a warning lost, or an error turned into a `ParseException` at the macro's site, or lost so that a broken program goes on. `macro-parse.sh`: 60 of 60, in lost warnings, then 0 of 60. Fix: `Global.withReporter`, per thread in a concurrent compiler. The back end's `withoutReporting` has the same fix; see A3.

**R2. A bridge copies the annotations of a getter in another unit.** A lazy or trait getter keeps its field's annotations until erasure reaches its definition. `addBridgeIfOK` copied them from wherever that had got to, so `RuntimeVisibleAnnotations` on the bridge varied. Corpus `bridge-annotations`: 20 of 200, then 0 of 1000. Fix: read through the filter that erasure will apply.

**R7. `compactify` shares one `MessageDigest`.** `update` and `digest` from two threads give a digest of both names, or of nothing, for a flat name over 233 characters. A class is named differently between runs, and two can get one name. Corpus `long-names`: 1 of 500, then 0 of 2500. Fix: a monitor around the two calls.

**R6. `permittedChildren`'s in-progress mark.** A plain field on the attachment of a sealed Java class, read by other threads as their own re-entry, so `children` was empty for them. Exhaustivity warnings vanished and "unreachable code" appeared. A class file parent check can hold the window open across a class load. With classes on the class path: 40 of 40, then 0 of 100; corpus `sealed-java`, from source: 2 of 20, then 0 of 200. Fix: the mark is per thread.

**R5. `addChild` loses a child.** A local class is registered by the thread completing it, which may be a foreign completion beside the unit's own thread. Children that are members are registered in the serial headers, so the pickle cannot differ (it records only that a local child exists); the effect is on exhaustivity's list of cases. Corpus `sealed-local`: 56 of 100, then 0 of 500. Fix: a monitor around the update.

**R9. `renamedCaseAccessors` decides the accessor `unapply` selects.** A plain map, written while a case class's template is typed and read when its synthetic companion's `unapply` is built, on the first thread that asks. For `case class C(private val x: Int)`, `unapply` selects `x$access$0` or the expanded `ca$C$$x`, with different class files for `C` and `C$`; `javap` differs serially with the order of the files. Corpus `case-accessors`: 26 of 100 with R1's step turned off, 0 of 500 with it. R1 closes this, since a synthetic companion completes with the headers, before any template is typed. The map also takes a monitor, for local case classes, whose companions complete on the pool.

## Fixed on the argument, not seen failing

**A1. `ExtraLazyAnnotationInfo.typeSymbol`** types the annotation's class in a lazy val, under the annotation's monitor, breaking the lock order that `forcedInfo` keeps on the same monitor. A hang the claims cannot see. Every `@Foo(...)` in a source has this form, and annotations are not forced before the bodies (O3). It takes the claim first.

**A2. `BaseTypeSeq.maxDepth`** as a lazy val of a shared sequence has an initialiser that can complete a class path existential's bound. It is a plain cache.

**A3. `BCodeHelpers.withoutReporting`** had the swap of R4. No loss was provoked: Java source members are complete by the jvm step, so the swap only happens for a class first loaded that late, and diagnostics in that step are few.

**A4. `CompilationUnit._depends`** took concurrent `HashSet` additions from foreign completions, on the path of every identifier. Nothing reads it, so only a failure of the set could show. A monitor guards it.

## Plausible and unreproduced

Not fixed. Ranked by consequence and likelihood. "Tried" means a corpus was written for it and did not provoke it.

- **P1. Wait cycles in `isCyclicOrErroneous`** (`Implicits.scala:342`). `isLockedHere` is outside the `try`, so a wait cycle's `CyclicReference` can leave the search as a false error where a serial nesting skips the candidate. Inside the `try`, the same cycle caches "erroneous" for the run on a symbol that is fine, and an `ImplicitInfo` held on a namer context is shared, contrary to the comment at line 253. Tried: `corpus/implicit-cycle`, valid, 0 of 200. The fix is small if it is ever seen.
- **P2. `-Ybackend-parallelism` above 1.** The handler's threads never set the per-thread phase, so a `LazyVar` first read in post-processing, or a `ClassBType.pendingInfo` closure run there, would read symbols at `NoPhase`. It is not rejected by `unsupported()`, and sbt projects set it. Tried: `late-phases` 0 of 20 and library 0 of 3 with the flag, so the reading is not borne out, and why not was not established. If the combination stays unsupported in practice, rejecting it there is the cheap answer.
- **P3. Computing a class's info twice is not idempotent for a symbol that fails to complete.** `completeSilentlyAndCheckErroneous` answers true for the first thread and false for the second, which then asserts on `NoSymbol`, reported as "Error while emitting". Needs a broken class path (scala/bug#9111).
- **P4. A false wait cycle in jvm.** The coarse root held by a `LazyVar` initialiser, wanting a Java source member's claim, whose holder wants the root for a class load. The shape of I19. Forcing Java members before the jvm step would remove it.
- **P5. `Scope`'s `ElemsCache` across a mid-chain `unlink`.** It is keyed by the head, and an `unlink` does not change the head, so a list computed across one is cached as current. `Erasure.scala:645`, `Mixin.scala:238` and `SpecializeTypes.scala:611` run on workers. A reader of the same class's declarations in the window sees a removed member.
- **P6. `lookupNextEntry` crossing `createHash` on ARM.** Nothing orders the load of `hashtable` before the load of `e.tail`, so a lookup found through the list can stop early once the table appears. A class crossing 8 members during `addSyntheticMethods` could lose alternatives for one lookup.
- **P7. A scope cloned by an unlocked transformer misses a rename.** `ExplicitOuter:175` and `SpecializeTypes:1337` clone while `makeNotPrivate` renames, and the clone is not yet in `owner.infos`, so the entry stays in the old bucket. A by-name lookup then fails.
- **P8. `PermittedSubclassSymbols.symbols`** is a lazy val whose initialiser types the `permits` clause, under the attachment's monitor. The lock order of A1, Java sources only.
- **P9. A member is entered before its info is set.** `assignAndEnterFinishedSymbol` does `enterInScope`, then `setInfo`. A default getter entered by a foreign completion is visible for that window with `infos` null. A reader iterating a class's members and reading their infos would fail an assertion. Those who iterate on the pool filter by flag or name first, as far as I followed them.
- **P10. Global error counts read as if per unit.** `Macros.scala:818` (a macro fails if any thread reported during it), `SyntheticMethods.scala:81` and `:438`. Failing compiles only: differing diagnostics.
- **P11. `constrParamss` copies a constructor another thread is typing** (`Unapplies.scala:88`), which can leave a stale local symbol in a synthetic default. Narrowed by R1, which moves most of those completions before the bodies.
- **P12. `PostProcessor.caseInsensitively`** keeps the first arrival, so which class gets "differs only in case" follows thread timing.
- **P13. Interrupting the driver** in `onPool` restores the reporter with workers still running, and has one narrow lock inversion between the buffer's monitor and `PerRunReporting`'s. Cancelled compiles only.
- **P14. `AsyncPhase.awaits`** is a plain set written from refchecks' workers under `-Xasync`, a handful of entries.
- **P15. `rawatt` on a symbol is a plain read-modify-write.** One cross-thread writer can be constructed: `UnCurry.scala:155` on a `@varargs` method. Publication is fine.
- **P16. `tpeStamped` after `info_=`.** A reader computing from the old info can store after `info_=` cleared it. It matters only for a symbol whose type parameters change, which is the class completions that readers wait for.
- **P17. `Symbol.isLess` breaks ties by id**, and ids of class path classes follow load order. Two unrelated traits with base type sequences of one length, as parents of an inferred lub, could flip. Not constructed; the plan keeps top-level ids stable only.
- **P18. `VirtualFileWriter`'s lock covers writers only.** Needs an output directory that is also on the class path.
- **P19. `SuperAccessors`'s unlink and enter of a `private[C]` member** leaves it absent for a moment, and a pickler fused with superaccessors could pickle either name, which is why the post-typer phases are separate pool steps. Noted in the plan as a residue.
- **P20. `ModuleTypeRef.narrow`** reads its cache twice around `invalidateTypeRefCaches`, the shape of the `normalize` fix. With R1 the only invalidations on the pool are local companions, whose types no other thread has.
- **P21. `currentFreshNameCreator` is not reset after a macro** and is read by the TASTy reader (`ContextOps.scala:294`); `Enclosures.enclosingUnit` is the demanding thread's unit in a foreign completion. Timing-dependent names, only with `-Ytasty-reader` or a macro reading the deprecated API.
- **P22. Each `Duplicator` takes per-thread slots for good.** Memory, not a race: growth by specialized body in a resident compiler.
- **P23. Warnings from info transforms** go to the unit of whichever thread forced the class, so their position in the output can vary. The set is the same.

## Benign

Written without protection, and harmless for a stated reason: `Contexts.savingEnclClass` on a shared context (no cache is written from the transient state, since `isAccessible` returns on `ThisType` first); `SubComponent.ownPhaseCache` and its run id (only a kept `Global`, and the previous run's phase has the same id); `Reporter._cancelled`, `seenMacroExpansionsFallingBack`, `lastSeenSourceFile`, `Scopes.scopeCount`, `Implicits.useCount*` (no reader), and `Duplicator.currentOwner` on the shared `duplicator`, whose only use is a choice between two equal results.

## Checked, and protected though it looks otherwise

`rawToExistential` is a `def`, a new map per call. `Transforms.Lazy` would be the two-field race, but `Global` overrides the accessors. `ConstructorDefaultsAttachment`'s append runs at enter, and the worker that drains it waits for the module first. `Scope.createHash` in a nested scope writes tails that depend only on older entries. An unlocked `specializeClass(nothingToDo = true)` writes one idempotent set under a monitor. `Mixin.scala:547`'s `sortBy(_.id)` compares symbols of one class made in sequence. `indyLambdaImplMethods`, `InnerClasses` order and the `symData -=` race are sorted or per class. `pendingInfo` publication pairs with the writer's monitor.

## Notes on the plan

`unlink` in the middle of a chain is not serial, see P5. The plan's "`_annotations` read-modify-write ... noted, not seen" stands, with R2 as a cross-unit case that is closed.

## Verification

Class-file oracle with every pool switch on and `checkScratchOwner`, 3 parallel runs at 8 threads each: reflect, compiler and library do not differ from the one-thread run. Against serial, reflect is identical, the compiler differs in the 11 fresh-name files the plan lists, and the library in the 3 class files of O1.

Stress at 8 threads with every pool switch, class files compared, 200 runs each: `sealed-java`, `serializable-companion`, `serializable-companion-neg`, `long-names`, `sealed-local`, `spec-parents`, `bridge-annotations`, `case-accessors`, `implicit-cycle`, `late-phases`, `post-typer`, `stress-shared`, `case-classes`, `annotations`, `name-clash`, none differ; reflect 100 runs, none differ. Pekko, scalap and re2s were not rerun with these fixes.

JUnit `scala.tools.nsc.typechecker.*`, `scala.reflect.internal.*`, `scala.tools.nsc.symtab.*`, `DeterminismTest` pass. `ForceAnalyzerLazyValsTest` finds `caseObjectSymbols` on the superclass of `analyzer.getClass`, as the field is on `AbstractAnalyzer` since scala/scala#11367. Partest was not rerun: every change is behind `isConcurrentCompiler` except `withReporter`'s serial branch, which is the old code, and the plain cache of `maxDepth`.

Timing was not measured. The new monitors are on slow paths, and `Global.reporter` takes one test and, in a concurrent compiler, a scratch lookup per read.

## Classification

Every mutable field, collection and lazy val with a non-trivial initialiser that the workers reach, from reading its callers. R, A and P refer to the sections above. A blank reason means the class says it.

| area | location | state | class | why |
|---|---|---|---|---|
| symbols | Symbols.scala:265 | `_rawflags` | claim or monitor | compare-and-set for every read-modify-write; blind stores by the creator, and `Namers.scala:1843` under the symbol's mark |
| symbols | Symbols.scala:235 | `_rawname` | serial step or confined to a unit | a rename is in its own unit's step or under `mixinClaim`; see P7 and P19 |
| symbols | Symbols.scala:264 | `_rawowner` | confined to a unit | `owner_=` on the unit's local symbols |
| symbols | Symbols.scala:275 | `_validTo` | claim or monitor | compare-and-set when advancing; publication with `infos` as in the log |
| symbols | Symbols.scala:1521 | `infos`, `TypeHistory` | claim or monitor | new node behind a fence or compare-and-set; `updateInfo` (line 1901) is a plain write, used on local or just-made symbols |
| symbols | Symbols.scala:1625 | `_completingThread` | claim or monitor | compare-and-set, a set of marks |
| symbols | Symbols.scala:1506 | `_privateWithin` | immutable after a point | written by the creator or under a completion |
| symbols | Symbols.scala:2376 | `_annotations` | confined to a unit, one read | writers are the symbol's claim or its unit; `Erasure:602` fixed, R2 |
| symbols | Symbols.scala:1351 | `javaBinaryNameStringCache` | unprotected, benign | a `String`, idempotent |
| symbols | Symbols.scala:2971 | `isOverridingSymbolCache` | unprotected, benign | one `Int`, first answer kept; the members that typing adds override `Any` or `Product` anyway, so the answer does not change |
| symbols | Symbols.scala:3379 | `_referenced` | confined to a unit | alias set after the join, the rest own unit |
| symbols | Symbols.scala:3503, 3908 | `flatname` | unprotected, benign | an interned name, idempotent since R7 |
| symbols | Symbols.scala:3530 | `mtpe*` | not reached | a concurrent compiler uses `Scratch.memberTypes` |
| symbols | Symbols.scala:3679 | `tyconCache`, `tyconRunId` | unprotected, benign | read once; an old value against a new run id is possible only with a kept `Global` |
| symbols | Symbols.scala:3683 | `tpeStamped` | immutable holder | stale value after `info_=` possible only for a symbol whose type parameters change, P16 |
| symbols | Symbols.scala:3909 | `_associatedFile` | claim or monitor | written by loaders and the namer |
| symbols | Symbols.scala:3910 | `thissym` | serial step | class headers |
| symbols | Symbols.scala:3914, 4134 | `thisTypeStamped`, `typeOfThisStamped` | immutable holder | fixed in the log |
| symbols | Symbols.scala:4062 | `childSet` | monitor | fixed, R5 |
| symbols | Symbols.scala:4137 | `implicitMembersCache` | immutable holder | |
| types | Types.scala:100 | `explainSwitch` | unprotected, benign | `-explaintypes` only: another thread's check may print its explanation |
| types | Types.scala:1443, 1530, 2591, 3033, 3115 | `trivial`, `isdepmeth` | unprotected, benign | idempotent |
| types | Types.scala:1452, 1633-34, 2402, 2619-20 | `*Stamped` caches | immutable holder | value and period as one object |
| types | Types.scala:2013 | `RefinedType.normalized` | unprotected, benign | never cleared; two threads can make two equal refinements |
| types | Types.scala:2122, 2129 | `refs`, `state` | claim or monitor | coarse root |
| types | Types.scala:2316 | `ModuleTypeRef.narrowedCache` | unprotected | read twice around `invalidateTypeRefCaches`, P20 |
| types | Types.scala:2621 | `TypeRef.normalized` | claim or monitor | volatile, read once |
| types | Types.scala:139 | `_intersectionWitness` | monitor | |
| types | Types.scala:4571 | `uniques` | monitor | a stripe each |
| reflect | StdNames.scala:51 | `compactify.md5` | monitor | fixed, R7 |
| reflect | StdNames.scala:40 | `KeywordSetBuilder.kws` | serial step | used during `nme`/`tpnme` construction |
| reflect | StdNames.scala:1186,1208,1338 | `typeNames`, `termNames`, `sn` lazy | claim or monitor | trivial init under the cake's monitor, no completion |
| reflect | Names.scala:44 | `_chrs` | claim or monitor, benign race | writes under `nameLock`, reads check length, old array frozen |
| reflect | Names.scala:55 | `nc` | claim or monitor | only read and written under `nameLock` in a concurrent compiler |
| reflect | Names.scala:59,62 | `termHashtable`, `typeHashtable` | claim or monitor for writes, lock-free reads | prepend only; final fields; miss retries under lock |
| reflect | Names.scala:215 | `lookupTypeName` | immutable after a point | the assertion needs the name made before; back end only, names exist by then (not traced to every caller) |
| reflect | Scopes.scala:25 | `scopeCount` | unprotected, benign | statistic only. Written by every thread per scope made, so a cache-line cost of the kind `Scratch.nodeCount` was moved to avoid |
| reflect | Scopes.scala:42,46 | `ScopeEntry.tail/next` | claim or monitor for writes | published after the fence. for the switch of chain |
| reflect | Scopes.scala:39 | `ScopeEntry.sym` | serial step or claim | written in place only by `enterBefore` |
| reflect | Scopes.scala:95,103 | `elems`, `hashtable` | claim or monitor for writes | monitor, dependent-load readers |
| reflect | Scopes.scala:99 | `nestinglevel` | immutable after a point | set before publication |
| reflect | Scopes.scala:111 | `elemsCache` | unprotected against a mid-chain unlink | P5 |
| reflect | BaseTypeSeqs.scala:54 | `elems` contents | benign race | two threads may evaluate one element, to `=:=` types; the plan says so |
| reflect | BaseTypeSeqs.scala:58 | `typeSymbols` | immutable after a point |  |
| reflect | BaseTypeSeqs.scala:75 | `pending` | not reached | serial only; per-thread list when concurrent |
| reflect | BaseTypeSeqs.scala:196 | `maxDepth` lazy | no monitor | fixed by argument, A2 |
| reflect | BaseTypeSeqs.scala:215 | `undetBaseTypeSeq` lazy | claim or monitor | trivial |
| reflect | AnnotationInfos.scala:139 | `CompleteAnnotationInfo.orig` | immutable after a point | set by the creator inside the lazy; published behind final fields' barrier |
| reflect | AnnotationInfos.scala:345 | `rawpos` | immutable after a point | same |
| reflect | AnnotationInfos.scala:160,162 | `_forced`, `forcedInfo0` | claim or monitor | lock before monitor. for the retry |
| reflect | AnnotationInfos.scala:174 | `claimSym` | claim or monitor | volatile, check-then-set |
| reflect | AnnotationInfos.scala:193 | `typeSymbol` lazy | claim, then monitor | fixed by argument, A1 |
| reflect | AnnotationInfos.scala:343 | `isTrivial` lazy | latent |  |
| reflect | AnnotationCheckers.scala:91 | `annotationCheckers` | serial step | plugins rejected under the flag |
| reflect | Definitions.scala:194 | `isInitialized` | serial step |  |
| reflect | Definitions.scala:98-, 1716- | `definitions`/`RunDefinitions` lazy vals | immutable after a point | `forceLazyVals`; a lazy val that throws in the sweep is retried on a worker under the monitor, only for a required class that is missing, when the compile is lost anyway |
| reflect | Definitions.scala:1013 | `samCache` | claim or monitor | read, compute outside, store |
| reflect | Definitions.scala:668 | `VarArityClass` | immutable after a point | vals |
| reflect | Trees.scala:48 | `Tree.id` via `Scratch.nodeCount` | per thread | ids repeat across threads; diagnostics only |
| reflect | Trees.scala Tree `tpe`/`symbol` | per-unit | confined to a unit | singletons: `CannotHaveAttrs` ignore writes. Tree contents shared via attachments are under typer below |
| reflect | Trees.scala:1903 | `duplicator.currentOwner` | unprotected, benign | see above |
| reflect | Trees.scala:1796 | `EmptyTreeTypeSubstituter` | immutable after a point | as above, `currentOwner` only |
| reflect | StdAttachments.scala:24 | `rawatt` | unprotected read-modify-write, not found reachable |  |
| reflect | StdAttachments.scala:179 | `PermittedSubclassSymbols.symbols` lazy | unprotected (lock order) | P8 |
| reflect | StdAttachments.scala:180,181 | `children`, `completing` | per thread | fixed, R6 |
| reflect | Positions.scala:375,377 | `posAssigner`, `posAssigners` | per thread | `perThread`; the lazy's init is trivial |
| reflect | Mirrors.scala:30 | `initialized` | serial step |  |
| reflect | Mirrors.scala:306-338 | roots lazy | serial step | forced at `init` |
| reflect | SymbolTable.scala:228 | `phStack` | serial step | `keepPhaseStack`, `-Ylog` rejected |
| reflect | SymbolTable.scala:229,230 | `ph`, `per` | immutable after a point | markers once `phasePerThread` |
| reflect | SymbolTable.scala:269 | `_flatClassesSeen` | benign race | a thread that needs it true wrote it |
| reflect | SymbolTable.scala:473 | `perRunCaches.caches` | claim or monitor | `recordCache` under monitor; `clearAll` between runs |
| reflect | SymbolTable.scala:595,649 | `completionLock`, `localCompletions` | immutable after a point | trivial lazy |
| reflect | SymbolTable.scala:640 | `PerThread`, `perThreadSlots` | per thread | atomic slot |
| reflect | SymbolTable.scala:659-776 | `Frame` | per thread | hung off `Scratch.frame` |
| reflect | SymbolTable.scala:781-880 | `Scratch` | per thread | fetched into locals; I saw no store in a field in this scope |
| reflect | SymbolTable.scala:888-890 | `oneScratch`, `perThreadScratches` | immutable after a point | under `firstScratch`, final fields |
| reflect | Types.scala:4550 | `uniques` | claim or monitor | stripes each under a monitor, `uniqueRunId` per stripe |
| reflect | Variances.scala:271 | `varianceInType` | per thread | `Scratch.varianceInTypes` |
| reflect | Variances.scala:55,177 | validator maps | confined to an instance | per `VarianceValidator` |
| reflect | ExistentialsAndSkolems.scala:45 | `skolemizationLevel` | per thread | `Scratch` |
| reflect | FreshNames.scala:23 | `globalFreshNameCreator` | not reached | used only by `JavaUniverse` |
| reflect | util/FreshNameCreator.scala | `counters` | claim or monitor | `ConcurrentHashMap` of `AtomicLong`, per unit |
| reflect | util/IdCounter.scala | `last` | claim or monitor | atomic. Order: "Known and open" |
| reflect | util/SourceFile.scala:183 | `lineIndices` lazy | immutable after a point | trivial, no symbols |
| reflect | util/SourceFile.scala:206 | `lastLine` | benign race | hint only, same file |
| reflect | util/Statistics.scala:148,219,264 | counters, timers | not reached | statistics rejected under the flag; all calls guarded |
| reflect | util/WeakHashSet.scala |  | claim or monitor | only via `uniques` |
| reflect | util/ReusableInstance.scala | `taken`, `cache` | per thread | every instance now lives in `Scratch` or `perThread` |
| reflect | util/TraceSymbolActivity.scala |  | not reached | off unless `scalac.debug.syms` |
| reflect | tpe/TypeMaps.scala:33-74,920,1047,1052,1175 | object maps | immutable after a point | no state |
| reflect | tpe/TypeMaps.scala:285 | `rawToExistential` | per call | a `def` |
| reflect | tpe/TypeMaps.scala:401 | `object wildcardExtrapolation` | not reached | variance state, but all callers in `Definitions` and `Typers` use `scratch.wildcardExtrapolation` |
| reflect | tpe/TypeMaps.scala:1095 | `findInTree` lazy | confined to an instance | on `ContainsCollector` |
| reflect | tpe/GlbLubs.scala | `lubs`, `glbs`, `globalGlbDepth`, collectors | per thread | `Frame`, `Scratch` |
| reflect | tpe/TypeComparers.scala | `pendingSubTypes`, `knownFalse`, recursions, `skolemizationLevel` | per thread | `Frame`, `Scratch` |
| reflect | tpe/TypeConstraints.scala:30 | `UndoLog` | per thread | per `Frame`; registered per run under `recordCache`'s monitor |
| reflect | tpe/TypeConstraints.scala:76,77 | `numericLoBound/HiBound` lazy | immutable after a point | forced by `$reflect$internal$` accessor sweep |
| reflect | tpe/FindMembers.scala | instance fields | per thread | per `FindMember` in `Scratch` or per call. Reads `decls.elems` directly, so not |
| reflect | tpe/CommonOwners.scala | `CommonOwnerMap.result` | per thread | `Scratch.commonOwnerMaps` |
| reflect | tpe/TypeToStrings.scala | recursion state | per thread | `Scratch` |
| reflect | transform/Transforms.scala:30-40 | `Lazy.value/_isDefined` | not reached | `Global` overrides the accessors. A real two-field race if anything ever forces it in a parallel compiler |
| reflect | transform/Erasure.scala:530-585 | erasure map objects | immutable after a point | no state; `boxPrimitives` moved to `Scratch` |
| reflect | transform/UnCurry.scala:48,89 | `uncurry`, `uncurryType` | immutable after a point | no state. `:155` writes an attachment under the class transform's lock () |
| reflect | transform/PostErasure.scala:25 | `elimErasedValueType` | immutable after a point | no state |
| reflect | pickling/UnPickler.scala:56 | `completingStack` | claim or monitor | classpath symbols complete under the coarse root (I7) |
| reflect | pickling/UnPickler.scala:59-72 | `Scan` fields, `readIndex`, `entries`, `symScopes` | claim or monitor | same |
| reflect | Reporting.scala:42 | `supplementedError` | benign race | a crash message supplemented twice |
| reflect | Reporting.scala:91,92 | `_errorCount`, `_warningCount` | claim or monitor | `BufferingReporter` overrides under its monitor |
| reflect | Phase.scala:36 | `nx` | serial step | built at `Run` start |
| reflect | InfoTransformers.scala:25,26 | `prev`, `next` | serial step | built at `Run` start |
| reflect | sharing/Stamped, PerKeyClaims | callers in scope | OK | only `AnnotationInfos` (`withCompletionClaim`) calls in this scope, 's note aside; `Stamped` is used from `Symbols`/`Types` only |
| typer | `SyntheticMethods.scala:61` | `renamedCaseAccessors` | monitor | fixed, R9 |
| typer | `reflect/macros/contexts/Parsers.scala:29` | write to `global.reporter` | per thread | fixed, R4 |
| typer | `Implicits.scala:1098` | peek at `isInitialized` | setting | fixed, R8 |
| typer | `Implicits.scala:312, 343` | `isErroneousCache`; `isLockedHere` outside `try` | unprotected | P1 |
| typer | `Macros.scala:818`, `SyntheticMethods.scala:81, 438` | global error count | unprotected (diagnostics) | P10 |
| typer | `Unapplies.scala:88` | foreign read of a tree being typed | unprotected | P11 |
| typer | `CompilationUnits.scala:74` | `_depends` | monitor | fixed by argument, A4 |
| typer | `SuperAccessors.scala:244-246` | unlink and rename | unprotected under a flag | P19 |
| typer | `Contexts.scala:233` | `enclClass` save and restore | unprotected (benign) | see "Benign" |
| typer | `Implicits.scala:310-311` | `tpeCache`, `depolyCache` | per thread, or shared via a namer context | idempotent |
| typer | `Implicits.scala:354-355` | `useCount*` | unprotected, dead | no reader |
| typer | `Symbols.scala:4074` via `Namers.scala:1232` | `childSet` | monitor | fixed, R5 |
| typer | `Contexts.scala:55, 64, 73, 1360` | start context, in-progress set, access details, lookup | per thread | `perThread` |
| typer | `Contexts.scala` | root import caches, the mask's candidates | monitor | computed outside it |
| typer | `Contexts.scala:1026-27` | `implicitsCache`, run id | shared; cache before id | a lost update only recomputes |
| typer | `Contexts.scala:231-285, 432, 463` | other per-context fields | confined to the context's typing | namer contexts read-only, except `importSig` |
| typer | `Implicits.scala:256-261, 2017` | search caches, shadower | per thread |  |
| typer | `Implicits.scala:265, 440` | `recordCache` clearers | per thread | see Low list |
| typer | `Implicits.scala:266` | `implicitSearchId` | atomic |  |
| typer | `Implicits.scala:290-299` | failure sentinels (lazy) | immutable after force | `forceLazyVals` |
| typer | `Infer.scala:145` | `instantiate` | per thread |  |
| typer | `Infer.scala:208-209, 1592` | lazy error symbols, `topTypes` | immutable after force |  |
| typer | `Macros.scala:303` | `macroImplBindingCache` | completion lock |  |
| typer | `Macros.scala:505-511` | macro state | per thread |  |
| typer | `Macros.scala:65`, `FastTrack.scala:57` | fast track | immutable after force; benign value+stamp | see "Benign" |
| typer | `reflect/macros/runtime/MacroRuntimes.scala:39, 55` | runtimes, classloader | completion lock | only from `macroExpand` |
| typer | `reflect/macros/contexts/*` | per context lazies | confined to an expansion | one at a time |
| typer | `reflect/macros/contexts/Enclosures.scala:51` | `currentUnit` | per thread, wrong for foreign | see "Benign" |
| typer | `reflect/macros/compiler/*` | resolver lazies | confined to an instance |  |
| typer | `Namers.scala:67, 1744` | `innerNamer`, `ownerNamer` | per namer lazy | init completes nothing |
| typer | `Namers.scala:614, 905-906, 1984-88` | `setAnnotations` | under the symbol's claim |  |
| typer | `Namers.scala:676, 680, 1534` | `unlink` | scope monitor |  |
| typer | `Namers.scala:684, 1525, 1562` | attachments on a symbol | under its claim | no other writer |
| typer | `Namers.scala:769-783` | `javaClassesByUnit`, permitted | serial prefix (Java headers) |  |
| typer | `Namers.scala:783, 797, 802` | attachments on companions | enter, serial or local |  |
| typer | `Namers.scala:1285-93` | `cda.defaults`, namer | serial prefix or local | checked above |
| typer | `Namers.scala:2207` | `okParams` | per instance |  |
| typer | `NamesDefaults.scala:36, 45, 50` | attachments' sets | under a claim or local |  |
| typer | `Typers.scala:49` | right-assoc state | per thread |  |
| typer | `Typers.scala:58, 2255, 6641` | `superConstructorCalls` | monitor on write; read after join | `Analyzer.scala:175` |
| typer | `Typers.scala:126-139` | fresh creators | monitor; per member | deterministic |
| typer | `Typers.scala:148` | `transformed` | concurrent map |  |
| typer | `Typers.scala:284` | `namerCache` | per typer; race only recreates |  |
| typer | `Typers.scala:467` | `setInfo(NoType)` on a stale symbol | unprotected, IDE-style case | a classfile whose source is in this run |
| typer | `Typers.scala:2183, 2378, 2412-21, 2526` | annotations of own symbols | unit's thread; one writer | readers see a list |
| typer | `Typers.scala:2498` | field attachment | unit's thread | the writer is under the claim that was awaited |
| typer | `TypersTracking.scala:66-67` | `typingStack` | unsupported flag | `-Ytyper-debug` rejected |
| typer | `TypeDiagnostics.scala:59` | `addendums` | monitor |  |
| typer | `TypeDiagnostics.scala:61`, `Symbols` shown names | side channels | per thread |  |
| typer | `TypeDiagnostics.scala:425ff, 522ff` | per message or traverser | confined |  |
| typer | `Unapplies.scala` | no state |  | and are reads |
| typer | `ConstantFolder`, `Checkable`, `Tags`, `Adaptations`, `EtaExpansion`, `PatternTypers:207` | none, or per call | confined |  |
| typer | `StdAttachments.scala:258-265` | `cached` | macro annotations, unsupported |  |
| typer | `ImportTracking.scala:30-31` | used selectors | monitor |  |
| typer | `RefChecks.scala:74` | `separatelyCompiledScalaSuperclass` | concurrent |  |
| typer | `RefChecks.scala:87-98, 1028-29` | transformer state, `symIndex` | per unit instance |  |
| typer | `RefChecks.scala:258-59, 1710, 2060, 2085` | writes to symbols | own unit's classes |  |
| typer | `SuperAccessors.scala:89-90, 418` | transformer state | per unit |  |
| typer | `Duplicators.scala:57-67` | owners, `invalidSyms` | per instance, per call | `SpecializeTypes.scala:1885, 1944` |
| typer | `SplainData.scala` `ImplicitErrors` | stack | unsupported flag | `-Vimplicits` rejected |
| typer | `AnalyzerPlugins.scala:319, 412` | plugin lists | immutable after a point | plugins rejected |
| typer | `Analyzer.scala:102` | `deferredOpen` | serial (packageobjects) |  |
| typer | `CompilationUnits.scala` `synthetics`, `transformed`, `toCheck` | concurrent or monitor |  | residual as in the plan |
| typer | `CompilationUnits.scala` `body`, `_firstXmlPos`, `fresh` | unit's thread; `fresh` atomic counters |  |  |
| typer | `Reporting.scala` `PerRunReporting` | monitor on `this` | rendered outside |  |
| typer | `Reporting.scala:461` | `seenMacroExpansionsFallingBack` | plain sticky; read after join | benign |
| typer | `ast/TreeGen.scala`, `ast/Trees.scala` | none, or locals; `typerHere` |  |  |
| typer | `ast/parser/SyntaxAnalyzer.scala` | parser state | confined; off by default | in the code's own list |
| typer | `settings/` | no write during a run reachable with the flag |  | `MacroAnnotationNamers:698` is rejected; `Parsers.scala:386` is script mode |
| transforms | SpecializeTypes:84 | `specializedClass` | reader now forces the class | fixed, R3 |
| transforms | SpecializeTypes:90, `:191` | `overloads`, `::=` | claim or monitor | class members under coarse root; tree path `:1917` for a local method of the unit, fresh key |
| transforms | SpecializeTypes:93 | `info` | claim or monitor | written in a transform under the root or at `:979` (fresh key); values immutable; `degenerate` lazy read only under the root |
| transforms | SpecializeTypes:96, `:707` | `typeEnv`, `m(k) = f(m(k))` | claim or monitor | fresh symbols, under root |
| transforms | SpecializeTypes:100 | `anyrefSpecCache` and inner map | claim or monitor | all three uses (`:526`, `:539`) under the root |
| transforms | SpecializeTypes:105, `:1014` | `wasSpecializedForTypeVars` | claim or monitor | fresh key, under root |
| transforms | SpecializeTypes:108 | `concreteSpecMethods` | claim or monitor | own monitor at all sites; contended (every concrete method of every class) |
| transforms | SpecializeTypes:386 | `specializableTypes` lazy | immutable after a point | forced at the step; can first run in uncurry's step via `isSpecializedIn` (`UnCurry:98`), initialiser touches completed classes only |
| transforms | SpecializeTypes:213 | `info.foreach`, flags | serial step | after `super.run()` on the driver |
| transforms | SpecializeTypes:631 | `currentRun.symSource` | claim or monitor | concurrent under the flag (`Global:1352`) |
| transforms | SpecializeTypes:754 | `ConstructorNeedsFence` on `clazz` ctor | claim or monitor | under the root; read in the Constructors step |
| transforms | SpecializeTypes:762, `:1008`, `:863` | flags of members | claim or monitor | under root, flag CAS |
| transforms | SpecializeTypes:894 | `updateInfo` | claim or monitor | only with non-empty `outerEnv`; plain write against a CAS, but a term's specialize step publishes no node, so none is lost |
| transforms | SpecializeTypes:608-616, `:911-913` | unlink of a bytecode `Foo$mcI$sp`, `setInfo(NoType)`, enter in package scope | under claim/monitor; readers see a transient miss | window across `toSpecializedClass`; no by-name reader of those names found on another thread; pickles have no `$sp` |
| transforms | SpecializeTypes:1143, `:1134` | `clazz.info.decls.enter(om)` | claim or monitor | root plus scope monitor |
| transforms | SpecializeTypes:1521 | `makeNotPrivate` from `ImplementationAdapter` | confined to a unit, F3 | private members of the class, companion or outer |
| transforms | SpecializeTypes:2025, `:2044` | `enter` in `sClass`'s scope | confined to a unit | own class, private accessor or `SPECIALIZED_INSTANCE`, not looked up elsewhere by name in the step |
| transforms | SpecializeTypes:1564, `:1567` | `body`, `parameters` | per thread | per transformer |
| transforms | Duplicators:57-59, `:67` | `oldClassOwner`, `newClassOwner`, `envSubstitution`, `invalidSyms` | per thread | a `Duplicator` per call (`:1885`, `:1944`, Constructors:378), see |
| transforms | Erasure:36 | `requiredDirectInterfaces` | claim or monitor | `addRequiredDirectInterface` under monitor; immutable values; read by predicate only, at jvm |
| transforms | Erasure:58 | `NeedsSigCollector` | per thread | new instance under the flag |
| transforms | Erasure:483, `:486` | `poundPoundMethods`, `interceptedMethods` | immutable after a point | forced by `forceLazyValsOf` |
| transforms | Erasure:491 | `erasureAnalyzer` | immutable after a point | module forced with the component; its state is the analyzer's (per-thread holders), under typer above |
| transforms | Erasure:501, `:635` | `bridgeTarget`, `bridges` | per thread | per `EnterBridges` |
| transforms | Erasure:602 | read `member.annotations` | filtered read | fixed, R2 |
| transforms | Erasure:621 | `exitingErasure(root.info.decls enter bridge)` | confined to a unit | readers filter by `isArtifact`/`BridgeFlags` |
| transforms | Erasure:913 | `low setInfo ErrorType` | confined to a unit | error path |
| transforms | Erasure:1382 | `tree1.symbol.setInfo(deconst)` | confined to a unit | private field; wipes history with a fresh node, as `info_=` |
| transforms | Erasure:1388 | `dropFieldAnnotationsFromGetter` | filtered read on the other side | fixed, R2 |
| transforms | reflect transform/Erasure:565-576 | `boxPrimitives` | per thread | `scratch` |
| transforms | reflect transform/Erasure:103 | erasure maps of runtime reflection | per thread | new instance outside the compiler |
| transforms | reflect transform/UnCurry:89-111, `:155` | class path: clone scope, forwarder, attachment | claim or monitor | class goes to the root (UnCurry:73) |
| transforms | reflect transform/Transforms:28 | `Lazy` | not reached in a compiler | see above |
| transforms | Mixin:69 | `treatedClassInfos` | claim or monitor | under `mixinClaim(clazz)` at both sites |
| transforms | Mixin:58, `:233-238` | rename, `unlink` in a trait | claim or monitor | claim of the trait; other classes read after their own `publicizeTraitMethods(mc)` |
| transforms | Mixin:169 | `clazz.info.decls enter` | claim or monitor | claim of `clazz`; superclass completed first |
| transforms | Mixin:168 | `removeAttachment` | confined to a unit | fresh clone |
| transforms | Mixin:221 | `symTparam.name =` | confined to a unit | fresh clones, owner not a class |
| transforms | Mixin:427 | `unit.registerDependency` | per thread | the calling unit; which unit records a superclass's traits depends on the order, read by nothing in the class files |
| transforms | Mixin:448, `:506`, `:543` | `nullables`, `usedIn`, `map` | per thread | per transformer; order settled by `:547` |
| transforms | Mixin:645 | `NeedStaticImpl` on a trait method | confined to a unit | read later (jvm); a clone removes it (`:168`) |
| transforms | AccessorSynthesis:121, `:122` | `_bitmapInfo`, `_slowPathFor` | claim or monitor | one write per key under the Fields transform; values immutable |
| transforms | AccessorSynthesis:116 | `BitmapInfo.storageClass` lazy | confined to a unit | per instance, tree transform only |
| transforms | AccessorSynthesis:203 | `groupBy` by category `Name` | immutable after a point | the four `nme` names exist before the pool, so hash order is fixed |
| transforms | Fields:214 | `moduleOrLazyVarOf` | claim or monitor | written under the root |
| transforms | Fields:279, `:340`, `:358`, `:366` | writes on a trait's members | claim or monitor | class path takes the root (`:90`), in that scope order |
| transforms | Fields:135 | ctor attachment | claim or monitor | own class, under the root |
| transforms | Fields:143 | `checkAndClear` | confined to a unit | own class's scope; a clone in a subclass sets the flag itself (`:124`) |
| transforms | UnCurry:81-85, `:145` | transformer's fields | per thread | per transformer |
| transforms | UnCurry:411 | `setFlag SYNCHRONIZED` | confined to a unit | read in a later step |
| transforms | UnCurry:733 | `p.symbol updateInfo` | confined to a unit | original parameter of a curried method; plain write against a possible CAS replaces a node of the same step; benign |
| transforms | LambdaLift:66-116 | maps | per thread | linked, per transformer |
| transforms | LambdaLift:44, `:49` | `refCreateMethod`, `refZeroMethod` | immutable after a point | forced at the step |
| transforms | LambdaLift:186, `:213`, `:356`, `:414`, `:479`, `:484` | flags, `updateInfo`, `owner =`, `enterUnique` | confined to a unit | local symbols or the unit's class |
| transforms | LambdaLift:303 | ctor attachment of a local trait's children | confined to a unit | children of a local trait |
| transforms | ExplicitOuter:175-190 | new symbols, cloned scope | per thread until published | for a rename meanwhile |
| transforms | ExplicitOuter:386-387, `:441`, `:446` | flags, rename | confined to a unit, except `:446`: flag CAS, idempotent, may be on another unit's protected member |  |
| transforms | Flatten:29-47 | package scope, `LIFTED` | claim or monitor | class path takes the root (`:117`); `:147-148` own module without the root, scope monitor, name unique |
| transforms | Flatten:124 | `liftedDefs` | per thread | per `Flattener`, despite `perRunCaches` |
| transforms | Constructors:34, `:35` | `guardedCtorStats`, `ctorParams` | confined to a unit | a specialized class is in its generic class's unit (`implSpecClasses`), after it in the stats |
| transforms | Constructors:373, `:804`, `:811` | flag, attachment, `unlink` | confined to a unit | own class; omittable only for private[this] or a final class's outer accessor, not read by name from other units |
| transforms | CleanUp:31 | `entryPoints` | claim or monitor | monitor; `getEntryPoints` sorts |
| transforms | CleanUp:40-41, `:65` | statics, `localTyper` | per thread | per transformer |
| transforms | Delambdafy:71, `:137`, `:197`, `:238`, `:279`, `:295` | caches, `enter`, flags | per thread / confined to a unit | own class |
| transforms | TailCalls:89-95 | `perRunCaches` maps | per thread | per transformer, as Flatten:124 |
| transforms | TypingTransformers:34-36 | `localTyper`, `curTree` | per thread |  |
| transforms | TypeAdaptingTransformer:149 | `ld.symbol.modifyInfo` | confined to a unit | a label |
| transforms | InfoTransform:50-57 | `infoTransformers insert` | serial step | phase construction, in `Run`'s constructor |
| transforms | ast/TreeGen | none | per thread | no fields; `typerHere` at `:193` |
| transforms | `global.typer` | no remaining use in `transform/*`, `backend/*` or TreeGen | per thread | `typer` in `patmat/*` is the translator's `localTyper` |
| transforms | patmat/PatternMatching:63 | `javaClassesByUnit` | serial step for this phase | not read by patmat; written by `JavaParsers` and read by `Namers`, under typer above |
| transforms | patmat/Logic:161, `:554-577`, `:771-789`, MatchAnalysis:201-240, `:794`, Solving, MatchCodeGen:25 | uniques, id counters, `ctr` | per thread | nested in `OptimizingMatchTranslator`, one per match (`PatternMatching:115`) |
| transforms | patmat/MatchOptimization:45-47, `:59`, `:101` | CSE maps | per thread | linked; `Set[Prop]` only for subset and equality |
| transforms | patmat/MatchTreeMaking:155-185, `:318` | binder sets | per thread | membership only |
| transforms | async/AsyncPhase:23, `:35` | `currentTransformState`, `sourceFilesToTransform` | serial step | not on the pool; write at `:47` under macro expansion's root |
| transforms | async/AsyncPhase:36 | `awaits` | unprotected | P14 |
| transforms | async/AsyncNames:28, `:67` | name caches | serial step | see |
| transforms | reflect Scopes:94 | `scopeCount += 1` | unprotected, benign | a statistic |
| back end, driver | BCodeHelpers:252 | `reporter` swap | per thread | fixed by the same change, A3 |
| back end, driver | BCodeHelpers:251 | `noReporter` lazy | immutable after a point | forced by `forceLazyValsOfObject(CodeGenImpl)` |
| back end, driver | BCodeHelpers:54-64 | `delambdafyInline.runId/value` | immutable after a point | written on main in `GenBCode.initialize`, before the workers (:100) |
| back end, driver | BCodeHelpers:435,448 | `symData` get, `-=` | claim or monitor | under its monitor; companion computed outside it. The `-=` race is benign: class and companion are one unit, on one thread |
| back end, driver | BCodeHelpers:402,409 | `pickleMarkerLocal/Foreign` | confined to a unit | `def`s: a new `Attribute` per use, so asm's `nextAttribute` is not shared |
| back end, driver | BCodeHelpers:476-479 | annotation retention lazies | confined to a unit | per builder, a builder per class |
| back end, driver | BCodeSkelBuilder:79-85,299-310,400-402 | class and method state | confined to a unit | `new SyncAndTryBuilder` per class (CodeGen `genClass`) |
| back end, driver | BCodeSkelBuilder:138 | `f.setFlag(STATIC)` | confined to a unit | fields of the module being generated, private, read by its own unit |
| back end, driver | BCodeSkelBuilder:142-150,417,550 | new symbols, clones, `changeOwner` | confined to a unit | not entered in shared scopes; owners local to the unit |
| back end, driver | BCodeIdiomatic:38 | `JavaStringBuilderClassName` lazy | immutable after a point | forced |
| back end, driver | CodeGen:27 | `mirrorCodeGen` | serial step | on the pool a new builder per mirror (:83) |
| back end, driver | CodeGen `addSbtIClassShim` | `currentUnit.icode` | confined to a unit |  |
| back end, driver | GenBCode:65, PostProcessor:48 | `generatedClassHandler`, `classfileWriter` | immutable after a point | set before the jobs are queued |
| back end, driver | GeneratedClassHandler:85-86 | `unitIndex` | immutable after a point | as above |
| back end, driver | GeneratedClassHandler:96-104,148-153 | `generatedUnits`, `processingUnits` | claim or monitor | monitors; sorted by unit on take |
| back end, driver | GeneratedClassHandler `task`, `classes` | `CompilationUnitInPostProcess` | immutable after a point | handed over through the executor; read on main after the join |
| back end, driver | PostProcessor:51 | `caseInsensitively` | unprotected | P12 |
| back end, driver | BTypes:56 | `classBTypeCache` | claim or monitor | CHM with `putIfAbsent`; one canonical `ClassBType` per name |
| back end, driver | BTypes:598 | `_info` | claim or monitor | volatile, set under the instance monitor, first wins; monotonic |
| back end, driver | BTypes:608 | `pendingInfo` | claim or monitor | plain, written before the CHM publish; a read of null goes to the `synchronized {}` branch, so it pairs with the writer's monitor. A stale non-null only recomputes. No missing fence. The computation itself |
| back end, driver | BTypes:801 | `cachedToString` | immutable after a point | benign race on a `String` |
| back end, driver | BTypes:860 | `pendingInfo` closure | unprotected | P2 |
| back end, driver | BTypes:1062-1098 | `AbstractLazy.t`, `value`, `postForce` | claim or monitor | a null `t` is only seen after `value` is set, and `init` tests `value` under its lock first. `postForce` under a monitor |
| back end, driver | BTypes:1139-1156 | `LazyVar` | claim or monitor | volatile `isInit` after `v`; `reInitialize` on main before the workers |
| back end, driver | BTypes:961 | `BTypeExporter.builderTL` | per thread |  |
| back end, driver | BTypesFromSymbols:50,113 | `primitiveCompilationUnits`, init closure | immutable after a point |  |
| back end, driver | BTypesFromSymbols:355 | local `nestedClassSymbols` | confined to a unit | local lazy in a closure, under the lock when lazy, or forced eagerly |
| back end, driver | BTypesFromSymbols:439-454 | `ph = phase` | confined to a unit | captures the computing thread's phase. Fine on a worker; `NoPhase` on a backend thread (). No caller reaches `classBTypeFromSymbol` under a phase travel except `enteringJVM` |
| back end, driver | BTypesFromSymbols:617 | `newSyntheticValueParam` | confined to a unit | new symbol, not entered |
| back end, driver | BTypesFromClassfile `classBTypeFromParsedClassfile` |  | claim or monitor | without `-opt` always a cache hit from `Collector`: `enterReferencedClass` (BCodeHelpers:683) caches every name in a signature, and a thread's own code generation caches the rest |
| back end, driver | CoreBTypes:~53 `LazyVar`s |  | claim or monitor | wrappers forced, values on first `.get` under `frontendSynch` (for the thread) |
| back end, driver | analysis/BackendUtils:56 | `indyLambdaImplMethods` | claim or monitor | `computeIfAbsent`, value under its own monitor; keyed per class; read through a `TreeSet` with a fixed ordering, so `$deserializeLambda$` is stable |
| back end, driver | analysis/BackendUtils:449 | InnerClasses order | immutable after a point | a `TreeSet` by internal name per class; constant pool order then comes from the per-class `ClassNode` |
| back end, driver | analysis/BackendUtils:289 | `modulesAllowSkipInitialization` | immutable after a point | forced |
| back end, driver | PerRunInit `inits` |  | claim or monitor | monitor |
| back end, driver | PostProcessorFrontendAccess:191 | `classBTypesOffLock` | immutable after a point | lazy, forced |
| back end, driver | PostProcessorFrontendAccess:249 | `localReporter` | per thread |  |
| back end, driver | ClassfileWriters JarEntryWriter |  | claim or monitor | monitor |
| back end, driver | ClassfileWriters:239 | `builtPaths` | claim or monitor | CHM, an optimisation over `createDirectories` |
| back end, driver | ClassfileWriters:335 | virtual tree | unprotected reader | P18 |
| back end, driver | VirtualFile `content` |  | immutable after a point | written on a worker, read after the join |
| back end, driver | BackendReporting `BufferingBackendReporting` |  | confined to a unit |  |
| back end, driver | backend/JavaPlatform:48-51 | `externalEquals*` lazies | claim or monitor | not forced. Worker takes `platform`'s monitor, then may need the completion lock (BCodeBodyBuilder:1563). No thread holds the lock and wants this monitor (codegen on the pool is off the lock), so no inversion; only a short wait. Would become one if something under `frontendSynch` read them |
| back end, driver | Global:132 | `currentReporter` | written by the driver only | swaps by workers removed, R4 |
| back end, driver | Global:301 | `propCnts` | per thread |  |
| back end, driver | Global:412-413 | `sourceCharset`, `stockSourceReader` | serial step | set in `reader`'s initialiser |
| back end, driver | Global:~466-494 | `readSources` | per thread | own reader and `StoreReporter` per thread; `out` read after the joins |
| back end, driver | Global:521 | `globalPhase` | serial step | main writes, between steps |
| back end, driver | Global:1137 | `lastSeenSourceFile` | unprotected, benign | crash text, and `currentSource` when a thread has no unit |
| back end, driver | Global:1125-1126 | `curRun`, `curRunId` | serial step | job queue gives the ordering |
| back end, driver | Global:1156 | `curFreshNameCreator` | per thread | under the flag |
| back end, driver | Global:1289-1296 | `currentUnit` | per thread |  |
| back end, driver | Global:1327 | `unitbuf` | claim or monitor | synchronized; `compileLate` rejected |
| back end, driver | Global:1346 | `compiledFiles` | immutable after a point | filled by `addUnit`, read by Typers:478 |
| back end, driver | Global:1349-1354 | `symSource` | claim or monitor | CHM; writers of distinct keys (Namers serial, Typers:1699, SpecializeTypes:631 under the coarse root) |
| back end, driver | Global:1357 | `symData` | claim or monitor | all accesses in Pickler and BCodeHelpers, under its monitor |
| back end, driver | Global:1359-1360 | `phasec`, `unitc` | claim or monitor | `advanceUnit` synchronized; `advancePhase` on main. `progress`, an sbt callback, runs under `Run`'s monitor; completes no symbol |
| back end, driver | Global:1367 | `stopPhaseSetting` | serial step | forced in `Run`'s initialisation |
| back end, driver | Global:1502 | `ranFused` | serial step | experiment; driver only |
| back end, driver | Global:869-881 | `phaseDescriptors`, `phasesSet`, `phasesDescMap`, `phaseNames` | immutable after a point | forced on main |
| back end, driver | Global:227 | `scalaPrimitives` | immutable after a point | `init()` on main in jvm's `initialize` only |
| back end, driver | Global:798 | `object typer` | shared by policy | compiler uses `typerHere`; macros that use `g.typer` share its context |
| back end, driver | SubComponent:66-81 | `ownPhaseCache`, `ownPhaseRunId` | unprotected, benign | two plain fields read as one, by workers (Fields, Mixin, LambdaLift, SpecializeTypes). Worst case: a thread reads the old cache, another thread writes both, and the first accepts the old value against the new run id. That is the previous run's `Phase` with the same id on the same `Global`; `enteringPhase` uses its id, and the phase list is the same. A null goes to the recompute. Only with a reused `Global`. |
| back end, driver | Run | `reporting.seenMacroExpansionsFallingBack` | unprotected, benign | only set to true; read after the join |
| back end, driver | Global `informUnitStarting` |  | public callback | arrival order; plan records it as harmless |
| back end, driver | Reporting:118-138 | suppressions, suspended, summaries, `textEdits` | claim or monitor | `PerRunReporting`'s monitor; rendering is outside it |
| back end, driver | Reporting:45,81 | `wconf`, `quickfixFilters` | claim or monitor | lazy on that same monitor; no symbols |
| back end, driver | Reporting:353-357 | `reportedFeature`, `explainedFeatures`, `deferFeatureExplanations` | claim or monitor | `featureReported` unlocked but only called from `ReplGlobal` |
| back end, driver | ParallelTyper:511-567 | `BufferingReporter` | claim or monitor | own monitor. Under the typer's ordering (file, point, arrival), a tie at one point keeps the first arrival, which is what `duplicateOk` shows: a nondeterminism in messages only if two threads report different messages at one point |
| back end, driver | reporters/Reporter:37 | `_cancelled` | unprotected, benign | visibility only, as in serial with sbt |
| back end, driver | reporters/Reporter:83 | `positions`, `messages` | serial step | underlying reporter, on main (is the exception) |
| back end, driver | Reporter `count`s via `increment` |  | claim or monitor | overridden with a monitor on the buffer |
| back end, driver | SymbolLoaders:233 | `ok` | claim or monitor | under `complete`'s lock, including the shared `moduleClassLoader` |
| back end, driver | SymbolLoaders:211 | `nameCharBuffer` | claim or monitor | callers all in `PackageLoader.doComplete` |
| back end, driver | SymbolLoaders:335 | `classFileDataReader` | claim or monitor | single-level reuse under the lock; only `parse` |
| back end, driver | SymbolLoaders:391-392 | `parentsLevel`, `pendingLoadActions` | dead | no other uses in nsc; runtime reflection has its own |
| back end, driver | SymbolLoaders:362-363 | `associatedFile` writes | claim or monitor | reads off the lock see a final-field `AbstractFile` or `NoAbstractFile` |
| back end, driver | ClassfileParser:65-79 | `file`, `in`, `pool`, `classTParams`, ... | claim or monitor | per loader; later completers are classpath symbols, whose `completionRoot` is the coarse root |
| back end, driver | Pickler:63,145 | `sigWriter`, `pendingSigFiles` | claim or monitor | lazy on the phase's monitor, no symbols; one `ArrayBuffer` per unit |
| back end, driver | Pickler:176 | `pickleState` | per thread |  |
| back end, driver | Pickler:188-237 | `Pickle` state | confined to a unit |  |
| back end, driver | BrowsingLoaders |  | not reached | interactive only |
| back end, driver | classpath/AggregateClassPath:38 | `packageIndex` | claim or monitor | monitor; the only use outside loaders is `-opt` |
| back end, driver | classpath/* (others) | lazies, `cachedPackages` | claim or monitor | every `list` and `findClassFile` found is under a loader's lock: SymbolLoaders:316, ClassfileParser:1092 |
| back end, driver | classpath/ZipAndJarFileLookupFactory:208 | `cache` | claim or monitor | monitor; shared between `Global`s |
| back end, driver | src/reflect/.../io/ZipArchive:151,248,288 | `lastDirName`, `root`, `allDirs` | claim or monitor | volatile; lazies under the archive's monitor, no symbols |
| back end, driver | src/reflect/.../io/AbstractFile:108, PlainFile:37 | `extension`, `canonicalPath` | immutable after a point | idempotent lazies |
| back end, driver | io/SourceReader:35 | `chars` | per thread | one per thread in `readSources` |
| back end, driver | settings/MutableSettings:365-946 | setting values | immutable after a point | `MultiChoiceSetting` computes when set, not when read. No worker write found; `Yshow.tryToSetColon` (Global:1667) is on main in the driver |
| back end, driver | settings/MutableSettings:238-243 | `OutputDirs` | immutable after a point |  |
| back end, driver | plugins/Plugins:109,156 | `plugins` | immutable after a point | forced by `computePhaseDescriptors`. Analyzer and macro plugins are rejected; other plugins' phases are not in `latePhasesOnPool`, so they are serial between pool steps. A plugin `InfoTransform` takes the coarse root by default |
| back end, driver | profile/* |  | rejected | `-Yprofile-enabled`; `NoOpProfiler` otherwise. `ThreadPoolFactory` |
| back end, driver | PhaseAssembly |  | serial step |  |
| back end, driver | Statistics |  | rejected | `-Ystatistics` in `unsupported()` |
| back end, driver | keepPhaseStack |  | rejected | `-Ylog` in `unsupported()` |
| back end, driver | Parsing:30 | `incompleteHandler` | immutable after a point | parallel parser off when set |
| back end, driver | Scanners:1546-1547, javac/JavaScanners:139-140 | keyword tables | immutable after a point | `kwArray` in the `SyntaxAnalyzer`'s constructor; Java's in a nested object, makes names only |
| back end, driver | Parsers:291 | `ScalaValueClassNames` | claim or monitor | lazy on the analyzer's monitor; names only, no symbol |
| back end, driver | Scanners, Parsers, MarkupParsers, SymbolicXMLBuilder, JavaParsers instance fields |  | confined to a unit | one parser per unit |
| back end, driver | SyntaxAnalyzer:43,68 | `MemberPosReporter` state | serial step | `-Ymemberpos` keeps the phase serial |
| back end, driver | SyntaxAnalyzer `parseAndLoadPackages` | `failure`, counters | serial step | driver only; `state` atomic |
