# Sharing between compilers across the build graph

Status: a design note for review, nothing implemented. It asks how the parallel typer spike (`PLAN-parallel-typer.md`, retronym/scala#217) dovetails with Zinc's rounds and with pipelined multi-project builds. Zinc is read at sbt/zinc `develop` as of 2026-10-11, IntelliJ at 2026-10-02, the Scala plugin at 2026-10-09. Section 12 separates what was run from what was only read.

## 1. Summary

The problem: a multi-project Scala build pays for one program many times. Each subproject, and each round inside a subproject, creates a fresh `Global`, reloads signatures from class files, retypes files whose inputs may not have changed, and runs the back end on output that a later round may replace. Downstream waits for all of upstream's rounds. The spike has made one `Global` readable by many threads, so the question is how far that can be pushed: across rounds, across subprojects, to one universe for the whole build.

What the reading and measurements say:

1. A true foreign reference does not exist as a middle option. A `Symbol` is an inner class of one `SymbolTable`, its identity tests are against that table's `definitions`, names and interned types, and its meaning depends on that table's phase and run registers. Any design is either a copy (pickles, importers) or one universe. The interesting range is how much of one universe, and how long it lives.
2. Reading a symbol table safely needs it frozen, and a Scala 2 table is not frozen after the pickler, because later phases rename symbols, set flags and enter scope members with no version attached. Measured: a kept `Global` emitted different bytes from a fresh one on real code, because a name expanded by a later phase in one run survived into the next (section 3). This is the same back-channel the spike's order effects come from, and the same one the Lean model's purity forbids.
3. Keeping the symbol table across rounds is worth less than it looks, once the JVM is warm: 74% on a one-file round, 17-26% on rounds of 20 to 40 files (section 3). Rounds themselves are worth more: 12-53% of compile time on four real builds is spent in rounds that are not the last (section 3).
4. On the incremental multi-project steps recorded, downstream is usually small next to upstream, or it uses macros. So speculative handoff of provisional signatures between projects saves little there (about 8-30% on the few steps recorded, and nothing on the macro-heavy build). Its payoff is on clean builds, where upstream's first round is already final.
5. Seven subprojects of this repository compiled jointly in one `Global` gave 5174 class files byte-identical to compiling them separately, and took 25% less total work on one thread. That is the best evidence so far for one universe: compositionality held on this corpus, and most of the cost of separation is symbol loading (22-26% of the compiler's samples in a profile of a fresh round).
6. Lowering, not signatures, is now the bound on the critical path. It is 60% of the work serially and about 80% once the front end is parallel, and with a parallel front end one module's lowering (the compiler's, about 10 s) is longer than the front ends of all eight modules together (about 4 s). Pipelining and every front-end handoff can only go as far as that, so the sibling work on parallel late phases matters more to a multi-project build than anything in this note until it lands.

Recommendation, in order: (a) keep investing in purity, because the oracle that makes the parallel typer trustworthy (any completion order gives the same bytes) is the obligation every sharing design rests on; (b) finish lowering on the pool; (c) take the steps that need no new universe: run rounds through the front end to a fixed point before running the back end, and compile groups of compatible subprojects jointly as one Zinc compile, which is already possible and has now passed a first byte-level check; (d) publish signatures from the spike's signature stage as a validated early output, once lowering no longer hides the gain; (e) treat a build-wide front end, one universe with per-project visibility and lowering kept out of the shared table, as the destination, with IntelliJ's architecture as its reference. Section 9 sets out that destination and section 10 ranks the options.

## 2. Vocabulary

From the talks (`zinc-incrementality` §1-6 and §22, `zinc-lean` §6a), used unchanged:

- A *query* is a question one class's compilation asks about another. A *trace* is the queries a compilation asked. A *key* is what Zinc stores for a class: a class and an aspect of it. A key *covers* a query if the query's answer cannot change while the key's hash stays the same. *π* is the hash per key. *Δ* is the keys whose hash changed in a round, *inv(Δ)* the classes that recorded one. A class is *up to date* if its output is what compiling it now would give and its keys cover that trace.
- The compiler of one unit is a query tree, F_d : (Query → Answer) → Out. *Purity* needs no axiom in the model, since the type rules out global state; `Symbol.id` in a sort key or completion order in a name are back-channels that the type forbids and scalac has.
- The bridge obligations: *compositionality* (a joint compile is a fixed point of the per-unit tasks, which is what joint ≡ separate means), *coverage*, *abstraction*, *policy soundness*. Theorems: T1 trace soundness, T2 round invariant, T2′ with non-local hashes (Δ over the affected classes, recomputed), T3 uniqueness given acyclic or source-determined interfaces, T4 termination, T5 downstream soundness with fresh snapshots. *Early agreement*: an answer read from an early output equals the final answer.

Labels this note adds:

- *Round*: one pass of Zinc's loop, which Zinc calls a cycle; R_n is the set of sources it compiles.
- *Universe*: one `SymbolTable` instance with its names, definitions, interned types, phase registers and run id.
- *Frozen view*: a set of symbols and types that a reader can use without any of it being mutated during the read, at one agreed phase.
- *Layer*: a universe, or a part of one, holding the symbols of a closed set of class path entries, identified by the content hash of those entries.
- *Handoff*: what upstream gives downstream so that downstream can proceed, with its *stage* (which of upstream's steps produced it) and its *grade*: *provisional* (may be replaced), *early-final* (Zinc's early output: replaced only if a later phase of the same compile fails) or *built* (class files exist).
- *Front end*: parser to pickler. *Lowering*: the phases after the pickler, which is where most mutation of shared symbols happens.
- *Signature stage*: in the spike, class headers, then member signatures, then annotations, before bodies.

## 3. Where the time goes

All timings here are from a laptop (M5 Pro, 6+12 cores) under load from other sessions, load average 15 to 120, so ratios within one paired row mean more than the absolute numbers.

### One round, fresh `Global` against kept `Global`

Zinc's bridge creates a new compiler per round: `CompilerBridge.run` makes a `CachedCompiler0`, whose constructor creates the `ZincCompiler`, and closes it afterwards (`internal/compiler-bridge/src/main/scala/xsbt/CompilerBridge.scala:39-43`). The experiment compiled the same sources repeatedly in one warm JVM, alternating a fresh `Global` with a `new Run` on a kept `Global`, 25 warm-up pairs and 15 measured pairs, through the back end, built from this branch without `-Yparallel-typer`. The first three corpora are files of the compiler or a module against the rest as class files, which is what a round looks like.

| corpus | files | fresh ms | kept ms | saved | typer fresh/kept ms | class files differing |
|---|---:|---:|---:|---:|---:|---|
| `transform/Flatten.scala` against the compiler | 1 | 141 | 37 | 74% | 72 / 11 | 0 of 3 |
| `transform/*.scala` against the compiler | 22 | 1607 | 1340 | 17% | 680 / 465 | 1 of 150 |
| `src/scalap` | 22 | 427 | 317 | 26% | 156 / 83 | 0 of 201 |
| `src/scaladoc` | 43 | 5929 | 4508 | 24% | 2169 / 1272 | 2 of 403 |

Almost all the saving is in the typer (a kept universe does not reload symbols of class files) plus about 20 ms of setup, with some in refchecks, mixin and fields. The back end's time does not move. The absolute saving is a few hundred ms per round at this classpath size. So the earlier position in sbt/zinc#711, that the gain is "a constant second or two", and Grzegorz Kossakowski's report quoted there that the improvements "were not as large as we expected", hold up. For very small rounds, the common first round of an edit, the ratio is large, and jvican's 10 ms against 180 ms for snippets in the same issue is the same effect. That issue records that nobody on the thread had measured a real round; this table is such a measurement, on one classpath.

The differing bytes are the important result. In `Erasure.class` and two anonymous classes of `ScaladocGlobal` the kept `Global` emits a field `scala$tools$nsc$typechecker$ParallelTyper$$BufferingReporter$module` where the fresh one emits `BufferingReporter$module`, and likewise for other nested objects of traits on the class path. Those traits' symbols are loaded once and retained, and a name expanded during lowering in an earlier run is still expanded in the next. I infer that mechanism from the output and from `expandName` writing `name` with no version, and have not traced the exact call site. `adaptInfo` discards a symbol's later infos between runs (`Symbols.scala:1885-1889`, after scala/bug#8871), and nothing does the same for names, flags or scope entries.

A second experiment ran one edit as Zinc would: four files with inferred types rippling through (`A.f` changes type, `B.g = A.f`, `C.x = B.g`, a trait with a private member, a case class, a specialized class), rounds {A}, {B}, {C, D}, class files pruned before each round, compared with a clean build of the edited program. A fresh `Global` per round and a kept `Global` both gave bytes identical to clean. A kept `Global` with `invalidateClassPathEntries` on the output directory before each round, which is what a sound protocol would seem to want since class files changed under it, failed in the last round with "Symbol 'type scala.package.Serializable' is missing from the classpath. This symbol is required by 'class p.CC'". That was one deterministic-looking run and I did not investigate the cause, but it means the official invalidation path cannot be assumed to work.

### Where a fresh round's time goes

Async-profiler, `itimer`, in a warm JVM, inclusive shares of the samples inside the compiler (a sample counts in every row whose frames it has, so rows overlap). Ten runs of `transform/*.scala`, six of `scaladoc`.

| bucket | fresh, `transform` | kept, `transform` | fresh, `scaladoc` |
|---|---:|---:|---:|
| symbol loading, all (`SymbolLoader.complete`) | 22.1% | 0% | 26.3% |
| of which class files | 15.0% | 0% | 18.6% |
| of which package listing and entering | 8.0% | 0% | 8.8% |
| unpickler, eager and lazy | 6.9% | 0% | 6.7% |
| class path listing | 7.6% | 0% | 8.9% |
| `Definitions.init` | 1.7% | 0% | 1.6% |
| `adaptToNewRunMap` | 0% | 1.0% | 0% |
| `baseTypeSeq` | 7.0% | 5.3% | 7.9% |
| `baseClasses` | 10.6% | 1.5% | 9.5% |
| `findMember` | 18.7% | 11.1% | 14.5% |
| `asSeenFrom` | 13.1% | 11.1% | 8.6% |

So loading is about a quarter of a warm round on this class path. Within it, unpickling is the smaller part: parsing the class file (constant pool, annotations, finding the pickle) and listing packages together cost more. `adaptToNewRunMap`, the feared cost of a resident compiler (sbt/zinc#711), is about 1%. Note that `baseTypeSeq` hardly moves between fresh and kept, since its caches are stamped by run and recomputed: a layer that kept those across runs could save up to another 5-8%, and that requires the finer stamps of section 8. `baseClasses` and `findMember` fall because the rows include the loading they trigger. One discrepancy I have not resolved: the kept run has 56% fewer compiler samples than the fresh one, where wall time fell 16% in this run and 17% in section 3's table.

Retained heap of a kept `Global` on `scaladoc`, after a full GC: 71 MB after run 1, then 82, 91, 100, 110, 119 MB. It grows by about 10 MB per run with the same sources, and that is a leak or an accumulation that a long-lived table would need to find, the same family as scala/scala3#27297. For scale, a whole-module class path table is modest, which is good news for layers.

### Critical path and the joint compile

Per-module phase times, warm JVM (three warm-up passes and three measured, medians), modules as in `build.sbt` (`interactive` and `repl` depend on `compiler`, and so on). `critpath.py` computes the models. *Traditional*: downstream starts after upstream's class files. *Pipelined*: after upstream's pickler. *Signature*: downstream parses at once, runs its signature stage after upstream's, its post-typer steps after upstream's. The signature stage is assumed to be 20% of the typer, in line with the plan's numbers. No Zinc or bridge phases are in these numbers. Serial phase times in ms:

| module | parse | index | typer | superaccessors to pickler | lowering | total |
|---|---:|---:|---:|---:|---:|---:|
| library | 263 | 21 | 1612 | 123 | 3982 | 5623 |
| reflect | 118 | 7 | 1666 | 73 | 3523 | 5459 |
| compiler | 276 | 10 | 4288 | 141 | 6875 | 11893 |
| interactive | 38 | 7 | 324 | 6 | 494 | 922 |
| repl | 35 | 7 | 370 | 6 | 455 | 891 |
| repl-frontend | 32 | 10 | 297 | 6 | 311 | 680 |
| scaladoc | 46 | 8 | 591 | 19 | 931 | 1612 |
| scalap | 31 | 5 | 248 | 8 | 407 | 735 |

Critical path of the seven modules after `library` (which needs its own flags), in ms. "Work/6" is the bound from six cores, and it is well below these, so the graph's shape is the limit here.

| front end | traditional | pipelined | signature | one `Global`, measured | work/6 |
|---|---:|---:|---:|---:|---:|
| serial per module | 19375 | 13453 | 11761 | 16694 on one thread | 3611 |
| `-Yparallel-typer`, 6 threads, parser on the pool | 18011 | 11938 | 11362 | 13825 (10397 to 18196) | 3366 |
| the same, with lowering assumed 4 times faster | 6932 | 4733 | 4157 | | 1312 |

The third row is a what-if. It is there because the first two rows are dominated by the compiler's lowering, serial in all of them. With lowering sped up, pipelining saves 32% over traditional and a signature handoff 12% more over pipelining. A longer chain is where signature handoff matters: for a chain of L equal modules with these fractions and lowering on the pool, a signature handoff is 1.5 module-times at L=8 against pipelining's 4.2, and 2.1 at L=16 against 7.9. This graph is one hub with a chain in front of it, and real many-module builds have not been simulated, which is the next thing to do.

The joint compile compiled `reflect`, `compiler`, `interactive`, `repl`, `repl-frontend`, `scaladoc` and `scalap` as one source list in one `Global`, one output directory, the same options, sharing's macros on the class path. Results:

- 5174 class files, with the same names and the same bytes as the seven separate compiles, serially, and under `-Yparallel-typer` against the separate parallel compiles. No difference at all. The separate compiles read the build's class files for what they depend on, so this is a direct check of joint ≡ separate over this code. It is one codebase, with explicit types on most public members and few macros, so it says less about other styles.
- On one thread: 16694 ms against 22192 ms for the sum of the separate compiles, 25% less work. Typer 5797 ms against 7784 ms, which matches the loading share in the profile. Lowering 10.2 s against 13.0 s, a difference I cannot explain by loading alone and which may be load on the machine.
- With `-Yparallel-typer`: typer 1869 ms for all seven. Lowering is then 11 s of 13.8 s.

So one thread on one `Global` is within 25% of the pipelined critical path on unlimited cores, and a parallel typer with parallel lowering on one `Global` should beat pipelined builds outright on this graph, with no handoff protocol at all.

Also found: with `parallelPostTyper` on, `src/library` crashed once in about 19 compilations with a `NullPointerException` (`tp2` null in `TypeComparers.isSubType2`, under `ExtensionMethods$Extender.transform` on a pool thread). The rerun without that property was clean, and all parallel numbers above are without it. Details are in the shared notes folder for the post-typer work.

### Rounds on real builds

The IncBench campaigns (`~/code/.integration/campaigns`, `talks/zinc-incrementality/integration/anomalies`) record per-cycle compile times. From the released Zinc cell, the last repetition of each edit and revert, pipelining off:

| build | edits and reverts | rounds per module, most in a step: count of steps | compile ms | in non-final rounds | clean build ms |
|---|---:|---|---:|---:|---:|
| pekko (`actor`, `actor-typed`) | 18 | 1: 10, 2: 8 | 10829 | 12% | 3595 |
| scala-steward (`core`) | 16 | 1: 6, 2: 10 | 8139 | 22% | 3248 |
| cats (`kernel`, `core`, `free`) | 10 | 2: 6, 4: 4 | 43231 | 29% | 20076 |
| shapeless (`core`, `examples`) | 18 | 4: 8, 5: 9 | 225372 | 53% | 7522 |

Time outside the compiler inside the Zinc call (pruning, analysis, merging) is a further 7-20% at the median. The shapeless row is a macro cascade: `recompileOnMacroDef=true` and the third invalidation in `invalidateAfterInternalCompilation` keep pulling macro-bearing classes back, so a body edit costs more than a clean build, as `zinc-incrementality` §4 predicts.

Per-cycle times on the steps that cross a project boundary, upstream's rounds then downstream's, in ms:

| step | upstream | downstream |
|---|---|---|
| pekko `timeout-add-implicit` | 102, 1467 | 704 |
| cats `functor-add-member` | 173, 4062 | 381 |
| cats `eval-add-case` | 128, 2376, 235, 4770 | 175 |
| cats `show-add-implicit` | 131, 2767, 517, 6067 | 111 |
| shapeless `hcons-tostring-body` | 226, 3250, 3710, 3060 | 3202 |

The shape: round 1 is small (the edited file), the later rounds are the work, and downstream is small unless it depends on macros, in which case it needs class files anyway. Note also 2376 and 4770 in the same step: the same classes are being recompiled.

### What this says about the options

The term to attack in an incremental build is Σ|R_n| and the number of rounds, before c_sig in the talk's cost model. Reloading signatures is a real term, about a quarter of a round's compiler time, but it is secondary. On clean builds, the critical path through subprojects is the term, and at present lowering dominates it.

## 4. Zinc's rounds, precisely

The loop is `IncrementalCommon.CycleState.next` (`internal/zinc-core/src/main/scala/sbt/internal/inc/IncrementalCommon.scala:79-150`).

**Before round 1**, `detectInitialChanges` (line 382) computes changed sources by stamp, products whose stamps changed, libraries whose stamps changed, and subproject API changes: `detectAPIChanges` over `previousAPIs.allExternals`, comparing the stored snapshot of each external class with `lookupAnalyzedClass` on the upstream analysis (lines 452-461). `invalidateInitial` (line 569) turns these into classes and sources: the classes of removed and modified sources, the `memberRef` dependents of removed classes, the classes invalidated by external API changes through name hashing, plus sources from added and modified files, changed products and changed libraries. A library change invalidates every source using it, with no name filter (line 590). With `-Ypickle-java`, if anything changed, every Java source is added (`Incremental.scala:393-395`).

**Each round**: classes are expanded by invalidated package objects (`invalidatedPackageObjects`, classes in the same file and their subclasses that are package objects), mapped to source files, and if more than `recompileAllFraction` (default 0.5) of sources would recompile, all of them do (`mapInvalidationsToSources`, line 298). The class files of those sources are pruned, through a `ClassFileManager` shared by all rounds of the compile so that a failure rolls back everything (`pruneClassFilesOfInvalidations`, line 939; `Incremental.scala:464-466`). The compiler runs with a new `AnalysisCallback`. The partial analysis is merged over the pruned previous one. `detectAPIChanges` compares `apiHash`, `extraHash`, the bytecode hash and, for macros, the transitive bytecode hash, for every class recompiled (lines 330-359), and for changed classes yields name changes or a change "due to" a macro or annotation definition.

**The next set**, `invalidateAfterInternalCompilation` (line 482), is the union of: (1) API-driven invalidation through `invalidateClassesInternally`: inheritance dependents transitively and unfiltered, `memberRef` dependents filtered by changed names, with `MemberRefInvalidator` dropping the filter for implicits, macros and annotations; (2) classes whose class files collide; (3) macro-bearing classes transitively downstream of anything recompiled, whether or not its API changed. The stop test subtracts R_n from (1) only. If that residue with (2) and (3) is empty there is no next round. Otherwise the next set is the unsubtracted union, so R_{n+1} can repeat classes in R_n.

**When it gives up**: from round `transitiveStep` (default 3, with `cycleNum` from 1) the next set becomes the transitive closure of (1) plus (2), (3) and everything just recompiled, so R_n only grows and the loop ends (T4). `isFullCompilation`, all sources in the round, ends the loop at once with no API comparison (lines 165, 204). `recompileAllFraction` can make that happen at any round.

**What crosses rounds today**: the `Analysis` (relations, stored APIs, stamps), the class files on disk, the backup held by `ClassFileManager`, the early-output flags in the callback builder, and with pipelining the set of Java sources, which are all recompiled in every round so that the Scala sources can see them (`IncrementalCommon.scala:129-139`, sbt/zinc#918). Also the JVM with its JIT state, the jar classpath cache across compiler instances (`-Yno-flat-classpath-cache`, `ScalaSettings.scala:355`), and cached macro and plugin class loaders. No symbol, tree, pickle or type crosses a round. Round n+1 reads round n's results only from class files, on the classpath as an output directory.

### What rounds could share, and what discipline makes it sound

Each step down this ladder needs the one above.

1. *Front-end convergence before lowering.* Zinc already decides whether another round is needed in the middle of a run when pipelining: `invalidateAndWriteEarlyOutput` computes `mergeAndInvalidate` at `dependencyPhaseCompleted`, after the pickler and before refchecks (`Incremental.scala:1463-1473`). So in a non-final round the compile could stop lowering the classes it now knows are in R_{n+1}, and next-round readers could read pickles from a `-Ypickle-write` output in place of class files, as downstream does under pipelining. No in-memory sharing at all. The discipline is T2's: a class outside inv(Δ) is up to date, so its lowering can proceed, or be deferred, against the signatures it was typed with. Two limits. A macro implementation in the same project must have bytecode for the next round to expand it, so classes that define macros and what they call lower eagerly; on shapeless that is the bulk of the case. And lowering reads more than typing (`zinc-lean` §16), which is covered by the existing keys only if the coverage obligation holds for those queries. A variant with more payoff is to suspend each non-final `Run` at the pickler with its `Global` alive, and at the fixed point resume lowering only for units not recompiled later, so that all back-end work happens once, at the end. That retains one universe per round for the length of the compile, disjoint and not shared.

2. *Keeping class path symbols across rounds* (a resident `Global` keyed by class path and options hash). Saves the table in section 3. Sound only if the class path symbols are frozen, which they are not: a lowering phase in round n leaves renames, flags and scope entries that round n+1 reads. Either those mutations must be versioned or undone (a list of what lowering writes to symbols not owned by the current run), or the kept universe must never lower (see 4).

3. *Keeping source symbols of units not invalidated.* A unit outside R_{n+1} keeps its source-derived symbols where today round n+1 would load them from the class file. This changes what compositionality must prove: today every round is separate compilation, and joint ≡ separate is the known obligation with a known list of bugs (`zinc-incrementality` §6, scala/scala#11289-#11293). With kept source symbols the reader sees the *joint* view of those units, across a run boundary, which is a third view. In return, the Java front-end split disappears for retained units, and a class of joint ≠ separate divergence stops mattering. The invalidation discipline: `adaptToNewRunMap` rebinds by name lookup through prefixes (`TypeMaps.scala:1175-1218`) and throws on a member that no longer exists, so it copes with removal, but a kept symbol's *signature* is only as fresh as its unit's last typing. A kept symbol whose inferred info read a changed answer must be discarded together with its unit, which is what inv(Δ) gives when coverage holds. Discarding needs package scopes updated too: this is where `invalidateClassPathEntries` failed above.

4. *Keeping typed trees of units not invalidated*, the far end. This does not save work on units that were not invalidated, since Zinc does not recompile those. It pays when a unit is in R_n only through over-approximation: a sibling class in the same file, an overload it does not pick, a descendant recompiled to refresh a materialised hash. The test for reuse is the verifying trace: re-ask the unit's traced queries against the new table, and if all answers are equal, T1 says the output is equal. This is rustc's `try_mark_green`, and §37 of `dotc-interactive` is its first step at the granularity of a body: 382 ms to 5.9 ms for a whole-file run after a keystroke in `Types.scala` in Scala 3's presentation compiler, keyed on a definition with a declared type, which is the rule of the Scala plugin's `BlockModificationTracker.hasStableType`.

### Relation to the spike's order findings

Every sharing step above relies on an output that does not depend on when something was completed. The spike found three kinds of violation, and each is a counterexample to purity in the model's terms.

- Fresh-name numbering leaks into class files when completion order changes. If a body is reused from an earlier round, numbering in the rest of the unit changes unless it is scoped per member (which the 2018 work did for user-written methods, `zinc-incrementality` §6a).
- O1: which unit first forces an `import scala.Predef.{...}` decides whether a signature says `Predef.String` or `java.lang.String`, deterministically, in the serial compiler too. In a kept table, the answer can differ between rounds with no source changed: a spurious API diff and an extra round (`zinc-incrementality` §16(e)).
- `Symbol.isLess` breaks ties by id, which reached lub parents; scala/scala#11291 orders by name instead. A retained table allocates ids across runs, so ids are not even comparable with a fresh compile.

The oracle the spike already has is therefore doing Zinc's work: run with a different order, on N threads, with `-Dscalac.parallelTyper.order`, and compare pickles. Extending it from "same as the one-thread parallel run" to "same under any order and under separate compilation" is the shared test for all of this. The kept-`Global` byte comparison from section 3 is a cheap addition to that family.

## 5. What another `Global`'s symbol would have to guarantee

The reference is impossible to hold in the plain sense. `Symbol` is an inner class of `SymbolTable`, its methods go through `outer`. The checks that make it a symbol of this compiler all compare identity against one table: `definitions` (`sym == ObjectClass`, in hundreds of places), `uniques` for types (`Types.scala:4503`), the name table (`Names.scala:19`, names compared with `eq`). A `TypeRef` to a symbol in another table would fail those tests silently. So a foreign symbol from a separate universe can only be read through a wrapper or copied: `StandardImporter` (`Importers.scala:41`) does the copy, lazily and with caches, and relinks static symbols by looking the name up in the target table, which means the target must have loaded them anyway. It exists for toolboxes. It would save nothing over unpickling, and adds retention of the source universe.

Hence the question becomes what a reader needs of symbols in a *shared* universe that a writer is also using. The conditions, in order of difficulty:

1. **A pinned time.** `info` depends on the reader's phase (`Symbols.scala:1878`), `flags` on `phase.flagMask` (line 799), validity on the current run (`SymbolTable.isValid`, line 356). The spike gave each thread a phase register, so a reader can sit at the pickler while a writer is elsewhere. Reader and writer need the same run id, which makes a new `Run` a write that excludes readers: the *epoch*. A symbol table with two projects at different rounds cannot use one run id, so rounds would have to bump a per-project epoch and `isValid` would have to consult it, or caches of shared types would need revalidation per project.
2. **Completers that run in the right view.** A reader can force a lazy symbol, and the completer must run with the owner's namer context, settings and classpath view. The spike already establishes the first part: a completion on another unit's thread uses that unit's context. It would extend to another project's.
3. **No in-progress state visible.** The spike's rule: another thread's in-progress state is something to wait for, never to act on. `_completingThread`, claims per key with wait-for cycle detection, stamped caches with markers. This holds across projects with no change, and a wait-for cycle between projects cannot be genuine, since the project graph is acyclic and any cycle is a lock order, answered by lending at a provisional info, as I9 was within one project.
4. **Publication.** Fences on `info_=` (I13), holders for stamped caches, scope entries published safely. No change.
5. **Stability after publication.** This is what the spike does not have, because within one project all units are in one phase. `isValid` and `isValidForBaseClasses` stamp caches by period and assume phases advance together. Superaccessors and extmethods rename and enter members, which is why the post-typer steps stay steps (`PLAN-parallel-typer.md`, "Steps, not fused"). Case class synthetics enter during body typing. Past the pickler, flatten renames, mixin and fields enter members, explicitouter and lambdalift set flags, `makeNotPrivate` expands names. `Scope` has no version at all ("an entry made by superaccessors is seen by a reader at any phase"). So upstream's lowering cannot run on the same symbols as downstream's typer. Options: lower only after every reader has finished (a build-wide lockstep); lower in a separate universe; or version names, flags and scopes by phase, as Scala 3's denotations do for names, flags and decls. Scala 3 is much nearer to this condition than Scala 2 by design, though its leaks show it is not free of the problem (scala/scala3#27297).
6. **Run-scoped state.** `perRunCaches`, `Run.symData`, `symSource`, `compiledFiles`, the reporter, `currentRun` itself are one per run; they would need to become per project.
7. **Retention and cancellation.** Readers keep the upstream alive. Cancelling one project's work must leave shared symbols resumable (`dotc-interactive` §16 and §12: Touched restored, or the symbol poisoned). I8 and I14 in the spike were leaks of whole `Global`s through thread-local state; scala/scala3#27297 found the same family when reusing a `ContextBase`.

## 6. The range of designs

| design | saves | costs | breaks |
|---|---|---|---|
| A. Today: class files, pickles and analysis through the file system | | IO, c_sig per `Global` | |
| B. Same formats in memory: pickle bytes and analysis objects passed directly | jar writing and reading, small | a build tool API (sbt's task graph is the owner) | nothing; a remote cache still needs files |
| C. A pool of resident `Global`s keyed by class path and options hash, for rounds and `~compile` | section 3's table | leak risk, invalidation protocol, byte differences must be fixed first | history: sbt/zinc#340, #711. Plugins that hold state; `-Yno-flat-classpath-cache`-style staleness |
| D. A shared read-only class path layer: library symbols loaded once, read by many projects | c_sig for libraries, once per build rather than per compile, plus base type sequences and member caches if their stamps survive | requires a universe shared by the readers, so in effect E's mechanism for the class path only. Every reader must agree on phases if it lowers | different eviction per project means different layers; optional dependencies resolve differently (below) |
| E. One `Global`, several output roots, per-project visibility, and project stages run in lockstep | all of c_0 and c_sig between projects in the group; a joint fixed point in place of cross-project rounds; cross-project soundness rules become one loop | settings per unit, reporter per project, memory | macros between projects in the group; plugins that differ; options that differ |
| F. Build-wide front end: one universe for all projects up to the pickler, lowering in per-project universes or after everything | as E, and upstream lowering no longer blocks or mutates | a new driver, and a way to hand typed trees to lowering | as E, plus the handoff of trees (section 9) |
| G. A compiler database: queries over content-hashed inputs, per-member traces, red-green reuse | rounds disappear as a concept | a rewrite in spirit; every implicit dependency must be a query | everything that is a back-channel today |

Design E has a version that needs no compiler change: an sbt or Mill build merges a group of compatible subprojects into one Zinc compile. The joint experiment in section 3 is this version, and Zinc's `MultipleOutput` and `settings.outputDirs.add` already route classes to several outputs. What it gives up is enforcement of the dependency direction and any isolation of errors, and name resolution can then see symbols of a project that is not a dependency. That can be turned into a check rather than a filter: run strict-deps on the recorded dependencies, as Bazel does, and let the joint ≡ separate byte oracle catch resolution that differs. An implicit, a shadowing class or a package object member from a non-dependency are what it would find.

Designs D and E need per-project visibility for real, and this is where the compiler's structure helps. *Visibility only differs at package level.* Once a reader holds a class symbol, members, parents and base types are the same for everyone, because they were resolved when that class was completed. So what must vary per project is package scope lookup (`Context.lookupSymbol`, `rootMirror`, `getPackage`, the unpickler's external references) and package object members opened into them (`openPackageModule`, `SymbolTable.scala:379`). IntelliJ passes a `GlobalSearchScope` explicitly to each lookup; a compiler with per-thread ambient state can read a current-project view at the same sites.

Same fully-qualified class in two projects (shaded copies, a test class that shadows a main class, two versions) needs package scopes keyed by (name, project), and the same `Symbol` cannot serve both. A layer is sound for a reader only if everything it resolved would resolve the same on the reader's classpath. So the layer's key must cover the transitive closure of its entries. A library that references an optional dependency only some projects have sees a stub in the layer and a real class in the project: detect a stub created in a shared layer, and fall back. This is the one place the design falls back to a fresh `Global` per project, and it should be a first-class path that the oracle exercises.

### What each breaks, feature by feature

- **Macros.** An expansion needs bytecode of the implementation, loaded through `findMacroClassLoader` (`Plugins.scala:182`). In one universe this forces *stages*: a project providing macros, with its dependencies, completes through lowering before the projects that use it start. Scala 3's suspension is the same idea inside one project: a unit calling a macro defined in the same run is suspended, the rest compile through the back end, then a second run compiles the suspended units (`Driver.scala:66-74`, `CompilationUnit.scala:116`). A macro is also a reader outside the query discipline: `macroObserve` in the model, `transitiveBytecodeHash` in Zinc. Zinc's `shouldDoEarlyOutput` already refuses pipelining for a project that defines a macro (`Lookup.scala:147`). Whitebox macros and implicit materialisers in inferred types put bytecode needs at the signature stage.
- **Java sources.** One universe gives the source view of Java everywhere, which is what pipelining already does, so `JavaParsers` must agree with `ClassfileParser` (`zinc-incrementality` §6, `java-context` verdicts). Javac still needs upstream class files, so it stays at the end (`MixedAnalyzingCompiler.scala:226`, `PipelineMain.scala:216`). Parameter names and Java constant folding remain the two known disagreements.
- **Compiler plugins.** Phases are per `Global` with ids in periods (8 bits), so projects with different plugin phases have different phase numbering, and one universe needs a union phase list with per-unit skipping. The spike rejects analyzer and macro plugins today. Kind-projector and semanticdb are common enough in real builds that this is a condition for any real use. Scope for the first version: groups of projects with identical plugins.
- **Options per project.** `settings` is read everywhere. Options split into those that decide how source is read (`-Xsource`, `-Yimports`, `-language`, `-Ytasty-reader`), which belong to a unit; diagnostics (`-W`, `-X` lints, `-Wconf`), per unit and already routed through `PerRunReporting`; and lowering (`-opt`, `-release`, `-target`), per project. A `-release` difference changes what classes exist, so it changes the JDK layer. First version: groups with the same reading options.
- **Memory.** A build-wide universe holds what is now spread and collected per compile. To be measured. The first-order mitigation is that lazily completed library symbols are shared, and unit trees can be dropped after lowering, as today. `Global`s retained by reader threads are the known leak family (I8, I14).
- **Error isolation.** Errors in upstream must not cascade as false errors in downstream. Today downstream does not start. In one universe, an `ErrorType` in a shared signature reaches readers, and diagnostics must be buffered per project and withheld for projects downstream of an error. The spike's buffering reporter is the mechanism. IntelliJ's stance, errors as values that do not stop analysis, is the alternative. Diagnostics in provisional handoff must be withheld until the upstream they read is final.
- **Cancellation.** The batch compiler has only the cooperative `reporter.cancelled`, between units (`API.scala:49-51` shows how the bridge polls it). Build-level cancellation when upstream changes its mind needs Scala 3's session work to be ported: a cancelled completion left resumable, `Touched` restored, and so on (`dotc-interactive` §12-18).

## 7. Pipelining with upstream doing rounds

### Today's protocol

The handoff unit is a jar of `.sig` pickles (`-Ypickle-write`) with a trimmed early analysis: internal APIs and `productClassName`, nothing else (`writeEarlyArtifacts`, `Incremental.scala:1486-1502`). sbt's `compile` completes `earlyOutputPing`, and downstream's `exportedPickles` blocks on it, taking class files if it says no early output (`sbt/main/src/main/scala/sbt/Defaults.scala:4634`).

The important property is when it fires: only in the last round. `invalidateAndWriteEarlyOutput` computes the next invalidations right after the dependency phase and writes early artifacts only `if !continue && shouldDoEarlyOutput`. So upstream's rounds are entirely inside downstream's wait: upstream's non-final rounds, *with their back ends*, then the last round's front end. Grade: early-final. It can only be withdrawn by a later failure in that same run, for Scala 2 anything from refchecks on, which sbt/zinc#1843 handles by rolling back. The Lean `PipelineLifecycle` proves the invariant published = built with that rollback.

So the answer to Jason's point that upstream is doing rounds: today there is no cross-project fixed point at all. It is stratified. Upstream reaches its fixed point alone, then downstream starts with `subprojectApiChanges` as its initial changes, which T5 says is enough. Sound and simple, and the price is exactly the latency above. That latency is worse than it first looks: a non-final round's back end serves only the next round, which could read pickles.

`PipelineMain`, the in-repo prototype, has no rounds. It has `Pipeline` (publish at the pickler phase in one run), and `OutlineTypePipeline`, which adds a first pass with `-Youtline` and no macro expansion, then a second full pass. Its critical-path accounting (lines 228-237) is the precedent for the simulation asked for.

### Which steps can start before upstream is final

Checking the earlier discussion's list against the source:

| step of downstream | what it needs from upstream | check |
|---|---|---|
| parser | nothing | confirmed; `definitions` lookups in the parser touch only the library and root classes (PLAN "What the parser shares") |
| index (enter top-level symbols) | less than "top-level names": a listing of packages, and a name only if it collides with one being entered | `enterClassDef` overwrites whatever a loader put in the scope. Java `permits` clauses are the exception, typing at the namer's phase |
| package objects phase | **upstream package object members**, a signature-level read | the earlier list missed this: `openPackageModule` reads `container.info.decls` of a package object from the class path (`SymbolTable.scala:379-394`, `Analyzer.scala:92-117`) |
| class headers | upstream headers, and base types | |
| member signatures | upstream signatures. Inferred types type downstream bodies here, so implicit search and overloads are reachable. A macro in an inferred result type needs **bytecode here** | the earlier list put macros only as bytecode, not at which stage |
| annotations | upstream annotations; `@implicitNotFound`, `@deprecated` are read through lazy thunks | |
| bodies | upstream signatures, plus things not in a signature stage: case class synthetics entered during upstream's body typing (`addSyntheticMethods`), sealed children for exhaustivity, implicit scope through companions | synthetics override `Any`/`Product` with the same signatures, per the spike's argument, but a pickle can show them |
| superaccessors, extmethods, pickler | upstream's names **after** its superaccessors: a `private[C]` member reached through a type is written as `C$$x` or `x` depending on whether upstream's phase ran | PLAN "Steps, not fused": this was a measured race in the fused experiment. Thus a downstream pickler cannot be byte-stable against a signature-stage handoff, unless the two agree on expansion |
| lowering | nothing from upstream's bytecode, unless `-opt:inline` crosses projects: bodies are API (`zinc-lean` §23b, sbt/zinc#537) | |
| javac | upstream class files | confirmed, `PipelineMain.scala:216` |
| plugins from the same build | bytecode | confirmed, `PipelineMain.scala:382` |
| Zinc's own bookkeeping | upstream's stored API for external snapshot | `lookupAnalyzedClass` through the early analysis |

### Options for handoff stage and grade

- *Earlier stage, early-final grade, clean builds.* When a project has only one round (every clean build, since `isFullCompilation` stops the loop), its signature stage is the only pass. Publish pickles of the signature stage. The spike has that stage separately, and downstream's typer can begin when upstream's bodies are just starting. What can differ from the pickler's output: synthetics, expanded names, annotations added during body typing. So the protocol is *validate*: at the end of upstream's pickler compare π of the final pickles with those published, and if they differ, downstream is invalidated by the usual rule, at the key level. No shared universe. In a real run, a downstream that finished against an unmatched pickle is simply invalidated by T5: Δ on the keys that moved.
- *Provisional grade, incremental builds.* Publish after each round with an epoch number. Downstream treats a new epoch's Δ as more initial invalidation. The loop is: compile R_n against the latest epoch, at the end check for a newer epoch, invalidate by its Δ against what you read, and continue. Publishing to downstream's downstream is withheld or marked by dependency on upstream's epoch.

### The cross-project fixed point

Let P be the acyclic project graph. A global state is one analysis per project plus the epoch each one read. By T2 per project and T5 across, the state with no dirty class and all read epochs current is a per-unit fixed point of separate compilation.

- *Termination.* Upstream's epochs are finite by T4, and downstream restarts at most once per upstream epoch, with a monotone policy from `transitiveStep` on within each. So the bound multiplies along a path, though that is a loose worst case: round counts are small in practice (table in section 3).
- *Uniqueness.* T3 needs the class graph acyclic or interfaces source-determined. Downstream's own inferred cycles are unaffected by the extra input. Project-level acyclicity prevents the bad multi-fixed-point cycles from crossing a project boundary.
- *`zinc-lean` §19 and §23a* already check that one loop per project and one loop with a layout policy agree run for run on the implicit-scope space. So a virtual monorepo is one loop with a different policy, at least in that space. It also has a benefit beyond speed: rules that need the whole graph stop at the project boundary, and this is why the same edit is clean in one subproject and wrong across two (§19, sbt/zinc#1845, retronym/zinc#26). With one loop over one graph these bugs have nowhere to live.

What must be redone when upstream changes its mind, with a provisional handoff: keys whose hash moved give inv(Δ) over downstream, as for any change. Downstream lowering that was finished for those classes is lost, and rollback must span projects: `ClassFileManager` is per project today, so "commit" must become a build-level transaction. Diagnostics reported from provisional reads must be dropped, and that means nothing reaches the user until the read epochs are final. Macro expansions that read provisional upstream are not covered by keys at all: under provisional handoff, projects with macros stay at built grade. Published class files or test runs based on a provisional view must be blocked.

### Is provisional handoff worth it?

From the steps table in section 3, assuming downstream could start after upstream's first round and run in parallel: pekko saves at most the 704 ms of 2273, cats about 111-381 ms of 4600 to 9800, shapeless nothing, because of macros. That is 8-30% and usually at the low end, on few samples, with pipelining off. The cost is a build-wide transaction and withheld diagnostics. I do not recommend it now. Front-end convergence cuts the same latency with no protocol.

## 8. The Merkle frame

### Hashed nodes and edges if sharing were keyed by content

`zinc-incrementality` §5 and §10 hash a class's members as its declarations composed with its parents' hashes. Extending that across the build, each node's key is the hash of its own input plus the keys of what it read:

| node | key | value | sharing it gives |
|---|---|---|---|
| source | content hash | text | |
| parse | source hash, parser options | tree, positions | across rounds; across branches; remote |
| outline (stub) | parse key | top-level names, headers and declared signatures as unresolved trees | the index with no upstream; a signature-determined early cutoff |
| class path layer | content hash of closed set of entries, in order | loaded symbols | across all projects with the same layer |
| class header | outline key, keys of each resolved parent's header | parents, type params, self type | |
| member signature, per class and name | declaration, verifying trace of what it read | as-declared type; for an inferred type, the verifying trace includes the body's reads | this is π(C)(n), the Merkle name hash |
| body | body source, trace | typed tree | §37's body edit |
| bytecode | unit's typed form, lowering options, keys of everything lowering reads | class file | Bazel-style action cache |

Edges are traced queries, with the key of the callee node at the time asked. That the *key* is a verifying trace, not a value, is the talk's point: "the Merkle hash that makes the per-query model work is the verifying trace of the walk".

Today's builds are keyed by build step: a hit requires sources and all dependency ABI jars identical. With content keys, a member's bytecode survives a rename in an unrelated part of the upstream jar. Zinc is doing this at the class-and-name level, with π stored and inv(Δ) computed; the Merkle PoC moves it towards what the table has (retronym/zinc#24). What's missing is any node below class level, and bytecode for any node at all, since Zinc's reuse is by not recompiling a file rather than by a store.

### Where the soundness conditions bite

- **Compositionality.** The obligation for designs C-G is stronger than joint ≡ separate: *one universe at any history ≡ a fresh compile*. Section 3's byte difference is a counterexample, found in the first experiment. Any design that retains a table needs this in CI as an oracle.
- **Coverage.** The keys come from the typed tree, not the trace (`zinc-lean` §23c), so misses and discarded lookups (`+=`, `_3`, an added class shadowing a name) are not recorded. In a shared universe every one of those is a stale-cache bug as well as an undercompilation bug. A reused body must be re-verified against a trace that includes misses. So the talk's open item "log the compiler's queries and check the bridge's keys against the log" is on the critical path for design G and for the typed tree step of the rounds ladder. The hook sites exist: lookups, `findMember`, implicit search, `baseClasses`.
- **Abstraction.** Equal hash, equal answers. Non-local hashes must be recomputed for the affected classes (T2′); `Stale.lean` has the two-unit counterexample. In-process, this is exactly `isValid`: a stamped cache of `baseTypeSeq` on a downstream class is a non-local hash, whose inputs are upstream's headers. Keeping it across rounds requires invalidating over the inheritance closure of anything recompiled, the same rule. The spike's stamps today use run and phase only, so they invalidate everything on a new run, which is sound and is what a kept table pays for (`baseTypeSeq` is recomputed each run). Finer stamps are a Merkle hash in disguise, and if they are too fine, a stale read.
- **Policy soundness** is untouched by these designs. Any policy that only adds work is sound. Skipping a round's lowering is an implementation detail if the next round's readers get equal answers from pickles: that is early agreement, and it fails on Java constants and `@inline` bodies under the optimizer (`zinc-lean` §23b). Those are the cases to fall back on class files for.
- **Uniqueness (T3).** Keeping source symbols is the case to worry about: separate compilation of `A.x: typeof(B.y)`, `B.y: typeof(A.x)` admits any type, and a kept table holds one chosen by history, whereas a joint compile reports a cycle. The spike's finding, that genuine cross-unit cycles are reported with varying blame once signatures are not completed in serial order, is the same underspecified point from the other side. So the error is the same in every order but the blame moves; content keys must not hash diagnostics' blame, and the choice of who reports must be fixed by a rule (by unit order), as the plan's claims sketch says.
- **Snapshots.** `Snapshot.lean`: a stored upstream API refreshed only for referenced classes is stale after an edit and revert, when hashes are non-local. A retained universe has the same trap in the opposite direction: the retained answer is from the last read, not the last refresh.

### What the spike's in-process sharing needs that it does not have

Within one run the spike's stamps and claims are sufficient, because hashes are not needed: everything computed in the run is from one input. Across runs or projects, a stamp needs to mean "the inputs I read are unchanged", and that is a key. So the bridge from the spike to the frame is: `Stamped` (period) becomes stamped (period or key), and `Once` facts on symbols record their inputs. With per-symbol claims already in place (the parallel signatures work), a completion is now a clear query boundary: it has one root, it runs under a claim, and it can record what it read. That is the concrete reason the spike dovetails with the frame: completion roots *are* the nodes.

## 9. The destination: a build-wide compiler, with IntelliJ as the reference

If design F or G were to exist, what would it look like? IntelliJ has run a version for two decades, for an analyser, with the same structural problems: many modules, one symbol graph, per-module visibility, stubs against ASTs, cheap invalidation.

| IntelliJ | what it does | scalac equivalent or gap |
|---|---|---|
| PSI, one graph for a whole project | modules do not partition the universe | one universe per build, design E/F |
| `GlobalSearchScope` (`moduleWithDependenciesScope`, `moduleRuntimeScope`, `GlobalSearchScope.java:200-248`), passed to every find; the Scala plugin's `ElementScope(project, scope)` | a *view* over one graph, one for main and one for tests | package-level project view, section 6. Ambient per-thread is the Scala 2 way to reach those sites without threading a parameter |
| Stubs: `StubBasedPsiElementBase`, "hold the most important information ... and use it during code analysis instead of parsing" | a file's outline, stored, with a switch to AST on demand that "is slow" | the outline node; `-Youtline`'s `canSkipRhs` (`Typers.scala:6620`); Buck's source-only ABI and Turbine for Java. Note that stub and AST must look identical to clients, which is joint ≡ separate again |
| `FileBasedIndex`, keyed by `IndexedHashesSupport` content hash; shared indexes downloaded ready-made | an index entry is reusable across projects and machines by content | content-keyed parse and outline, and class path layers |
| `CachedValuesManager` with `ModificationTracker` dependencies; `PsiModificationTracker` | a cached value declares what it depends on; project-wide count is the coarse fallback | `Stamped`, per run and phase today. Moving this to a tracker per project and per key is section 8's stamp |
| `BlockModificationTracker.hasStableType` (Scala plugin) | an edit in a body with a stable type bumps only that body's counter | §37 of `dotc-interactive`; Kotlin's in-block against out-of-block modification, where a `FirModuleResolveState`'s lifetime "is limited by Kotlin Out of Block Modification" |
| Read and write actions; pending write cancels reads; `ProgressManager.checkCanceled` | epochs and cancellation | a new `Run` or an upstream epoch is a write action. Scala 3's session has this; Scala 2's batch path does not |
| `RecursionManager` | a re-entry returns null and refuses to cache above it | `LOCKED` and `CyclicReference`, with `Touched` |
| JPS: an out-of-process builder. `ModuleChunk` is the strongly connected set. `ExitCode.ADDITIONAL_PASS_REQUIRED`, `CHUNK_REBUILD_REQUIRED` (`ModuleLevelBuilder.java:48`, `IncProjectBuilder.java:1461-1577`), parallel chunks with a ready list (`BuildChunkTask`) | rounds, with passes marked dirty through a dependency graph, and project-level parallelism | Zinc's cycle and sbt's task graph; `CHUNK_REBUILD_REQUIRED` is `recompileAllFraction` |
| Compiler reference index: JPS writes one and the IDE reads it (`CompilerReferenceServiceBase`, `ScalaCompilerReferenceIndexBuilder`) | the only arrow from builder to analyser | semanticdb, Zinc's analysis |

Two things IntelliJ teaches by what it does not do.

*The PSI universe is never lowered.* That is what lets one graph live for days: nothing like erasure rewrites a shared `PsiClass`. And JPS does not reuse it: the code analysis and the compile are two independent implementations, in two processes. For Scala, this split is the reason for false red code, and a compiler with one implementation cannot afford the split. But it can adopt the discipline: the long-lived shared table holds front-end facts only, and everything after the pickler happens elsewhere.

*Sharing costs per-query discipline, paid everywhere from the start.* Every IntelliJ lookup takes a scope. Scala 2's is ambient, and has 656 unprotected fields found by the state map. The spike has now paid a large share of that cost for threads. The next phase of that cost, per-project state, can reuse that map's categories.

Kotlin K2's low-level FIR sits in the same place in a compiler: a session per module with a "view from a specific module to the dependent modules", composite symbol providers across source modules and binaries, resolve-in-place per declaration with a compare-and-set state. Roslyn is the design with most to teach for references across compilations: `CompilationReference` lets one compilation reference another's symbols directly, with `RetargetingAssemblySymbol` wrapping the symbols when the core library differs. It works because symbols are immutable once complete and lowering does not mutate them (the last point from memory, unverified). Retargeting is the honest cost model for *different layers*: wrappers per symbol. GHC's `--make` has a home package table and an external package state, one process with interfaces read once; the EPS is design D for dependencies. Rust's `.rmeta` is the same handoff point as a pickle, and proc macros are the same exception (cargo waits for full compilation). Salsa's revision is the epoch.

### Sketch of the build-wide front end

A long-lived compiler process per build, per Scala version and per compatible options group.

1. *Content stores* for parse and outline, on disk, reusable across branches and machines. A clean build's parse phase starts from them. Top-level names of all projects are known at time zero, as is whether a declared signature changed.
2. *Layers* for class path entries, closed under dependency, loaded lazily, with no mutation permitted after load. A stub in a layer demotes the project to a fresh `Global`. External library signatures become a once-per-build cost.
3. *One front-end table* for all project sources. Stages: the parallel typer's existing steps, build-wide. Parse all, index all, class headers (serial, in a defined project and unit order: the spike's evidence is that headers need it, and the cake is where it matters), member signatures on the pool, annotations, bodies on the pool, superaccessors, extmethods, pickler. Macro-providing projects with what they depend on are stage 0, compiled through lowering first, then visible as class path. A project in a stage reads only its view.
4. *Lowering outside the table.* For this step I see three candidates, and it is the main unknown. (a) Lower after all readers finish, in the same table, destructively, one project at a time, and discard the universe at the end of the build: simple, loses retention across builds, and upstream lowering can no longer start early, though with typing on all cores the wait may be short. (b) Each project's lowering in its own disposable universe, which needs typed trees to cross. Scala 2 has no TASTy, so either the trees are copied through `Importers` (which exists, and has fixups for cycles), or the pipeline is reorganised so that lowering runs from pickled typed trees: a large format project. (c) Version names, flags and scopes by phase so that one table holds all phases, as Scala 3's denotations do: a change that touches every phase, but then the table is permanent. For Scala 3, (c) is largely present and (b) has TASTy, so the same destination is much closer there.
5. *Red-green inside the store.* Each signature and body keeps its verifying trace. An edit re-verifies, and if a node's answers are unchanged its value stands. Zinc's analysis remains as the persisted form of these keys, so that the rest of the ecosystem and the cold start still work, and π is still what crosses to anything outside the process.
6. *The Zinc loop* becomes a policy: its round is what happens when a key's new hash differs, but a build-wide query store can process that on the affected queries, in dependency order, without a batch boundary. Zinc's rounds exist because Zinc learns the API by running a whole-file batch. The formal content does not change: T1-T3 hold with a finer unit. Early cutoff per key, per body.

What survives if this never gets built: all the steps that lead there are independently useful, since they are the purity oracle, per-member query records, stamps with inputs, and a front-end/lowering separation in the driver.

## 10. Options ranked

Each entry: expected payoff against risk, on what's known so far.

1. **Purity as a CI oracle, across order and history.** Payoff: enables everything, and fixes real bugs now (remote cache hit rates, spurious rounds). Risk: low. Partly done. Add: kept-`Global` byte comparison; the stability check over the community build.
2. **Lowering on the pool.** Not in this note's scope (the late-phases session has it), but listed because it sets the value of everything after it. Payoff: on section 3's graph, 11.9 s to about 4.7 s pipelined, if lowering gets 4 times faster. Risk: being measured there.
3. **Front-end convergence, then lowering once** (section 4 ladder, step 1). Payoff: the non-final fraction, 12-53%, of which 60% is lowering, so perhaps 7-30% of compile time in incremental builds, plus the same off upstream's latency, since non-final back ends leave downstream's wait. Less where macros fall back, and shapeless is that case. Risk: moderate. Touches Zinc and the bridge, no shared universe, no new soundness theory beyond early agreement. Macros and Java constants fall back.
4. **Joint compile groups: one `Global`, several output roots, one Zinc compile, no compiler change for the first version** (design E, section 6). Payoff: measured at 25% of total work on the seven modules, with byte-identical output, no handoff to get wrong, and the cross-project soundness gaps of `zinc-lean` §19 gone inside a group. The cost is that a change anywhere in the group can pull the group's API checks into one loop, and that errors and options are per group. Risk: moderate. It is reversible, a build setting. Macros between members and differing options or plugins exclude a project from a group. Should be tried on a real build next.
5. **Trace records at completion roots** (section 8). Payoff: a coverage check against real compiles, which finds undercompilation bugs regardless of anything else, and the foundation of red-green. Risk: low for the measurement, unknown overhead when on.
6. **Validated signature-stage early output for clean and full builds** (section 7). Payoff: small now on this graph (12%), large on long chains once lowering is fast (a chain of 8: 1.5 module-times against 4.2). Risk: moderate. Needs the parallel typer's stage split, also useful serially. Downstream's byte stability depends on expanded names, which needs an answer. Ranked below joint groups because within a group it is not needed, and because many real graphs may be wide rather than long: the simulation on real graphs decides.
7. **Resident `Global` pool for small rounds and `~compile`.** Payoff: real for very small rounds (74%), 17-26% otherwise, and Zinc's non-compile overhead in those rounds is similar in size. Risk: known bad history, the byte difference, the heap growth, the broken invalidation path. Do only after 1, and if at all only for the first round of a body edit. Fixing the mutations it needs is valuable in itself for 8.
8. **Build-wide front end** (design F, section 9). Payoff: potentially large on real many-module builds; joint groups measure the upper bound for free. Risk: high, multi-year, needs the lowering decision.
9. **Provisional cross-project handoff.** Payoff: small on recorded steps. Risk: high. Not now.
10. **Importer-based or in-memory handoff** (B). Payoff: IO only, which I did not measure separately. The profile puts most of loading in parsing and entering symbols, which an in-memory handoff keeps. Skip.

Recommendation: 1 and 2 are underway. Next, 3 and 4, independently: both can ship without a shared-universe protocol, and 4 can be tried tomorrow with a build setting on pekko, cats or a larger build. Keep 5 running as a measurement. Decide on 6 from the simulation on real graphs after lowering lands. Treat 8 as the direction that joint groups grow into, if they work: visibility as a check first, then a view, then lowering out of the table.

## 11. Cheapest experiments

Each says what would confirm or kill the option.

1. *Joint compile on a real build through Zinc* (option 4). Done without Zinc on this repository (section 3). Next: merge subprojects of pekko, cats (no macros between members) in the build definition, run IncBench's edits with `lockstep --verify`, and compare rounds, recompiled classes and wall time with the separate layout. Kill: a group's rounds grow enough to lose the gain, or joint ≠ separate bytes on code less regular than this repository. A strict-deps check for dependency direction is a small add-on.
2. *Critical path simulation on real graphs* (options 2, 6, 8, 9). `critpath.py` exists and has run on scala/scala's graph. Sharpen with x-ray recordings, and run on pekko, cats and a many-module build using `export-build`, with the signature share taken from `-Dscalac.parallelTyper.log`. Kill option 6: if chains in real builds are short.
3. *`PipelineMain` as it stands* on the same graph: `Traditional`, `Pipeline`, `OutlineTypePipeline`. Validates the simulator against something real.
4. *Non-final lowering fraction and overlap* (option 2). Replay IncBench steps with `xsbt-dependency` timestamps, and from the invalidation log compute |R_n ∩ R_{n+1}| and the phase split per cycle. The current data has counts per cycle, not sets. Kill: if overlap is small and most non-final classes are never repeated, and suspended lowering's memory is too high.
5. *Skip lowering in non-final rounds as a Zinc hack*: on a step, stop after the pickler in rounds where `continue` is true, reading pickles next round, then compile everything invalidated in a final round. Differential check against clean using the `lockstep --verify` harness.
6. *Signature-stage pickle divergence* (option 3). With the spike, write pickles at the end of the signature stage and at the pickler, on all corpora. Count the classes whose π differs. Kill: if a typical unit differs, the validation would always fail.
7. *What lowering writes to foreign symbols* (option 5, section 5). Instrument `name_=`, `setFlag`, `enter` during phases after the pickler, keyed by whether the symbol is from the current run's sources. Gives the actual list behind the byte difference and the size of a "freeze" or "undo". The state map tooling can help.
8. *Trace overhead* (option 5). Record queries at completion roots on the corpus, compare with Zinc's keys for coverage.
9. *What grows in a kept `Global`* (option 7): a heap diff between run 2 and run 6 on `scaladoc`. Cheap with `heap-paths`.
10. *Why `invalidateClassPathEntries` fails on the ripple* (option 7, and reachable by anything that wants to reload): `tools/parallel-typer/cross-global/run.sh ripple` reproduces it.

## 12. What was run and what was only read

Run for this note:

All on this branch's quick build, on a loaded machine (load average 15 to 120). Harness and scripts are in `tools/parallel-typer/cross-global`; raw outputs are in the shared notes folder under `cross-global`.

- The fresh against kept `Global` table, including byte comparison (section 3). Before a laptop restart, with the same harness as now committed.
- The inspection with `javap` of the differing class files.
- The four-file edit ripple on fresh, kept and kept with invalidation, run twice with the same outcome.
- Profiles of fresh and kept rounds, and the retained heap of a kept `Global`.
- Per-module phase times, serial and under `-Yparallel-typer`; `critpath.py` over them. The critical paths are computed, not observed. The signature share (20%) and the factor of 4 for lowering are assumptions. The `lockstep` model in that script assumes all lowering runs on the pool, and I left it out of the tables.
- The joint compile of seven modules with its byte comparison, on one thread and with six.
- Aggregation of existing IncBench campaign JSON for two tables in section 3. I did not re-run those campaigns, and I took the last repetition of each step.

Read from source, cited in the text: Zinc's loop and early output, the bridge's compiler lifetime and phase placement, sbt's pipelining tasks, `PipelineMain`, importers, `adaptInfo`, `openPackageModule`, Scala 3's suspension, IntelliJ's `GlobalSearchScope`, stubs, JPS exit codes and chunks, `BlockModificationTracker`.

Asserted from reading with the mechanism inferred and not confirmed by a run:

- That the byte difference comes from a name expanded by lowering in an earlier run. The evidence is the field names; I did not locate the phase.
- That skipping lowering of a class in R_{n+1} is sound given coverage. Argued from T2, not tested.
- That validation of signature-stage pickles makes a downstream compile end in a correct state. Argued from T5.
- That visibility varies only at package level. I know of no counterexample but searched only by reasoning about lookups. The likely weak spots are implicit search through imports and `companionSymbol`.
- All statements about what a stage in a build-wide front end could do. Nothing has been built.
- Option 3's payoff range, from multiplying non-final fractions on some builds by the lowering share measured on another.

From documentation or memory, not checked in source: Roslyn (`CompilationReference`, retargeting is from `source.dot.net`; immutability of symbols through lowering is memory), GHC's HPT and EPS (from source comments found by search), cargo's `.rmeta` pipelining (from the Rust internals thread and cargo#6883), Buck's source-only ABI (from Buck's documentation), Bazel's ijar and Turbine (from their documentation), Kotlin LL FIR (`docs/analysis-api/low-level-api-fir.md` and `fir-basics.md` at a mirror, an older revision), `try_mark_green` (memory).

Not done: section 11's experiments, apart from the first steps of 1 and 2 as described. `PipelineMain` itself was not run, so the simulated pipelined column has no check against a real pipelined build.

## 13. Questions for Jason's judgment

1. Destination. Is a build-wide front end a direction you want to hold in mind, or is the aim only to make today's per-project shape faster? It changes whether option 5's freezing work and per-project ambient state are worth starting.
2. Lowering. Of the three candidates in section 9, which do you believe in for Scala 2? Or is that the reason to do the destination in Scala 3 first, where denotations and TASTy already give most of it, with Scala 2 as the place where steps 1 to 4 ship?
3. Does the "one universe at any history ≡ fresh" obligation belong in the Lean model as a variant of compositionality? It seems to need only a compile function indexed by history.
4. Source symbols across rounds give a third view next to joint and separate. Is it better to embrace it (a kept joint view, dropping class-file rereads inside a project), or keep each round a separate compilation so that the existing obligations stay exactly what is tested?
5. For validated signature-stage output: should a downstream run on a pickle that later fails validation be allowed to finish and be repaired by the next round, or cancelled? Cancellation needs Scala 3's session work ported.
6. Is shapeless-style macro cascading in scope here, or does your macro fast-track work (rz#73) already take that row of the table out?
7. Joint groups as a build setting: would you try it on a real build next, or does the loss of error isolation and dependency direction rule it out for real users even with a strict-deps check? If it survives, it changes what a virtual monorepo first means: a setting, with a compiler project behind it only when the setting hits a limit.
