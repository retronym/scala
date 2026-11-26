# Wave four: generated inputs, oracles and coverage steering

Waves one to three found about 500 defects in Scala 2.13.x by reading code and writing small programs by hand. That method finds what a reviewer thinks to look for. It does not find the cell next to the one that was fixed, and it has no measure of what was never exercised. Wave four changes the method: generate inputs that are valid by construction, check them against an oracle that needs no human, and after each round use coverage to decide what the generator should learn next. The prompt for this was Datadog's Ephyra announcement; this note says what Ephyra is, what to take from it, and how to run a wave on that basis. A first spike is in this directory and already found defects that survive the draft fixes of the earlier waves.

## What Ephyra is

Sources: the [repository](https://github.com/datadog-labs/ephyra) (README, `STARTUP.md`, `API.md`, the `case-studies/` harnesses and bug indexes), the [Datadog post](https://www.datadoghq.com/blog/ai/agentic-property-based-testing-with-ephyra/) of 24 November 2026, and the preliminary paper [arXiv 2611.23282](https://arxiv.org/abs/2611.23282) (single author, Alperen Keleş; to appear at the Future of Property-Based Testing workshop at POPL 27).

- **A Rust property-based testing library, MIT, version 0.0.1, explicitly experimental.** `#[ephyra::test]` properties run under `cargo test`; the runner gives shrinking, exact replay by seed and iteration, a regressions directory, failure signatures (by panic site, or named by the property) so one long run can collect many distinct failures, and reports. An `ephyra` command line runs suites and checks generators. Settings come from environment variables.
- **A working method for a coding agent, written down in `STARTUP.md`.** This is the part that matters most. Decide what correct means before generating anything, and take it from the project's own documents. Oracles in rising order of effort: crash, round trip, reference model, invariant, metamorphic. Generators are valid by construction and start from the project's own tests as seeds. Stateful targets get sequences of operations. An oracle that fails on more than about 1% of inputs is assumed to be wrong. When nothing fails, change the generator against what it never produces or add an oracle on the same code; "a new oracle often failed on its first run; an old oracle given more time rarely did". After two flat rounds, move elsewhere.
- **"Autonomous generator optimization" is an agent rewriting the generator between rounds.** The README contrasts it with coverage-guided fuzzing: a fuzzer mutates promising inputs, Ephyra rewrites the generator so that promising inputs become likely. The paper calls the optimizer "a very simple hill-climbing loop". Coverage reports say which functions and lines were never reached, and the agent aims the next change there. A round is one such steering step; the harness notes say "steering round 1" added option toggles, round 2 a const-evaluation oracle, round 3 destructor counting. The SQLite chart in the announcement is 37 of these: coverage of SQLite's counters went from 35.9% to 74.1%, the SQL features the generator emits from 79 to 261, with 26 bugs.
- **The autonomous loop ("ephyra-agent") is not in the repository.** One harness README describes it as "an autonomous loop of Claude agents around the ephyra library". The paper's figure names its stages: setup, synthesis, search (twelve 30-minute slots), pre-triage (replay, classify), triage (minimize, fix), re-verify, write-up (deduplicate, packet), then a person validates and files. Every report was validated and submitted by the authors.
- **Coverage is LLVM-shaped and per target.** Rust through `-C instrument-coverage`; SQLite's amalgamation through clang's profile instrumentation, read in process; OCaml through bisect_ppx; Elixir through Erlang's `cover`; rustc by instrumenting the stage-one compiler. Each harness has a build script with a coverage mode and a `coverage.json` the controller maps back to sources. There is nothing for the JVM, and no JVM target among the 27 case-study directories.
- **For targets that are not Rust, the Rust test crate is a driver.** The OCaml, rustc and Elixir harnesses use Ephyra's seeded source as the random number generator and its runner for shrinking and replay, and shell out to the compilers.

### What found bugs in compilers

| Target | Generator | Oracles |
|---|---|---|
| OCaml (5 bugs listed), OxCaml (13) | Typed programs in three modules, valid by construction, cross-module inlining on purpose; shrinking by deletion only so reduced programs stay well typed | Crash; native code (closure and flambda back ends, sometimes `-linscan`) against the bytecode compiler of the same tree and against the previous release, seven configurations per case |
| rustc (harness published, no bug index) | Safe Rust, 100% compiling at baseline; 8% of cases from the existing rustlantis generator | Internal compiler error; optimisation levels and MIR passes against `-O0`; random pass toggles; compile-time evaluation against run time for `const fn`; destructor counts |
| Elixir (9) | Modules and calls aimed at the new type checker | Type checker verdict against run-time behaviour |
| Lean 4 (25) | Not published | Kernel rejecting proofs the elaborator built, compiled code against the kernel's evaluation, standard library round trips |
| ty (21) | Seed harness extended by the agent | Type checker and editor actions producing invalid code |

The pattern is the same each time: one program, several ways of running it that must agree. The most cited OCaml bug (a non-exhaustive integer match that jumps to a wild pointer, 24 years old) fell to native against bytecode.

### The numbers, and what they do not show

- The post claims 535 unique bugs in 26 systems, 373 confirmed by maintainers, 313 fixed. The paper, a month older, has 349 found and 300 reported in 14 systems. The bug indexes in the repository list 516 rows; Turso (109) and SQLite (76) are over a third of them.
- Cost, from the paper's only table: five Rust and C++ systems, about eight hours each on a 16-CPU machine with a $100 budget per system, $330 spent in total for 80 bugs, at 22% to 36% coverage. No token counts and no model named in the paper.
- No false-positive rate is published. The paper's status column includes duplicate, declined and "won't report"; the indexes have rows such as "reported by others".
- No controlled comparison. The paper says its results do not yet validate the design, that the loop "is not unique to Ephyra", and names reward hacking (optimizing coverage rather than testing power) as an open problem. The repository does carry experiments asking whether mutation pays against fresh generation on SQLite; their results were not read for this note.

## Decision: borrow the loop, not the library

Ephyra's library is the wrong substrate here and its loop is the right method.

- The targets are JVM code that is fastest in process. The collections spike below runs tens of thousands of calls per second inside one JVM; compiler programs need a warm `Global`. A Rust driver would talk to a JVM over a pipe to gain a seeded source, a shrinker and a replay line, each of which is a few dozen lines on the JVM.
- Its coverage integration does not reach the JVM. JaCoCo does, and this review already has it wired to the build.
- The part that produced the results, the agent loop, is not published. What is published about it is a discipline, and the coordinator sessions of waves two and three are already that kind of loop.

Taken from Ephyra: `STARTUP.md` nearly verbatim as the hunters' brief; the oracle ladder; signatures for deduplication inside a run; steering rounds with one generator change each, measured before and after; coverage as a list of gaps, never as the score; pre-triage of new failures against known bugs; a person validating every report.

Worth a second look later, not now: Jazzer (Apache 2, active, coverage-guided in-process fuzzing for the JVM) and JQF with Zest (generator-based coverage-guided fuzzing, BSD; maintenance unclear) would add mutation of promising inputs on top of generators. Ephyra's own experiments treat "does mutation pay" as open; a wave can find out cheaply once generators exist.

## The loop on the JVM

A track is a generator, a set of oracles and a coverage scope. One agent owns a track for a session.

1. **Oracle first.** The hunter writes down the three to five properties and where each comes from (Scaladoc, the specification, a reference implementation, a universal rule) before generating.
2. **Search round.** A seeded run with a time budget on the integration build of earlier draft fixes. Failures are shrunk by the generator's own structure (delete an element, a call, a member; never a text reducer, so reduced programs stay well typed) and bucketed by signature: operation, implementation or phase, and kind of difference (value, exception for value, value for exception, exception type, hang). After a hang or an out-of-memory error the cell is quarantined for the rest of the run so one bug does not eat the budget.
3. **Pre-triage.** Each new signature is replayed on the unfixed base and on 2.13.18, and matched against the findings of waves one to three. Test bug, intended, environment, known, new instance of a known family, new.
4. **Steer.** Coverage for the round is dumped and rendered. The hunter reads the largest unreached methods in scope and the count of inputs each oracle discarded, makes one change to the generator or adds one oracle, and records the before and after. Two flat rounds with no new signature: pivot.
5. **Write-up.** Same finding directory as wave three, plus the seed and signature. Each finding still maps to a fork pull request under the existing process.

### Coverage chooses the direction, not the earlier findings

The earlier waves left a map of where defects were found: self-aliasing, overflow at the Int extremes, value classes, specialization. A generator aimed at that map finds more of the same, which confirms fixes are narrow but says nothing about the rest of the code. So the generators here produce ordinary usage at ordinary weights, with the known seams as features among many, and each round is aimed at the largest code the previous round did not reach. The oracles are chosen to need no knowledge of a seam: a model, a second way of running the same program, a second implementation. The spike's own result is a caution in this respect: the self-aliasing cells came from an argument source added to the generator because wave two had found that shape.

After the spike's two rounds the largest unreached sources in `scala.collection` were, by missed lines: `immutable.Vector` (789 of 1625), `ArrayOps` (522 of 618), `StringOps` (445 of 477), `mutable.LongMap` (all 339), `immutable.HashMap` (335 of 1300), `CollisionProofHashMap`, the Java collection wrappers, `NumericRange`, `IntMap` and `immutable.LongMap` (all unreached), `LazyZipOps`, `HashSet`, `TrieMap`, `TreeSeqMap`, `ArrayBuilder`, the steppers and stream extensions, `StringParsers`, `PriorityQueue`, `OpenHashMap`. That list, not the findings list, is the agenda for the next collections round.

### Coverage and the treemap

JaCoCo's agent is already used for this review's coverage runs; attaching it to a harness JVM with `includes=scala.*` and writing one execution file per round is all a track needs. `tools/coverage_treemap.py` turns the XML reports into the picture from the announcement: one panel per round, each rectangle a method sized by instruction count and shaded by the share reached, grouped by source file, with the layout fixed across panels so the eye sees what a round opened up. `--prefix scala/tools/nsc/transform` (or `typechecker`, `backend/jvm`) gives the per-phase view for compiler tracks; `reports/compiler-transform-baseline-treemap.html` is that view for what the existing test suites reach, which is the starting panel for the compiler tracks.

Two cautions. Coverage of the compiler from in-process compilation needs the compiler classes on the harness class path to be the ones the report is built from, so build the report from the same `build/pack` jars. And coverage is a gap list: a track is scored by distinct confirmed signatures, with a frozen set of seeds rerun each round as a regression check on the generator itself.

## Targets, ranked

Ranked by expected new findings per unit of effort after the spikes. Java sources and embeddings of expressions rank high because their oracles are independent of any earlier finding and each new construct paid at once; more volume on library files already covered ranks low.

1. **Java sources: scalac's parser against javac.** Generated Java sources, with a Scala probe compiled jointly with the sources and then against javac's class files. javac is a complete reference, cases are cheap, and four findings including a regression came from 70 files. Not yet generated: type annotations, several types per file, cross-package access, `package-info`, probes that should be rejected.
2. **Generated Scala programs, run several ways that must agree.** A general generator of small terminating programs in everyday Scala: pattern matching, for-comprehensions, lazy vals and by-name parameters, try/finally, closures and local classes, traits and linearization, generics, default arguments, with value classes and `@specialized` as features at ordinary weight. The same program is compiled with the optimizer on and off, in both delambdafy modes, jointly and separately, by 2.13.18, and where valid by Scala 3; rewrites that should not change meaning (drop `@specialized`, drop `extends AnyVal`) are further oracles. All must print the same lines. This is Ephyra's OCaml and rustc recipe. Cost is the generator: it has to compile nearly always to be worth running, and coverage of the transform phases and the back end says which construct to add next.
3. **Programs of calls on the library against reference models.** Aim only at files not yet reached (the specialised maps, `TrieMap`, `TreeSeqMap`, `PriorityQueue`, the Java wrappers, steppers); the files already covered are done. Thousands of programs a second and trivially valid, so cheap to keep running against each new integration build. The existing `test/scalacheck` collection suites generate small arbitrary lists and maps and a few builder command sequences; across them there are ten mentions of `Int.MaxValue` or `Int.MinValue`, no self-aliasing and no comparison across implementations.
4. **Expressions in many embeddings.** The values are sound; what paid was placing an expression where few tests put one (a match scrutinee, an annotation argument). Continue by adding embedding contexts and statement forms, aimed at the unreached parts of `CleanUp`, erasure and the back end, not by adding arithmetic.
5. **Existing tests as seeds for the same rewrites.** The run tests under `test/files` are several thousand valid programs with known output. Applying the rewrites of target 2 mechanically to them is a generator with no validity problem. Wave three already diffed typer output of 4627 partest files against 2.13.18; this reuses that corpus with metamorphic oracles instead.
6. **Scala 3 as a second opinion on generated programs.** Only where the two languages agree, so the grammar is narrow. Better as an extra oracle on targets 2 and 3 than as a track.
7. **Type checker laws in process.** Reflexivity, transitivity and congruence of `<:<` and `=:=`, `lub`/`glb` laws, `asSeenFrom` round trips over generated types. Wave three ran three hand-built matrices of up to 199 types through such a harness and found six unsound cases, so the oracle works; what is new is generating the types and letting coverage of `TypeComparers` and `GlbLubs` say what the matrices missed. Ranked lower because every hit needs careful reading before it is a finding.
8. **Parser and scanner round trips.** Print a parsed tree and reparse. Cheap, but the earlier review of the scanner was thorough and the printer is not a faithful inverse, so expect oracle noise.
9. **Pickle round trip and `-Xasync` against a desugared equivalent.** Real oracles, but each needs a larger harness; hold for a second pass.

### Prior art to reuse rather than rebuild

- **Hephaestus and Thalia** (PLDI 2022, POPL 2024): program generators for type checker bugs in JVM languages; Thalia has a `--language scala` mode and reports 84 typing bugs across Scala, Kotlin and Groovy. They look for wrong rejections and unsoundness in typing, not wrong run-time results, so they complement target 2 and could feed target 7. Python; state of maintenance not checked.
- **CrossLangFuzzer** (2025): cross-language JVM fuzzer; reports 2 Scala 2 and 7 Scala 3 compiler bugs. Not examined.
- **rustlantis in Ephyra's rustc harness** is the precedent for giving an existing generator a fixed share of cases rather than replacing it.
- **Wave three's own harnesses**: the type comparer driver and universes under `wave3/reports/types-comparer-harness`, and the generators behind "10,000 generated overload sets" and "8,000 inference cases" in its inference track. Those are seeds for target 7 and show the earlier waves were not wholly without generated inputs.
- **scalacheck `Commands`** is used by three builder suites in the tree; target 1's models could be upstreamed in that form if wanted.
- Java program generators aimed at JIT testing (JavaFuzzer, JAttack, Artemis) are recalled, not verified, and aim at different bugs; the Java track needs declaration-heavy sources, which is a small grammar.

## Not finding the same 500 again

- Hunt on the integration build of draft fixes, as wave three did. Replay every new signature on the unfixed base and on 2.13.18.
- Keep a table from signature to earlier finding id, checked into the wave's directory, and load it as a quarantine list so a known unfixed bug stops masking what is behind it. The spike shows the need: mutable sorted range views (wave-two finding LB2, no fix yet) fail every property on those views.
- Search the two findings lists and the wave-three finding files before recording.
- Expect and want one kind of repeat: a new cell of a known family on the integration build. That is evidence the draft fix is narrower than the defect, and it belongs on the existing pull request rather than in a new finding.

## Spike

Five tracks ran on 26 November: a first collections spike, then four in parallel under `SPIKE-BRIEF.md`. Findings are in `findings/<ID>/` in the format of waves two and three (22 so far), per-track reports in `reports/`, harnesses in `harness/`. `tools/merge_wave4.py` folds `findings/` and `state.json` into the review dashboard, where wave four has its own filter.

### First collections spike

Sources in `harness/collections`, finding GA1, raw summaries in `reports/collections-spike-summaries`.

**What was built.** `SeqDiff`: one random call out of 164 read-only operations, with indices and lengths drawn from the boundaries (−1, 0, length ± 1, the Int extremes), on 31 Seq implementations including differently shaped instances of the same class (a Vector built by prepending, sliced out of a larger one, concatenated; a wrapped-around ArrayDeque; a Queue with both halves in use), with List as the reference. `CollDiff`: programs of one to four calls from the mutating and non-mutating API of buffers, Int sets (sorted, hashed, bit sets, Java wrappers) and maps whose keys collide in hash, against reference models that hold an immutable collection in a var; arguments include the collection itself, its views and its iterators. Both shrink and bucket by signature.

**What it cost.** About two hours including two rounds of fixing the harness's own oracle (a search over duplicates, a stepper split compared piecewise, an exception subclass, an immutable reference for mutating calls, order-dependent picks from unordered sets). Every one of those first-round failures was the test's fault, as Ephyra's guide predicts.

**Calibration.** On the unfixed base the read-only harness rediscovered a wave-two finding (padTo with a length near `Int.MinValue`) inside 90 seconds; on the integration build that signature is gone.

**Results on the integration build.**

| Family | Programs | Implementations | Verdict |
|---|---|---|---|
| Read-only Seq calls | 89,103 in 90 s | 30 | No value differences. Two exception-type differences on empty collections (`Queue.tail`, `LazyList.updated`) |
| Buffers | 2,156 in 40 s | 10 | Self-aliasing defects below |
| Int sets, immutable API | 41,725 in 40 s | 21 | Only the known range-view defect |
| Int sets, mutating API | 164,024 in 40 s | 7 | Only the known range-view defect |
| Maps, immutable API | 31,220 in 40 s | 23 | Only the known range-view defect |
| Maps, mutating API | 63,261 in 40 s | 11 | Only the known range-view defect |

**Defects that survive the draft fixes.** All reproduce on the integration build, the unfixed base and 2.13.18 (finding GA1; its `cmd.sh` prints the table):

- `ArrayDeque(6, -2).addAll(self.view.reverse)` gives `ArrayDeque(6, -2, -2, -2)`.
- `ArrayDeque(0, 1, -1, -1).patchInPlace(3, self.view.map(_ + 1), 65)` ends in `2` where `0` is right.
- `ArrayDeque(1, 4).subtractAll(self.view.slice(0, 2))` throws IndexOutOfBoundsException.
- `UnrolledBuffer(1).prependAll(self.view.map(_ + 1))`, and `++=:`, leave the buffer unchanged.
- `UnrolledBuffer(3, 4).addAll(self.iterator)` and `UnrolledBuffer(4, 6).patchInPlace(2, self.view, 0)` never terminate.
- `UnrolledBuffer(5, -1).addAll(self.view.filter(_ > 0))` appends an element twice; `subtractAll` of a slice of itself leaves an element behind.
- `ArrayBuffer.addAll(self.view)` and `ListBuffer.subtractAll(self.view…)` throw ConcurrentModificationException: failing fast, which is defensible, but it is a third behaviour for the same call shape.

These are new cells of the wave-two family "mutable collections given themselves or a view of themselves" (LB5, LM2, LM3). Those findings named a method and a source each, and the draft fixes handle those pairs. The generator varies both and shows the family is open. The fix should be by class: any source that is not known to be disjoint from the receiver is read before the receiver is changed.

**Coverage.** Under JaCoCo, round one (read-only Seq calls, 40 s) reached 24.2% of the lines of `scala.collection`; adding round two (mutation programs, sets and maps, 150 s) reached 55.3%. The whole existing scalacheck suite reached 47.0% in the review's coverage run of 19 November (a different commit of 2.13.x and its own build, so the comparison is indicative). `reports/collections-treemap.html` shows the two rounds.

### The four parallel spikes

| Track | Generator and oracle | Volume | Findings |
|---|---|---|---|
| `javaparser` | Java 21 sources with a Scala probe; joint compilation against javac's class files, and members typed from source against members read from the class file | 300 cases in 92 s; every finding within the first 70 | GJ1 to GJ4 |
| `collections3` | Ordinary usage of the files coverage showed unreached, against JDK and List models; before the change of direction, Range against exact arithmetic and a re-entrancy matrix | about 22 million checks of ordinary usage | none from ordinary usage; GC1 to GC7 from the earlier part |
| `fold` | Expression trees printed in up to 22 forms (inline literals, hidden operands, boxes, value classes, default arguments, annotation arguments), optimizer on and off | about 3,100 trees, 150 to 370 a minute | GF1, GF2 |
| `erasure` | Small programs in everyday Scala compiled several ways: without `@specialized`, without `AnyVal`, separately, with the other delambdafy mode, with the inliner | under 100 programs at 6 to 14 a minute | GE1 to GE8 |

What the findings are:

- **Java sources (GJ).** A sealed Java interface without `permits` whose implementors are records in the same file has no known subclasses in joint compilation, so a match on it gets "unreachable code" and no exhaustivity warning; 2.13.18 warns correctly, so this is new in unreleased 2.13.19. A varargs record component's accessor is typed `Int*` from source. `@Deprecated` with a constant-expression argument is dropped from source. `enum E { , }` is rejected.
- **Library (GC).** Range and NumericRange iterators compute `knownSize` in a type that overflows, so `(Int.MinValue to Int.MaxValue by Int.MaxValue).iterator.toVector` has one element. `drop` on the iterator of an empty Range makes it non-empty. Mutable `HashMap.updateWith` and `LinkedHashMap.updateWith` store nothing when the function removes the key. GC7 records self-aliasing in sets, maps and `ArrayBuffer.flatMapInPlace` as one open class, with GA1.
- **Expressions (GF).** No wrong value in primitive arithmetic, shifts, conversions, equality, hashing or concatenation. A match whose scrutinee is an `if` with a constant condition and a `Nothing` branch crashes the back end. A block as an annotation argument is accepted and its statements dropped.
- **Programs (GE).** Five crashes or wrong rejections where `@specialized` meets a super call, a by-name override, a partial function literal or a captured local lazy val; the inliner exceeds an allocation budget on a program 2.13.18 compiles; two further crashes are unshrunk.

GA1, GE1 to GE6, GF1, GF2 and three of the four GJ findings were re-run by the coordinating session from their recorded commands and reproduce. None has been read by an independent reviewer and none has a pull request.

### What the spikes say about the method

- **It finds things the earlier waves did not, quickly, when the generator reaches a construct for the first time.** Every track's findings came in its first few dozen to few thousand cases. More cases of the same kind then found nothing: 450 more Java files, 22 million library checks, 3,000 expression trees. Yield comes from new constructs and new oracles, not from running longer, which is Ephyra's own observation.
- **Clean results are results.** Ordinary usage of `Vector`, `ArrayOps`, `StringOps`, the string parsers and the hash tries at volume, and primitive semantics across 22 embeddings, are now measured as sound on the integration build, with coverage figures to say how much was exercised.
- **The findings still cluster on the old seams when the generator is told about them.** The Range, re-entrancy and specialization findings came from parts of the generators written with earlier findings in mind. The Java-source findings and GF1 did not, and those two tracks used the most seam-neutral oracles (a reference compiler; the same expression in many embeddings).
- **Throughput differs by three orders of magnitude.** Library calls run at tens of thousands a second; whole programs with five recompilations at ten a minute. The program track needs the shared in-process driver and a cheaper oracle mix before a campaign, and its general grammar is close to untested.
- **Coverage steering worked where it was tried.** Given the list of unreached library files, one agent took `ArrayOps` from 96 to 484 of 618 lines and `StringParsers` from none to all in two rounds. No compiler track got as far as aiming a round at its own coverage gaps; the gaps are recorded (`CleanUp`, `UnCurry`, the switch emitter, `JavaConstantFolder`, structural calls).

## Campaign

After the spikes, each track ran as a campaign of coverage-steered rounds under `CAMPAIGN-BRIEF.md`, on an integration build that also holds the wave-four fixes (`INTEGRATION.md`). Every finding was then re-run and judged by a reviewer that did not write it (`REVIEW.md`, `reviews/`), turned into a fix or a failing test on its own branch (`FIXBRIEF.md`, `fixes/`), and each fix was read by a second independent reviewer before its pull request left draft. Round records are in `rounds/`, reports and treemaps in `reports/`.

| Track | Rounds | Volume | Findings |
|---|---|---|---|
| Java sources against javac | 4 | 700 generated cases | WJ1 |
| Library files still unreached | 3 | 9.7 million checked calls | WL1, WL2, WL3 |
| Expressions in embedding contexts | 3 | 8,100 cases over 170 contexts | WX1 |
| Generated Scala programs, three campaigns | 15 | 4,650 programs, about 55,000 compilations | WP1 to WP10 |
| Existing run tests as seeds | 4 | 944 tests under seven rewrites | WS1, WS2 |

The findings that matter most:

- **WP3** (retronym/scala#777): `xs.collect { case x => x match { case d: Double => Meters(d); case s: String => try Meters(s.toDouble) catch { ... } } }` compiles and then fails bytecode verification. A regression in 2.13.9 and 2.12.17: erasure boxes a value class around a `try` that uncurry did not lift.
- **WP7** (retronym/scala#785): `(if (ok) Meters(d) else Meters(0)).value` crashes erasure, since 2.11.0. A value-class construction was typed with its argument's own type, so two branches could not be merged.
- **WP5** (retronym/scala#784): a `return` in operand position under `try`/`finally` gives a VerifyError or a back-end crash; scala/bug#10183, open since 2017.
- **WS1** (retronym/scala#778): a lambda whose body type is an existential over local classes is rejected whenever it is expanded into a class, which is always under `-Ydelambdafy:inline` (set by mdoc and Scastie) and for lambdas in super-constructor arguments. A regression from 2.12.0.
- **WL1** (retronym/scala#770): every mutator on a read-only `TrieMap` snapshot throws NullPointerException or spins forever.
- **WJ1** with GJ3 and wave-one J5 (retronym/scala#773): Java annotations with class-literal or constant-expression arguments are dropped from source.
- **WX1** (retronym/scala#774): a structural call to a `Unit` method seen at `Any` returns `null`.
- **WL2**, **WL3**, **WP1**, **WP2**, **WP4**, **WP6**, **WP8**, **WP9**, **WP10**, **WS2**: bounds checks in accumulators, a null key read as 0 through `asJava`, six tree-checker reports, a sequence argument rejected in the branch of a constant `if` (fixed by retronym/scala#750), and a tree-checker cost.

What the campaign added to what the spikes showed:

- **Constructs meeting is where the compiler findings were.** WP3, WP5, WP7 and WP10 each came from families added in different rounds occurring in one program, none from the family a round had just added. A generator that weights such meetings is worth more than one that adds a family and tests it alone.
- **The programs track paid until it did not.** Its first ten rounds gave nine findings; the last five gave one compile error and no miscompile, verification failure or crash in 1,692 programs, with coverage still rising slowly. That is the point to stop a generator and change the oracle.
- **Independent review changed the picture of most findings.** It found WP3 needs no partial function and is a regression, that WL1 hangs where the hunter saw only an exception, that WS1 fails without the flag, that two hunters' mechanisms were wrong, and it rejected two hunters' fix sketches. A finding went to a fix worker only with its review.
- **Diff review caught defects in a third of the fixes.** Of the 26 fix pull requests read so far, 8 came back with a defect or a concern at their first read, among them every first version of the self-aliasing fixes and the constant-`if` fix, whose first three versions each failed review and which ended as a different design. None of those defects showed in the authors' own test runs. The reviewers' most useful habit was compiling the changed sources over a built compiler and comparing class files of the standard library and a few hundred tests against the base.
- **The integration build earns its keep in both directions.** In 1,691 programs of the last campaign, 2.13.18 behaved differently from the integration build in 534; 533 of those were defects fixed by earlier waves, which is evidence for those fixes from inputs nobody chose.
- **Clean results at volume.** Ordinary use of the specialised maps, priority queues, steppers and streams, iterators, `LazyList` evaluation counts, sorting and `BigInt`/`BigDecimal` against `java.math`; evaluation order of an expression in 170 contexts; `-Ydelambdafy:inline` and separate compilation against the default over 4,650 programs; and every existing single-file run test under the optimizer, both delambdafy modes and separate compilation.

Coverage reached by the generated inputs alone, in lines: `scala/tools/nsc/transform` 4,177 of 5,328, the pattern matcher 1,776 of 2,186, `backend/jvm` 2,867 of 3,905; the 31 library files of the library track 5,660 of 7,713; `JavaParsers` 712 of 768.

### What a further pass would do

- A Java-source oracle for Java-facing members of generated Scala programs (the generated program's own reflection dump found nothing; javac compiling a client against the output would be the stricter judge).
- Invalid Java inputs with a both-compilers-reject oracle, which is all that is left unreached in the Java parser and scanner.
- Warnings as an oracle, to reach the pattern matcher's exhaustivity and reachability analysis: the same program with a case removed must warn, and a complete match must not.
- `-Xasync` and the tests that need partest or JUnit classes as seeds.
- Refresh the integration build after each batch of fixes; the last campaign rediscovered one defect only because the build carried an older version of a fix.

## Cost and risks

- **Generator cost dominates compiler tracks.** Ephyra's compiler harnesses took its agent a full run each, and a generator that compiles 60% of its programs wastes the wave. The spikes measure the compile rate before anything longer is planned.
- **Oracle noise.** Every first-round failure in the spike was the test's fault. Budget a round for it in every track and hold to the 1% rule.
- **Under-specified behaviour.** Differences in exception type and in what happens on overflow are real but mostly unfixable; they are bucketed separately and reported as a table, not as findings.
- **Known bugs masking new ones.** Without the quarantine table a track spends its budget rediscovering one unfixed defect.
- **Coverage as a target.** A hunter told to raise coverage will raise coverage. Score by confirmed signatures.
- **Machine.** Eighteen cores and about 47 GB of free disk are shared; two JVMs per track at 2 GB, no sbt in track agents, scratch capped per track. Compiler coverage runs with JaCoCo on the compiler jars slow compilation noticeably; do them once per round, not continuously.
- **Money.** Ephyra's only figure is about $66 and eight machine-hours per target for the closed loop. A track here is one agent session of 45 to 90 minutes plus a verifying read, the same order as a wave-three hunter.

## Decisions for the owner

Each is stated in the body of its pull request; none was made on his behalf.

- retronym/scala#758, #759 (drafts): a buffer given a view or iterator of itself. Copy the source first, as these do, or fail fast as `ArrayBuffer`'s views already do. Copying first makes a source whose termination depends on the receiver growing run forever. #759 also puts a type test for `UnrolledBuffer` in `Growable.addAll` to stay binary compatible.
- retronym/scala#761: the same question for sets and maps, tied to scala/bug#12121; failing tests and three options.
- retronym/scala#749: `updateWith` whose function modifies the map: re-find the entry, or reject as `java.util.HashMap.compute` does.
- retronym/scala#772: a null key through `asJava` on Int- and Long-keyed collections: answer by iterating, or type-test the known collections.
- retronym/scala#756: non-constant annotation arguments (a block, a call of a literal-typed method) are accepted by 2.13.0 to 2.13.18 and rejected by this fix, as by 2.12 and Scala 3; a warning is the alternative.
- retronym/scala#767: the ASM 9.11.1 frame limit that crashes the optimizer on a method with about 190 exception handlers (a regression in unreleased 2.13.19): catch and skip, count handlers in the size estimate, or widen ASM's counter.
- retronym/scala#784: the back-end fix for scala/bug#10183 runs an analysis after emitting each method that returns under a finalizer; the alternatives are tracking stack height during code generation or lifting earlier. With it, retronym/scala#618 is not needed for correctness.
- retronym/scala#773 replaces the approach of retronym/scala#288 and includes the test of retronym/scala#757; those two would be closed in its favour.
- retronym/scala#763 also fixes a separate crash for overloads that differ only in an unspecialized parameter type; it could be split out.
- retronym/scala#774 makes Scala 2 return `()` where Scala 3 still returns `null`; it is labelled for porting.
- retronym/scala#764 adds an on-by-default warning for `def m[@specialized X](q: => X)`, which can fail a `-Werror` build.
- retronym/scala#775 is partial: a primitive-typed match in statement position with one arm that erases to `Object` still fails the tree checker.
- retronym/scala#779: nested binders are not scoped to their own binder, the same limit retronym/scala#459 has for existentials.
- The four `area:generated:*` labels on the fork are unused and can be deleted.

## Not verified

- The treemap and the 37 rounds in the announcement were not reproduced; the round semantics are inferred from the harness notes and the paper's figure.
- The post's totals (535, 373, 313) cannot be checked against the repository, which lists 516 rows with duplicates; no per-system confirmed and fixed counts were found.
- The model and the token cost of ephyra-agent; the false-positive rate of its reports.
- The last quarter of the paper's appendix and the second half of the Datadog post reached this session only as summaries.
- Maintenance state of Thalia, Hephaestus, CrossLangFuzzer, JQF; the Java JIT fuzzers are from memory.
- The scalacheck coverage figure comes from an older commit than the spike's build.
- Reviewers and diff reviewers ran fixed code by compiling the changed sources over a built compiler, not from a full build of each branch; the fix authors ran the tests of the area and some partest categories, never the whole suite. The fork's CI on JDK 8 is the first full run of each branch.
- The allocation bounds asserted in retronym/scala#758 to #760 were measured on JDK 21 and 25, not JDK 8.
- The compile-time cost of the analysis in retronym/scala#784 and of the copy in the self-aliasing fixes on real code was not measured.
- The integration build was smoke-tested and checked against the findings only; it carries the third version of retronym/scala#750, not the final one.
