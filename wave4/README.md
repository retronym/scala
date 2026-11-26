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

### Coverage and the treemap

JaCoCo's agent is already used for this review's coverage runs; attaching it to a harness JVM with `includes=scala.*` and writing one execution file per round is all a track needs. `tools/coverage_treemap.py` turns the XML reports into the picture from the announcement: one panel per round, each rectangle a method sized by instruction count and shaded by the share reached, grouped by source file, with the layout fixed across panels so the eye sees what a round opened up. `--prefix scala/tools/nsc/transform` (or `typechecker`, `backend/jvm`) gives the per-phase view for compiler tracks; `spike-results/compiler-transform-baseline-treemap.html` is that view for what the existing test suites reach, which is the starting panel for the compiler tracks.

Two cautions. Coverage of the compiler from in-process compilation needs the compiler classes on the harness class path to be the ones the report is built from, so build the report from the same `build/pack` jars. And coverage is a gap list: a track is scored by distinct confirmed signatures, with a frozen set of seeds rerun each round as a regression check on the generator itself.

## Targets, ranked

Ranked by expected new findings per unit of effort, given what the spike showed and what the earlier waves already covered. The ranking will move when the parallel spikes below report.

1. **Mutation programs on collections against reference models.** Cheapest and already paying: thousands of programs a second, trivially valid, and the earlier waves fixed this area cell by cell. Extend from buffers, sets and maps to every mutable class and every bulk method, with the collection itself, its views and its iterators as arguments; to Range and NumericRange against exact arithmetic; to iterator chains with extreme arguments. The existing `test/scalacheck` collection suites generate small arbitrary lists and maps and a few builder command sequences; across them there are ten mentions of `Int.MaxValue` or `Int.MinValue`, no self-aliasing and no comparison across implementations.
2. **Generated programs across the erasure fault lines, checked by rewrites.** Value classes, `@specialized`, generics, traits, lambdas and local classes in one small program; the same program with the annotation dropped, with `extends AnyVal` removed, compiled separately instead of jointly, with the optimizer on, with the other delambdafy mode. All must print the same lines. This is Ephyra's OCaml and rustc recipe, and it aims at the two fault lines that produced the most compiler findings by hand. Cost is the generator: it has to compile nearly always to be worth running.
3. **Primitive semantics: folded against evaluated.** The same expression with literals inline, so the typer folds it, and with operands hidden from the folder; boxed against unboxed equality and hashing; optimizer on against off. No second compiler needed, hundreds of expressions per compilation.
4. **Java sources: scalac's parser against javac.** Generated Java sources, with a Scala probe compiled jointly with the sources and then against javac's class files. Two prior families of findings say this boundary is soft; javac is a perfect reference.
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

Built and run in this session; sources in `harness/collections`, reproduction in `repro/SelfAlias.scala`, raw summaries in `spike-results/collections`.

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

**Defects that survive the draft fixes.** All reproduce on the integration build, the unfixed base and 2.13.18 (`repro/SelfAlias.scala` prints the table):

- `ArrayDeque(6, -2).addAll(self.view.reverse)` gives `ArrayDeque(6, -2, -2, -2)`.
- `ArrayDeque(0, 1, -1, -1).patchInPlace(3, self.view.map(_ + 1), 65)` ends in `2` where `0` is right.
- `ArrayDeque(1, 4).subtractAll(self.view.slice(0, 2))` throws IndexOutOfBoundsException.
- `UnrolledBuffer(1).prependAll(self.view.map(_ + 1))`, and `++=:`, leave the buffer unchanged.
- `UnrolledBuffer(3, 4).addAll(self.iterator)` and `UnrolledBuffer(4, 6).patchInPlace(2, self.view, 0)` never terminate.
- `UnrolledBuffer(5, -1).addAll(self.view.filter(_ > 0))` appends an element twice; `subtractAll` of a slice of itself leaves an element behind.
- `ArrayBuffer.addAll(self.view)` and `ListBuffer.subtractAll(self.view…)` throw ConcurrentModificationException: failing fast, which is defensible, but it is a third behaviour for the same call shape.

These are new cells of the wave-two family "mutable collections given themselves or a view of themselves" (LB5, LM2, LM3). Those findings named a method and a source each, and the draft fixes handle those pairs. The generator varies both and shows the family is open. The fix should be by class: any source that is not known to be disjoint from the receiver is read before the receiver is changed.

**Coverage.** Under JaCoCo, round one (read-only Seq calls, 40 s) reached 24.2% of the lines of `scala.collection`; adding round two (mutation programs, sets and maps, 150 s) reached 55.3%. The whole existing scalacheck suite reached 47.0% in the review's coverage run of 19 November (a different commit of 2.13.x and its own build, so the comparison is indicative). `spike-results/collections-treemap.html` shows the two rounds.

**Does it justify going on.** Yes for the library: a few hours bought more coverage than the standing property suite, a calibration hit, and a class of silent corruption the earlier fixes missed. The compiler tracks are the open question; four parallel spikes (targets 1 to 4 above) are running under `SPIKE-BRIEF.md` and report into `spike-results/`.

## Plan for the wave

**Shape.** One coordinator, six to eight track agents in parallel, two passes. The first pass is the spikes: about 45 minutes each, three rounds at most, to learn which generators compile, which oracles are quiet, and what throughput is. The second pass gives the tracks that produced a signature, or whose coverage is still rising, a longer session with the steering loop above, and drops the rest.

**Shared pieces to build once, before the second pass.**

- A compile-and-run driver for program tracks: one JVM, a fresh `Global` and class loader per program, a timeout, the several configurations of a case compiled side by side, output and exception class captured. Wave three's brief already prescribes this shape; make it a file the tracks copy.
- Round bookkeeping: per round the seed, cases run, signatures (new and repeated), oracle discards, the JaCoCo execution file, and the one change made. The treemap script reads the execution files.
- The known-signature table and the integration build, refreshed to the current heads of the draft fixes.
- A fixed set of seeds per track that is rerun after each generator change.

**What the coordinator does between rounds.** Reads each track's new signatures and coverage delta, moves budget toward tracks with rising coverage or new signatures, and has a second agent reproduce each claimed finding from its files alone before it counts. That second read is the false-positive control Ephyra's authors did by hand.

**What comes out.** Finding directories in the wave-three format with seed and signature added; one fork pull request per root cause under the existing labels, or a comment-ready note on the existing pull request when the finding is a new cell of a fixed family; the harnesses themselves, which unlike a hand-written repro can be rerun on every later integration build; and candidates for `test/scalacheck` where a model is small enough to live in the tree.

## Cost and risks

- **Generator cost dominates compiler tracks.** Ephyra's compiler harnesses took its agent a full run each, and a generator that compiles 60% of its programs wastes the wave. The spikes measure the compile rate before anything longer is planned.
- **Oracle noise.** Every first-round failure in the spike was the test's fault. Budget a round for it in every track and hold to the 1% rule.
- **Under-specified behaviour.** Differences in exception type and in what happens on overflow are real but mostly unfixable; they are bucketed separately and reported as a table, not as findings.
- **Known bugs masking new ones.** Without the quarantine table a track spends its budget rediscovering one unfixed defect.
- **Coverage as a target.** A hunter told to raise coverage will raise coverage. Score by confirmed signatures.
- **Machine.** Eighteen cores and about 47 GB of free disk are shared; two JVMs per track at 2 GB, no sbt in track agents, scratch capped per track. Compiler coverage runs with JaCoCo on the compiler jars slow compilation noticeably; do them once per round, not continuously.
- **Money.** Ephyra's only figure is about $66 and eight machine-hours per target for the closed loop. A track here is one agent session of 45 to 90 minutes plus a verifying read, the same order as a wave-three hunter.

## Not verified

- The treemap and the 37 rounds in the announcement were not reproduced; the round semantics are inferred from the harness notes and the paper's figure.
- The post's totals (535, 373, 313) cannot be checked against the repository, which lists 516 rows with duplicates; no per-system confirmed and fixed counts were found.
- The model and the token cost of ephyra-agent; the false-positive rate of its reports.
- The last quarter of the paper's appendix and the second half of the Datadog post reached this session only as summaries.
- Maintenance state of Thalia, Hephaestus, CrossLangFuzzer, JQF; the Java JIT fuzzers are from memory.
- The scalacheck coverage figure comes from an older commit than the spike's build.
