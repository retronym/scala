# Are type hash codes reproducible across benchmark forks?

Campaign `m4-forkvar-hash` found that `-XX:hashCode=3` cut the fork-to-fork CV of instructions/op from 0.52% to 0.17%. The hypothesis: `Symbol` has no `hashCode`, so its hash is the identity hash, and every type-keyed table (the uniques interner, subtype and lub caches) gets a fork-specific layout. This note tests that directly. A probe dumps every typed tree's `tpe.hashCode` and every slot of the uniques table after each benchmark op, in the real harness: JMH forks of the scala2 lab's `CompileBenchmark`, run with `perflab run` locally on the m5-pro laptop, on the patched JDK 26, with strong chunked uniques (`-Dscala.uniques.layout=chunked`) in every mode.

## Summary

- **The compiler itself is deterministic.** Given the same identity hash stream, every phase from `parser` to `cleanup` consumes exactly the same identity hashes in every fork. All fork-to-fork differences come from outside the compiler's own logic, from three sources:
  1. **JMH iteration boundaries** (mode A only). JMH's own threads take about 23–29 identity hashes at each iteration boundary. With `hashCode=3`, a single global counter, that shifts every later hash. Because iterations are time-boxed, the boundary falls after a different op in each fork.
  2. **A class load requested by the JIT** (all modes). When the tiered policy submits a C2 compile of `scala.tools.asm.ClassWriter.<init>(ClassReader, int)`, `CompileBroker::compile_method` calls `Method::load_signature_classes`. That loads `ClassReader` on the benchmark thread that tripped the C1 counter, and the class load takes 2 identity hashes on that thread. The compiler never uses `ClassReader` otherwise (the constructor is always called with `null`), so this happens once per fork, at a point set by JIT timing: op 27 in some forks, op 33 in others, on scalap.
  3. **The benchmark thread's hash seed** (mode C only). The default `hashCode=5` is a per-thread xorshift. Its seed is `os::random()` at `Thread` construction. That global generator is also advanced by every VM `Symbol` and thread created before the JMH worker, some of them on other threads. So the seed usually matches across forks, but not always.
- **`hashCode=3` (A) is the least reproducible mode.** Every hash on any thread advances its counter, and JMH's threads hash concurrently with the benchmark. It still helped the campaign because between iteration boundaries the counter does run in lockstep, while the default mode (C) gives some forks a different seed.
- **Ids instead of identity hashes (B) make types reproducible across forks and across runs.** This needs `Symbol.hashCode = id`, plus per-run ids for `TypeVar` and `Scope`, and `ClassReader` preloaded. On scalap: 42/42 runs identical across 3 forks, and every run identical to the previous one. With `-Ystop-before:jvm` no preload is needed: 47/47.
- **Seed study (`-Ystop-after:pickler`, 22 seeds).** Without `-Xbatch`, JIT variance swamps the layout (seed explains nothing, p = 0.39). With `-Xbatch`, forks of one seed agree to about 0.1% in instructions/op and exactly in allocation, and typical layouts differ by an sd of 0.37% (p = 0.003 by permutation). One seed in 22 cost 11.7% more, because its hash layout changed which path was hot when C2 compiled `compoundBaseTypeSeq` and blew that compile's inlining budget, not because of hash-table cost. A/B campaigns should pair variants over several seeds.
- **The `scala` corpus has one more source:** the pattern matcher creates a varying number of symbols (±60 per run, run to run and fork to fork, in every mode including B2 with tree ids). From `uncurry` on, the number of unique types then varies by ±20. Typed trees after the typer are bit-identical in B2; the patmat cause is narrowed to its identity-keyed hash maps but not yet pinned down.

## Probe

On branch `claude/hash-determinism-3ab1c6` (based on `scala2-perf-integration`), all behind system properties that default to off:

| Property | Effect |
|---|---|
| `-Dscalac.hashdump=<dir>` | After the typer, record each typed tree and its `tpe`. At the end of the run, before `perRunCaches.clearAll()`, write `<dir>/<pid>/run-<n>.{trees.tsv,uniques.tsv,meta}`. Hashes are read before any `toString`, so printing doesn't move the identity hashes the dump reports. |
| `-Dscalac.hashdump.full=<runs>` | Full TSVs only for these run indices; the other runs write `meta` only, with order-sensitive digests of tree hashes, symbol hashes and uniques `(slot, hash)` pairs (used for the `scala` corpus). |
| `-Dscalac.hashdump.fine=true` | Fingerprints per backend unit, method and class (for locating the `ClassReader` load). |
| `-Dscalac.hashdump.preload=<classes>` | `Class.forName(c, initialize = false)` up front. |
| `-Dscalac.symbolHashById=true` | `Symbol.hashCode = id` (mode B). |
| `-Dscalac.stableHash=true` | `TypeVar.hashCode` and `Scope.hashCode` from per-run counters. |
| `-Dscalac.treeHashById=true` | `Tree.hashCode = id` (also used by `MatchAnalysis.equivalenceHash` now). |
| `-Dscalac.hashSeed=<int>` | All three id modes, with each id scrambled by the seed. |

The flags are static finals in `scala.reflect.internal.util.HashProbe`, so the JIT folds them.

`meta` has a *fingerprint*, `System.identityHashCode(new Object)`, taken at the start and end of the run and after every phase. Under `hashCode=3` this is the global counter's value, so differences between forks show where extra hashes were taken. Under the per-thread modes it identifies the benchmark thread's position in its hash stream.

`tools/hashprobe/run.sh` runs a mode through `perflab run --local` in launch mode; `compare.py`, `digest.py` and `phases.py` compare dumps across forks and runs; `seeds.py` analyses a seed campaign (specs in `tools/hashprobe/campaigns`, for the scala2 lab).

## Modes

- **A**: `-XX:+UnlockExperimentalVMOptions -XX:hashCode=3`, unmodified compiler.
- **B**: `-Dscalac.symbolHashById=true`. **B2** adds `-Dscalac.stableHash=true`.
- **C**: default JVM flags.
- `pre` / `stop`: `ClassReader` preloaded, or `-Ystop-before:jvm`.

## Results: scalap

Each fork does about 43 ops: warmup 2×3s, measurement 3×3s, about 37,000 typed trees and 37,862 unique types per run. "Identical" means every tree's type hash and every uniques slot `(slot, hash)` matches at that run index in every fork.

| Mode | Forks | Runs identical across all forks | Run-to-run (fraction of tree type hashes unchanged from the previous run) |
|---|---|---|---|
| A | 3 / 4 | 35/43, 31/42 | 22.7% (every type that involves a symbol changes) |
| A, `ClassReader` preloaded | 3 | 37/42 | 22.7% |
| B | 3 | 36/43 (diverging runs still have 99.66% of tree hashes equal) | 99.7% |
| B2, preloaded | 3 | **42/42** | **100%** |
| B2 + tree ids, preloaded | 3 | **41/41** | **100%** |
| B2, `-Ystop-before:jvm`, no preload | 3 | **47/47** | **100%** |
| C | 2 | 35/42 (diverges in runs 27–33) | 22.7% |
| C | 3 | 0/42: fork 3 had a different seed; forks 1 and 2 matched 42/42 | 22.7% |
| C, preloaded | 3 | **42/42** | 22.7% |
| C, `-Ystop-before:jvm` | 3 | 0/47: fork 2 had a different seed; forks 1 and 3 matched | 22.7% |

The 22.7% of tree types that never change are the ones with no symbol in them, such as constant types.

**1. Fork to fork, same run index.** A is never 100%. Its runs diverge between JMH iteration boundaries that fall after different ops, and at the `ClassReader` load. B and C diverge only around the `ClassReader` load (runs 27–33). Preloading it makes them 100%, except when C's seed differs. B2 is 100% in every run and fork measured.

**2. Iteration to iteration within a fork.** A and C drift completely from run to run (77% of type hashes change), as expected: the counter or the thread's xorshift keeps advancing. The drift has the same sequence in every fork whose stream started in step. B drifts slightly: 0.3% of tree types and about 12% of distinct unique types (1,486 of 12,129) change between runs. These are `TypeRef`s with `TypeVar` arguments (`TypeVar.hashCode` is an identity hash, scala/bug#7226), `TypeBounds` over refinements, and types containing a `Scope`. Fresh `Global`s per op restart symbol ids, so `Symbol.hashCode = id` alone does not drift. B2 removes the rest: no drift.

**3. First divergence and what took the extra hashes.**
- In A, per-phase fingerprints put every divergence in one of two places. One is *between* ops: offsets of 23, 29, 123 appear and later cancel, at JMH iteration boundaries on other threads. The other is *in `jvm`*: an offset of exactly 2. Per-method and per-class marks put the 2 inside `PostProcessor.serializeClass`, at `new ClassWriterWithBTypeLub`. The class varies from fork to fork: `scalasig/PolyType`, `PolyType$` or `Classfile$Attribute$`.
- `-Xlog:class+load` showed `scala.tools.asm.ClassReader` loaded once per fork, at 18–20s, at that point. `-XX:LogClassLoadingCauseFor=scala.tools.asm.ClassReader` gave the cause: `Runtime1::counter_overflow` → `CompilationPolicy::event` → `CompileBroker::compile_method` → `Method::load_signature_classes` → `SystemDictionary::load_instance_class`, on the JMH worker, under C1-compiled `ClassWriter.<init>(ClassReader, int)`. The interpreter never needs `ClassReader` there, because `classReader` is always `null`. The JIT loads it when it decides to compile the constructor, so the time depends on JIT queue state. B and C show the same divergence in the same runs, because the class load hashes 2 objects on the worker thread itself.
- It is not class unloading during concurrent mark: `-XX:-ClassUnloadingWithConcurrentMark` changed nothing.
- C's seed: `JavaThread` sets `_hashStateX = os::random()` (`thread.cpp`). `os::random` starts at a fixed seed and is advanced by every `Symbol` created in the VM (`symbol.cpp`) and every thread. Run 0's start fingerprint is the same in most C forks and different in some (1 of 3, 0 of 3, 1 of 3 on scalap; 2 of 2 on `scala`).

**4. Uniques table layout.** Where the type hashes match, the chunked table's layout matches too: every `(slot, hash)` pair is identical, and the sum of probe displacements is equal (7,914 in run 0 on scalap, for example). In B, during the diverged runs, 9.4% of unique types hash differently and 13% of `(slot, hash)` pairs differ.

## Results: the `scala` corpus

The `scala` corpus is library, reflect and compiler: 1,135 sources, 1.08M typed trees and 1.54M unique types per run. It needs `-p jdkHome=<JDK 21>`: compiled against the JDK 26 modules, `Global.scala` fails with a name clash between `Comparator.max` (new in JDK 26) and `Ordering.max`. 2 forks per mode, about 13 ops each, digests only.

| Mode | Forks | Tree digest across forks | Uniques across forks | Tree digest run to run |
|---|---|---|---|---|
| A, preloaded | 2 | 6/12 runs | 0/12 runs (sizes differ) | changes every run |
| B2, preloaded | 2 | **13/13** | 0/13 (sizes differ by up to 20) | **unchanged** |
| C, preloaded | 2 | 0/13 (different seeds) | 0/13 | changes every run |
| B2 + tree ids, preloaded | 2 | **12/12** | 0/12 (sizes differ) | **unchanged** |

In B2 every typed tree's type hash and every tree symbol's hash is identical across forks and runs. Even so, the number of unique types at the end of the run varies by about ±20. Per-phase counts show the symbol count first varying after `patmat` (±60 per run, already different in run 0 between forks), with the unique types following from `uncurry` on. So on this corpus the pattern matcher's *output* depends on identity hashes, which is a compiler nondeterminism in its own right, not just a layout effect. The typer's output does not.

The first suspect, `MatchAnalysis.equivalenceHash`, returns `System.identityHashCode(t)` for trees that cannot be equivalent. The probe now routes that case through `Tree.hashCode`, so that `-Dscalac.treeHashById=true` covers it. With tree ids as well, patmat still varies, so that fallback is not the (only) cause. Remaining candidates are the identity-keyed hash maps in `MatchOptimization` (`mutable.HashMap[TreeMaker, ReusedCondTreeMaker]`, `HashMap[Test, …]` and `HashMap[Prop, …]`). The order in which they iterate can decide which tests are shared, and with it which temporaries patmat creates. Not yet pinned down.

## What this means for variance campaigns

- For instructions/op, `hashCode=3` is a weaker fix than it looked. It is reproducible only between JMH iteration boundaries, and the boundaries fall after a time-dependent number of ops.
- Making the hashes the compiler uses depend only on its own counters (`Symbol.id`, plus counters for `TypeVar`, `Scope` and, for patmat, trees) gives bit-identical type hashes and uniques layouts across forks and runs on scalap, independent of JMH, the JIT and the JVM's hash mode. On `scala` the typed trees are bit-identical, but the uniques table still varies through patmat until that is fixed. The cost is a field on `TypeVar` and `Scope` and a static-final check. That is the mode worth measuring for fork CV next. `-Ystop-before:jvm` also removes the `ClassReader` event; otherwise preload `ClassReader`, which the lab could do in `CompileBenchmark`'s setup.
- **Don't overfit an A/B to one hash assignment.** Deterministic hashes remove fork-to-fork noise by measuring one fixed hash assignment, and so one fixed layout of every type-keyed table, over and over. That layout's collisions and probe lengths are a bias, not noise, and no number of forks averages them out. A change in one variant can alter the sequence of hash assignment without having anything to do with hashing: one extra symbol created early shifts every later `Symbol.id`, and one extra identity hash on the benchmark thread shifts the whole xorshift or counter stream. The variant then measures a different layout, and its delta mixes the effect of the change with how lucky or unlucky the new layout happens to be. The same holds for mode A, and for C within forks that share a seed.
- **Test at several seeds.** `-Dscalac.hashSeed=<n>` turns on all the id modes and scrambles each id with the seed: `HashProbe.idHash`, MurmurHash3's finalizer, which is a bijection, so distinct ids keep distinct hashes. This lets a campaign sample layouts deliberately instead of by accident. On scalap with `ClassReader` preloaded, seeds 1 and 2 were each identical across 2 forks (40/40 and 42/42 runs) with no run-to-run drift. Only 3% of unique-type hash values were shared between the two seeds, and the total probe displacement of the uniques table was 7,659 at seed 1, 7,672 at seed 2 and 7,760 unseeded. Each fork gets one seed, and both variants run the same set of seeds, so each comparison is paired by seed. Within a seed, forks are reproducible, so the paired difference has little noise. Across seeds, the spread of the paired differences measures how sensitive the result is to layout. A result that holds at every seed is about the change. One that flips sign between seeds is about hash layout, and should be reported that way, or chased as a layout problem (for example the hash quality of `TypeRef`). This is the measurement-bias problem from Mytkowicz et al., "Producing Wrong Data Without Doing Anything Obviously Wrong!" (ASPLOS 2009). There, the size of the UNIX environment and the link order changed code and stack layout enough to reverse conclusions such as whether `-O3` beats `-O2`. Stabilizer (Curtsinger and Berger, ASPLOS 2013) answered it by re-randomising layout. Here the layout in question is hash layout.
- Remaining sources not addressed here: other `identityHashCode` users in the compiler; `Tree.hashCode` (identity by default) in tree-keyed tables outside patmat; and any JDK class the JIT loads on the benchmark thread through `load_signature_classes` on other corpora.

## Seed study: the `scala` corpus to `pickler`

How large is the layout effect next to fork-to-fork noise? Three local campaigns on the m5-pro laptop, `scala` corpus with `-Ystop-after:pickler` (no patmat, so the compiler's output is identical at every seed: 872,294 symbols and 964,917 unique types per op in every fork), single-shot, patched JDK 26 with the corpus compiled against JDK 25. Every earlier variance reduction is stacked: the perf-integration build with `SCALA_RELEASE_OPT=1`, strong chunked uniques, and an 8 GB heap with a fixed 3 GB young generation and `-XX:-G1UseAdaptiveIHOP`. Instructions/op come from xctrace, for the benchmark threads. The probe runs in lite mode (`-Dscalac.hashdump.lite=true`: per-op fingerprints and the uniques table's layout digest, no tree records), so every fork can be grouped by the hash stream it saw. `tools/hashprobe/seeds.py` joins perflab's per-fork results with the probe's output and does the statistics below.

| Campaign | JIT | Variants × forks | Warmup / measured ops |
|---|---|---|---|
| `m5-hashseed-pickler` | default | identity, hc3, seeds 1–6 × 4 (31 forks done) | 8 / 6 |
| `m5-hashseed-pickler-xbatch` | `-Xbatch` | identity, hc3, seeds 1–6 × 4 | 10 / 6 |
| `m5-hashseed-pickler-xbatch-many` | `-Xbatch` | seeds 7–22 × 2 | 10 / 6 |

**Without `-Xbatch`, the JIT dominates and the seed explains nothing.** Forks of one seed see bit-identical hashes, yet differ by 0.35–1.4% (CV) in instructions/op. Two whole forks ran about 9–12% slower (seed 3: 99.46G, hc3: 96.26G against a median of about 88.6G). Allocation is bimodal and independent of seed: 13 forks allocate about 4.02 GB/op and 18 about 4.12 GB/op, and the high-allocation forks take 0.8% more instructions. One-way ANOVA over the six seeds: F(5,17) = 1.11, p = 0.39; Kruskal–Wallis p = 0.51; intraclass correlation 0.03.

**With `-Xbatch`, each fork is reproducible and the seed matters.** Compilation then blocks the thread that requested it, so the JIT's decisions follow the program's progress rather than compiler-thread timing:
- Allocation per op is identical to the byte across the forks of a seed (CV ≤ 0.0003%), and differs between seeds (4001.6 to 4004.7 MB/op among the typical ones).
- The identity-hash stream is reproducible too. Run 0's start fingerprint matched in all 4 forks of `identity` and all 4 of `hc3`, and every fork of `identity` produced the same uniques layout, in the same per-run sequence.
- Within a seed, instructions/op vary with a median absolute deviation of 0.01–0.36%, typically under 0.1%.
- Over 22 seeds (56 forks), seed medians: 87.997G (seed 22) to 89.707G (seed 7), plus one outlier, seed 20 at 99.167G. Without seed 20 the 21 medians have an sd of 0.37%, an interquartile range of 0.26% and a range of 1.9%. 17 of 21 lie within 0.5% of the median of medians (88.807G).
- The between-seed effect is significant and as large as the noise. On seeds 1–6: F(5,18) = 3.61, p = 0.02 (permutation p = 0.003; Kruskal–Wallis permutation p = 0.018), between-seed sd 0.32% against within-seed sd 0.40%, intraclass correlation 0.39. Adding `identity` and `hc3` as two more layouts: F(7,24) = 4.14, p = 0.004, ICC 0.44. With all 22 seeds the ICC is 0.97, but seed 20 alone drives that.
- The cost of a layout does not track the uniques table's probe statistics. The correlation of seed means with total probe displacement is between −0.12 and 0.31, and the displacement varies by only about 1% between seeds (408,828 to 412,903). The uniques table is not where layouts differ in cost.

**Seed 20: the layout changes the JIT's trajectory, not the hash tables' cost.** Both forks of seed 20 took 11.7% more instructions than the median seed and allocated 101 MB/op more, and a profiling campaign reproduced this to within 0.03% (`m5-hashseed-pickler-prof20`: kperf instructions and cycles, allocation profile and JIT recording, seed 20 against seed 10, 2 forks each). The whole difference, about 10G instructions/op, is inside `BaseTypeSeqs.compoundBaseTypeSeq`, and so is the extra allocation: a `ListBuffer`, its `CheckedIterator` and a lambda allocated there, which never show up in seed 10's allocation profile. The JIT recording explains it. At seed 10 the last C2 compile of `compoundBaseTypeSeq` inlines `nextTypeSymbol$1`, `nextRawElem$1`, `loop$1`, `Objects.equals`, `copyToArray` and the `ListBuffer` operations, and escape analysis then removes the buffer. At seed 20 that compile (earlier in the fork) instead inlined `tpe_*`'s cache-refresh path (10 `TypeRef` constructions with their `computeHashCode`, 27 `MurmurHash3.mix` calls and `Symbol.hashCode`), ran out of budget, and refused almost everything else with `size > DesiredMethodLimit`. That compile was never replaced. Which paths were hot when the compile was requested depends on the order in which hash-keyed structures are visited and filled, so the seed decided the inlining. The hash tables' own cost hardly changed. The 101 MB/op of extra allocation is close to the gap between the two allocation modes without `-Xbatch` (about 100 MB/op), and seed 20's 99.2G is close to the two slow forks above (96.3G, 99.5G). So the same compile outcome may be what JIT timing produces at random without `-Xbatch`. That is a hypothesis: those forks were not profiled.

**What this means for A/B campaigns.**
- `-Xbatch` together with a fixed hash seed makes a fork nearly a deterministic function of the build. Forks of one seed agree to about 0.1% in instructions/op, and allocation agrees exactly. That is the cheapest way found so far to get a precise number, and the earlier `m4-forkvar-gc-confirm` campaign agrees (full pipeline, 6 forks: instructions CV 0.69% at baseline, 0.39% with `-Xbatch` and a fixed young generation).
- Precise is not the same as accurate. The layout is a bias with an sd of about 0.37% across typical seeds, so a single-seed A/B can show about ±0.5% of difference that comes from the layout alone. Occasionally a layout also flips a C2 inlining decision and costs 12%. A change that adds one early symbol re-deals every later hash, so it can move a variant from one layout to another without any real effect.
- So run each variant over several seeds, paired by seed: the forks of a seed are reproducible, so one fork per (variant, seed) pair is enough. Report the median of the paired differences and its spread across seeds, and look separately at any seed where the variants disagree sharply. About 8–10 seeds would bound a typical layout's bias to about 0.1–0.15%. An outlier like seed 20 shows up as one discordant pair, not as a shifted mean.
- Some of the variance that looks like hashing is the JIT. The mechanism at seed 20 (a hot, large method compiled once, early, with a profile that fills its inlining budget with a cold-later path) is a code-shape problem in `compoundBaseTypeSeq` and `tpe_*`. Making `compoundBaseTypeSeq` smaller, or keeping its cache-refresh path out of line, should make it robust to both seed and timing. This is a candidate perf fix in its own right, and it should shrink the no-`-Xbatch` allocation bimodality too, if the hypothesis above holds.

## Related work

None of the studies below separates identity-hash layout from the other causes of differences between JVM processes, and none gives a distribution of fork-to-fork CV for a compiler-sized workload on a quiet machine. They do agree that variation between process executions is the largest level and has to be sampled, not averaged away within a process.

- Kalibera, Bulej and Tůma, [Benchmark Precision and Random Initial State](https://www.cs.kent.ac.uk/pubs/2005/3143/content.pdf) (SPECTS 2005). Each process starts in a random initial state, so averaging many samples from one process overstates precision. The paper gives a way to quantify how much the initial state contributes. Our identity-hash streams, the JIT-requested class load and C's thread seed are concrete examples of that random initial state.
- Georges, Buytaert and Eeckhout, [Statistically Rigorous Java Performance Evaluation](https://www2.ccs.neu.edu/racket/Performance/andy-georges-paper.pdf) (OOPSLA 2007). Multiple VM invocations and confidence intervals across them, partly because different methods get optimised at different tiers in different invocations.
- Kalibera and Jones, [Rigorous Benchmarking in Reasonable Time](https://kar.kent.ac.uk/id/document/3127886) (ISMM 2013). Variance components per level (execution, iteration) and how many repetitions to spend at each. In practice the execution level dominates, and its repetition count cannot be derived from the model.
- Barrett, Bolz-Tereick, Killick, Mount and Tratt, [Virtual Machine Warmup Blows Hot and Cold](https://arxiv.org/abs/1602.00602) (OOPSLA 2017). The broadest study across process executions: 30 executions × 2000 iterations per (VM, benchmark), HotSpot included. Only 30–43.5% of (VM, benchmark) pairs consistently warmed up or stayed flat, and many pairs were inconsistent across their executions. Tratt's [2022 follow-up](https://tratt.net/laurie/blog/2022/more_evidence_for_problems_in_vm_warmup.html) re-analyses related data.
- Traini, Cortellessa, Di Pompeo and Tucci, [Towards effective assessment of steady state performance in Java software: are we there yet?](https://arxiv.org/abs/2209.15369) (EMSE 2023). 586 JMH benchmarks from 30 systems: a considerable share never reach a steady state, and developers' warmup settings are often wrong.
- Laaber, Scheuner and Leitner, [Software Microbenchmarking in the Cloud. How Bad is it Really?](https://research.chalmers.se/en/publication/?id=511491) (EMSE 2019). Java (JMH) and Go benchmarks on AWS, GCE and Azure: CV from 0.03% to over 100% depending on benchmark and instance. Their remedy is test and control on the same instances in randomised order, which is the cloud counterpart of perflab's interleaved and paired variants.
- Mytkowicz, Diwan, Hauswirth and Sweeney, Producing Wrong Data Without Doing Anything Obviously Wrong! (ASPLOS 2009), and Curtsinger and Berger, Stabilizer (ASPLOS 2013). Layout as measurement bias, and randomising it.
- CPython's benchmark tooling: [the 2016 discussion on python-speed](https://mail.python.org/pipermail/speed/2016-April/000335.html). Hash randomisation changed microbenchmark results. Instead of fixing `PYTHONHASHSEED` (the old `perf.py` used seed 1), pyperf runs many processes, each with a different hash seed and memory layout, and averages them. Victor Stinner's [write-up](https://vstinner.github.io/journey-to-stable-benchmark-average.html) settled on at least 25 processes. Fijalkowski's point in that thread is the one in this note: a fixed seed turns a distribution into a single draw, repeated. `-Dscalac.hashSeed` is the same remedy, except that each seed is reproducible, so variants can be paired by seed.
