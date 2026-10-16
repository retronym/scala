# compiler-lab: design and plan

## Problem

[scala/compiler-benchmark](https://github.com/scala/compiler-benchmark) answers "how fast is this compiler version on corpus Y": a published or `publishLocal`ed compiler, sources and output on disk, tracked over versions. A compiler engineer's question is different: what did my change do to the machine, and where? That needs an instrument that:

- **runs the working tree**: edit, run, compare against the branch point, with no version string;
- **reaches inside**: `private[nsc]` access to swap the classpath and output for in-memory ones, and to hook the compiler's profiler;
- **measures the compiler and nothing else**: no file I/O in the measured region, counters for the compiler thread rather than JIT and GC threads summed in;
- **explains, not just times**: counters say whether a change saved instructions or stalls; flame graphs and costs per definition say where.

The Scala 3 compiler has such a lab (retronym/scala3 `bench-lab`). Its general parts now live in [retronym/jvm-perf-lab](https://github.com/retronym/jvm-perf-lab): the JMH driver and profilers (JIT stability, paired variants within a fork, xctrace counters per thread group), the caller-audit agent, and the `perflab` CLI (campaigns, live index, flame graph tools, cost-per-definition reports). This lab is the Scala 2 counterpart: the compiler-specific pieces only, on top of jvm-perf-lab.

## Key decisions

1. **A new sbt project, `compilerLab` in `test/compiler-lab`, not the existing `bench`.** `bench` is the library's benchmarks: built with the optimizer inlining from `scala/**`, run with JMH's stock driver. The lab needs perflab's driver as its main class, perflab-jmh on the classpath (Java 17), and fork JVM options naming the corpora. Keeping those out of `bench` leaves the library benchmarks as they are. Like `bench`, it depends on the `compiler` project (quick: the working tree compiled by STARR with the build's optimizer settings); a change to code generation needs `restarrFull` to show in the compiler's own bytecode.

2. **perflab-jmh as a published jar.** For now a snapshot in `~/.m2` (`mise run install` in jvm-perf-lab; `-Dperflab.version` picks another); later JitPack at a jvm-perf-lab tag (`com.github.retronym.jvm-perf-lab:perflab-jmh:<tag>`). The resolver is added to `compilerLab` only, so the rest of the build resolves as before.

3. **One op is one full compile of a corpus in a warm JVM, with no file I/O.** Sources are read once into `VirtualFile`s. The classpath (every module of the running JDK, as scalac sees them without `-release`; the quick library, reflect and compiler, as compiler-benchmark puts the Scala jars on every corpus's classpath, except for the `scala` corpus, which compiles them; and the corpus's dependencies) is read once into `VirtualDirectory`s and installed as the platform's classpath (`JavaPlatform.currentClassPath`), bypassing `PathResolver` and the process-wide jar and jrt caches. Output goes to a `VirtualDirectory`, so the backend still writes class files (`VirtualFileWriter`), just not to disk. Each op makes a fresh `Global` and `Run`, so nothing carries over between ops (in Scala 2 even the name table is per `Global`); `-p global=resident` reuses one `Global` with a new `Run` per op, as a compile server or the REPL does.

4. **Corpora from a scala/compiler-benchmark checkout**, by default beside this one (`$COMPILER_BENCHMARK` otherwise): `corpus/<name>/latest`, read as compiler-benchmark reads it: `//#if 2.13` filter lines applied for this compiler's version, and the jars named in `deps.txt` downloaded once into a cache under `target/` and then held in memory with the rest of the classpath. The `scala` corpus (library, reflect, compiler) is the most representative; `scalap`, `re2s` and the small stress corpora `map-seq-inference` and `patmat-exhaust-huge` cover narrower workloads. The others do not compile with 2.13 on JDK 17+: `better-files` needs `javax.xml.bind`, `vector` is 2.12's collections, `scalactic-position` calls a 2.12 macro, and `induction` overflows a 2 MB stack. An absolute path compiles the sources under any directory.

5. **Variants within a fork are scalac options.** `-p variants='base=|new=-Yfoo'` (perflab's `Variants`, `IterationLog`): each iteration compiles with the next variant in a Williams order, so variants share the fork's compiled code and `PairedProfiler` and `XctraceProfiler` report paired differences. Since every op builds a new `Global`, switching costs nothing. Settings compared this way must be read per `Global` (scalac options are; system properties read once per JVM are not).

6. **perflab drives runs; the build keeps no custom commands.** `perflab.toml` names `sbt --client 'compilerLab/Jmh/run {args}'` with an sbt server, and perflab passes its JMH arguments as an `@file`, so the `;` between profiler options never reaches sbt's command parser. `perflab run --profile none|async:<events>|xctrace:<template>|audit:<spec>`, `perflab ab`, `perflab serve` and the analysis commands then work as on any perflab project.

7. **Cost per definition builds on the profiler hooks `-Yprofile-trace` already uses.** `Profiler` has hooks for phases, units, typed class/module/def/val definitions, implicit searches, macro expansions and classfile completions (`SymbolLoaders`); `-Ystatistics` has stackable timers giving exclusive time by tree class. A `DefCosts` stack in `scala.tools.nsc.profile` follows the same hooks with one frame per hook, keyed by symbol, charging each frame its self time (as stackable timers do, but per definition). One hook is missing and gets added: Namer completions (`TypeCompleterBase.complete`, so an inferred result type typed on another definition's stack is charged to its own definition). Work after the typer is charged to its unit for now: Scala 2's transforms have no common per-member seam (Scala 3's MegaPhase has one), so a per-member hook would mean touching each phase's transformer. The typed-definition hooks that fire after the typer (erasure's retyping) are `transform` frames. Two consumers, as in Scala 3: `-Yprofile-defs:<file>`, a table in the format `perflab defcosts` reads (kinds `phase`, `unit`, `def`, `complete`, `load`, `implicit`, `macro`, `transform`), and `-Yprofile-defs-listener:<class>`, a listener that `compilerLab`'s `SpanCostListener` uses to record each frame as an async-profiler span, for `jfrconv --spans` flame graphs broken down by definition. With neither option, a hook costs what it costs today. This is a compiler change and goes upstream as its own PR.

## Prior art

- retronym/scala3 `bench-lab` (`CompileBenchmark`, `InMemoryCompiler`, `InMemoryClasspath`, `SpanCostListener`, `DefCosts`), which this ports.
- scala/compiler-benchmark's `HotScalacBenchmark`: the corpora, and the fresh-vs-resident distinction.
- `-Ystatistics` stackable timers and `-Yprofile-trace` Chrome traces: per tree class and per occurrence; the cost table adds per-definition aggregation and exclusive time across completions.

## Plan

- DONE 0. This plan, in a draft PR.
- DONE 1. `compilerLab` sbt project: depends on `compiler`, perflab-jmh (JitPack or `~/.m2`), `Jmh / run / mainClass` = perflab's driver, fork options naming the corpora and quick's library classes; a Java 17+ check.
- DONE 2. `CompileBenchmark` (`corpus`, `scalacOptions`, `global=fresh|resident`, `variants`), `InMemoryCompiler`, `InMemoryClasspath`, `Corpus`; `CompileLoop` for running outside JMH with the live heap per run. Checked with JFR file events over 10 runs of scalap: after the first run, the only file reads are two lazy class loads by the JVM. The live heap after each run stays flat (211 MB).
- DONE 3. `perflab.toml`; `perflab run` under each profile, a sample paired campaign (`campaigns/`), `perflab serve`. README with how to run.
- 4. Profiles.
  - DONE async-profiler (`async:cpu,alloc`), xctrace (`xctrace:ipc`: 2.93 G instructions/op on scalap, IPC 2.71 on the benchmark thread), caller audits with the perflab agent (`audit/as-seen-from.audit`: 460 k `asSeenFrom` calls per scalap compile, 6.3% under implicit search).
  - TODO kperf events through retronym/async-profiler `integration-kperf-flamegraph`; a predicates helper compiled against the compiler, for audit conditions (`when=`).
- 5. Cost per definition.
  - DONE `DefCosts` and the Namer completion hook in the compiler, `-Yprofile-defs` and `-Yprofile-defs-listener` (JUnit `DefCostsTest`); `SpanCostListener` in the lab; the `macro` kind in `perflab defcosts`. On scalap the table accounts for 532 ms of self time in a 555 ms compile (61% of it in units after the typer, at unit granularity). Spans: 99.2% of CPU samples fall in a span; with kperf (`SpanCostListener$All`, see below), 0.9% of instructions do not.
  - DONE Overhead, paired within forks (scalap, 3 forks, `campaigns/def-costs-overhead.json`): the listener with no recording +0.2% ± 1.8; writing the table every op +4.2% ± 0.2.
  - TODO With the options off, against the branch point across builds (`perflab trees`). Off, the run's profiler is still `NoOpProfiler`; the change adds one call to its empty `beforeNamerCompletion`/`afterNamerCompletion` per source completion.
  - TODO Per-member frames in the post-typer transforms.
  - TODO async-profiler: advance the sampled thread's last-sample time for kperf samples (`FlightRecorder::recordEvent` updates the recording thread's, and skips `HW_SAMPLE`), so `SpanCostListener` can drop unsampled spans under kperf too.
- Future work: phase-level benchmarks (typer only, backend only from captured trees), operation-level micro benchmarks (subtyping, implicit search, `asSeenFrom`) with recorded inputs, output digests to check two builds generate the same bytecode, Linux parity (perf counters).
