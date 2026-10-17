# compiler-lab

Benchmarks and profiles of the working tree's compiler (quick), for answering what a change did to the machine and where. It keeps the compiler-specific parts (an in-memory compile benchmark, corpora, compiler hooks); running, comparing and profiling are [retronym/jvm-perf-lab](https://github.com/retronym/jvm-perf-lab)'s, through its JMH driver and the `perflab` CLI. Design and plan: [doc/design.md](doc/design.md).

## Setup

- A [scala/compiler-benchmark](https://github.com/scala/compiler-benchmark) checkout for the corpora: beside this one, or named by `COMPILER_BENCHMARK` when sbt starts.
- jvm-perf-lab: `mise run install` there puts perflab-jmh and the audit agent in `~/.m2`, and `mise run cli` installs `perflab`.
- sbt on Java 17 or later (perflab-jmh's baseline); the benchmark forks use the same JVM. Start a server in this checkout (`sbt --client`), which `perflab` drives.

## CompileBenchmark

One op is one full compilation of a corpus with a new `Global`, in a warm JVM, with no file I/O: sources, classpath (the JDK's modules, the quick library, reflect and compiler, the corpus's jars) and output are all in memory. Parameters:

- `corpus`: `scalap`, `re2s`, `scala` (library, reflect and compiler), `map-seq-inference`, `patmat-exhaust-huge` from compiler-benchmark, or an absolute path to a directory of sources.
- `scalacOptions`: extra options, space separated, e.g. `-Ystop-after:typer`.
- `global`: `fresh` (default) or `resident`, one `Global` for every op with a new `Run` each, as a compile server.
- `variants`: settings compared within one fork, `base=|new=-Yfoo`, alternating by iteration so the variants share the fork's compiled code; reported as paired differences.
  A variant can also name a build, `base=@build=../base|new=@build=.`: that checkout's build/quick reflect and compiler, loaded in a class loader of its own along with the lab's classes. This pairs builds within a fork too, for changes to the compiler's own code that no flag selects; across separate forks (`perflab trees`), ms/op varies by several percent from fork to fork, which takes many forks per build to see through. The builds share the benchmark's scala-library (and scala-asm), which must be byte-identical to theirs, and they share the JIT's queue, code cache and the profiles of library code; `campaigns/builds-aa.json` checks that two loaders of one build measure the same.

Every run has perflab's default profilers: `gc` (allocation per op) and `JitProfiler` (compilations and deoptimizations while measuring; if `jit.compilations` is large, warm up longer).

`CompileLoop <corpus> <runs> [scalac options]` compiles in a loop outside JMH, printing each run's time and the live heap after it, for attaching other tools:

```bash
sbt "compilerLab/Jmh/runMain scala.tools.nsc.compilerlab.CompileLoop scalap 20"
```

## Running with perflab

`perflab.toml` at the root of the checkout tells `perflab` to run `compilerLab/Jmh/run` through the sbt server, with its arguments in a file, so profiler options containing `;` never reach sbt's command parser. Output goes to `out/compiler-lab`.

```bash
perflab serve    # the live index of runs and campaigns; open its URL first
perflab run -- -p corpus=scalap CompileBenchmark
perflab run --profile async:cpu,alloc -- -p corpus=scalap CompileBenchmark
perflab run --profile xctrace:ipc -- -p corpus=re2s CompileBenchmark
perflab ab run test/compiler-lab/campaigns/paired-aa.json && perflab ab report test/compiler-lab/campaigns/paired-aa.json
```

Profiles are `none`, `async:<events>` (async-profiler; kperf PMU events with retronym/async-profiler's integration branch), `xctrace:<template>` (Apple Silicon counters for the benchmark thread) and `audit:<spec>` (caller audits with perflab's agent). Campaigns (`campaigns/*.json`) run variants over corpora round by round, or paired within forks, and check that each run measured what it claims. `perflab trees` compares builds in several worktrees, each in its own forks; `@build=` variants compare them within forks. See jvm-perf-lab's README for the analysis commands.

## Cost per definition

Two compiler options (`scala.tools.nsc.profile.DefCosts`) keep a stack of frames in step with the profiler's hooks (phase, unit, typing a definition, its Namer completion, classfile loads, implicit searches, macro expansions), charging each frame its self time, so an inferred type typed ahead on another definition's stack is charged to its own definition. Work after the typer is charged to its unit, except erasure's retyping (`transform`).

- `-Yprofile-defs:<file>`: a table per kind, phase and definition (calls, self and total µs, self allocation, size), for `perflab defcosts`:

  ```bash
  sbt "compilerLab/Jmh/runMain scala.tools.nsc.compilerlab.CompileLoop scalap 10 -Yprofile-defs:$PWD/out/compiler-lab/scalap.tsv"
  perflab defcosts --out out/compiler-lab/scalap.html out/compiler-lab/scalap.tsv
  ```

- `-Yprofile-defs-listener:scala.tools.nsc.compilerlab.SpanCostListener`: each frame as an async-profiler span, for `jfrconv --spans` flame graphs with a span panel by definition, phase or unit. Needs retronym/async-profiler's integration branch, for the library (`perflab run --lib`) and `jfrconv`:

  ```bash
  perflab run --profile async:cpu --lib <libasyncProfiler> -- -p corpus=scalap -p scalacOptions=-Yprofile-defs-listener:scala.tools.nsc.compilerlab.SpanCostListener CompileBenchmark
  jfrconv --cpu --spans --threads -I '.*jmh-worker.*' <run>/<benchmark>/jfr-cpu.jfr spans.html
  ```

  With kperf events (`--kperf <socket>`, `async:cycles+instructions@1000000`), use `SpanCostListener$All`, which records every span: async-profiler does not yet tell the sampled thread about kperf samples, so the default listener would drop them all. Render with `jfrconv --hw cycles` (or `instructions`) in place of `--cpu`.
