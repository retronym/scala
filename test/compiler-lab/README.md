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

Profiles are `none`, `async:<events>` (async-profiler; kperf PMU events with retronym/async-profiler's integration branch), `xctrace:<template>` (Apple Silicon counters for the benchmark thread) and `audit:<spec>` (caller audits with perflab's agent). Campaigns (`campaigns/*.json`) run variants over corpora round by round, or paired within forks, and check that each run measured what it claims. `perflab trees` compares builds in several worktrees. See jvm-perf-lab's README for the analysis commands.
