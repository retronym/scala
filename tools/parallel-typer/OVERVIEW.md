# Concurrent readers over one symbol table

A Scala compile is one thread walking one mutable symbol table. This is the case for letting several threads walk it at once, with the evidence from two compilers and one build. It is the entrance to three deeper pages:

- [The parallel typer x-ray](https://claude.ai/artifact/Fiw4X4GmXv9xdkRTaq8dfF): one Scala 2 compile on N threads, with timelines, profiles and the ceiling the machine sets.
- [Pipelined build scaling](https://claude.ai/artifact/3WkxG4ro95NpnWyAmJ55Gv): how a clean build of ScalaTest should split its cores between compiles and threads.
- [Scala 3, interactive: retronym/scala3#84](https://github.com/retronym/scala3/pull/84): the same discipline in dotc's presentation compiler, with the Lean model, the race catalogue and the state map.

The Scala 2 work is in [retronym/scala#217](https://github.com/retronym/scala/pull/217), with `PLAN-parallel-typer.md` as its lab notebook. Scala 2 is in maintenance; it was chosen as the second compiler because the design had to transfer to be worth anything, and because scalac is small enough to put every phase on a pool and measure the whole compile.

## The question

A machine has sixteen cores and a build has forty modules. The obvious way to use the cores is to compile sixteen modules at once, each on the serial compiler, and every build tool does that already. So why make the compiler itself parallel?

The answer comes from measurements that do not depend on which compiler is under test.

**N serial compiles are not N times faster.** Eight independent compilers running at once, each with its own symbol table and nothing shared, are the ceiling: no coordination, no locks, the best case for module-level parallelism. On this machine (an M5 Pro, 6 performance and 12 efficiency cores, 48 GB) they reach 4.6× to 4.9× over one compile in a 12 GB heap, and 6.1× to 6.5× in the best heap found. The gap to 8× is the collector and the memory system. Each compile allocates about 3 GB and keeps a few hundred megabytes live; eight of them in one heap turn one young collection per compile into fifteen, each copying four times as much, and stop-the-world time is 70 to 81 percent of what the eight add over one compile. With a 16 GB young generation most of that goes, and what is left is hardware: 14 to 21 percent more CPU per compile for the same instructions, at a lower IPC and clock. Eight compiles need a 5 GB heap between them before full collections stop; in 3 GB the compiler corpus takes thirteen times longer and some compiles fail.

**Real module graphs have a dominant module.** Across the projects sampled, 23 to 43 percent of the code sits in one module. Sixteen serial compiles cannot shorten the module that holds a third of the work. On ScalaTest, 1.54 million lines in 48 modules, 8 serial compiles reach 3.0× and 16 reach 2.65×, slower than 8, because every extra compile adds a live set for the collector to copy while adding nothing to the one module everyone waits for.

So the cores are better spent in a mix. On the same build, 4 compiles on 2 threads each reach 4.79× at 8 cores, and 4 compiles on 4 threads reach 6.71× at 16, against 3.35× for 8×2 and 2.65× for 16×1. Threads add CPU and no live set; compiles add both. The rule that fits so far is compiles = threads = √cores, and it picks the winner at both core counts tried.

The rest of this page is what it took to make one compile use several threads, and how far that got.

## One compile on N threads

`scalac -Yparallel-typer` runs every phase, parser to bytecode, on a pool of workers, with units as the tasks. At 8 threads, with sources, classpath and output in memory, median of three unprofiled compiles on a quiet machine:

| corpus | serial | 8 threads | speed-up |
|---|---|---|---|
| pekko (stream, 1 module) | 4257 ms | 952 ms | 4.5× |
| scalac itself | 5293 ms | 1207 ms | 4.4× |
| junit tests | 3461 ms | 946 ms | 3.7× |
| scala-library | 3658 ms | 1076 ms | 3.4× |

Against the independent-compile ceiling in the same heap (4.6× and 4.9× for pekko and the compiler), the parallel compiler is at 4.3× and 4.5× on the same baseline, so it is within ten percent of what eight uncoordinated compilers get from this machine, with one live set instead of eight: it stops for about 100 ms per compile, what serial does. Where it falls short of the ceiling is CPU: it spends 24 to 29 percent more non-GC CPU than eight independent compiles, diffuse across the profile, and the one-thread run is 4 to 15 percent slower than the serial compiler. That is the room left, and the x-ray's profile comparison is the tool for finding it.

The x-ray records a compile with async-profiler under a patched build that attributes samples to spans, and shows, per thread and per instant, which unit and phase it was in, what it waited on and who held it, where the CPU and the allocation went, and the Amdahl bound from what ran on one thread by design. The claims above about why the compiler is not 8× come from that page, as did the causes behind most of the fixes (a convoy on the completion lock, a serial class-header step that held the speed-up at 2.4×, a 100 ms GC pause that looked like a lock wait).

## What it took

The design decisions below are the same in both compilers.

**Completions are the unit of mutual exclusion.** A symbol's info is computed once, by the thread that claims it; every other thread that needs it waits on that claim. The claim is per completion root, not per symbol: a classfile loader enters a class and its module together, a TASTy file's completers share one unpickler, so the root is the smallest unit a completion writes. Coarser roots mean waits that are lock orders rather than dependency cycles, and the victim rule has to tell them apart; finer roots mean making the loaders safe for several readers for waits that already total milliseconds per compile. A Lean model of the protocol (`retronym/jvm-lang-models`) gives the correctness argument: with completions owned and a barrier for everyone else, every value a reader stores is the sequential fixed point's, under every interleaving.

**Per-thread scratch, chosen at construction.** The typer's scratch state (the phase and period, context pools, recursion guards, the diagnostics' renaming, the member-type cache) moves to a per-thread holder reached through one virtual call that a batch JVM devirtualizes. Caches and interners are chosen once, when the symbol table is built, as concurrent or plain. The serial compiler never loads the concurrent classes, and the A/B against the stock compiler is inside the noise.

**Signatures first, bodies on the pool.** The class headers are completed on one thread before the bodies are typed in parallel, so the bodies rarely wait on a completion. That one-thread prefix is 3 to 8 percent of the compile at 8 threads; Amdahl then allows about 7×, and the gap from there to 4.5× is CPU, not waiting.

**The info protocol stays the serial one.** Type histories, phase travel, info transformers and `asSeenFrom` are unchanged; what changed is which thread may write an entry and when another may read it. The races in it are a reader taking a state another thread has published half way for a finished one (`initialize` reading a provisional info, `adaptInfo` writing its run before its history), and the fixes are a few lines each.

**Determinism by oracle.** Class files from an 8-thread compile are compared byte for byte with the one-thread compile, over hundreds of runs per corpus, and the one-thread compile with serial. The residue (fresh-name counters, and one import-order dependence the serial compiler also has) is catalogued, not hidden.

The cost of getting there is in the race catalogues, I1 to I98 in #84 and I1 to I21, L1 to L4 and O1 to O3 in the plan, each entry with its mechanism. Nearly all were found by the two-reader soak and the oracle rather than by reading code. Most are a peek at another thread's completion in progress, a cache written from a transient state, a symbol renamed for the length of a message, or an unpublished period.

## What transfers, and where it stands

The Scala 3 side came first and aims at latency rather than throughput. With `-Yinteractive-parallel`, a background pass types `Types.scala` in 560 to 850 ms while hovers beside it stay at 1.2 ms median, where under one lock for every completion a hover waited 141 ms; eight readers typing all 484 files of dotc reach 2.7× one reader, with root waits of a few milliseconds per run. The open item there is a victim rule that mistakes a lock order for a dependency cycle at eight readers (I68). What the Scala 2 port adds is the batch result: that the same claims, scratch and protocol carry every phase of a whole compile at 4.5×, and the x-ray and the build harness that say where the rest went.

What a build tool should do with this is still open. The build page's model says workers that move between compiles would gain little on ScalaTest, because a compile cannot change its thread count once started; that is the compiler-side change a real scheduler needs. The square-root rule has been tried on one project at two core counts. A real clean build starts cold and the first build of a JVM is about 15 percent slower than the warm ones measured.

Some pieces go upstream now, independent of any flag: the analyzer cake as a class (scala/scala#11367, merged, which took an interface-dispatch stub from 8 percent of the profile to 1), the `nextFrom` lookup that, in a run stopped before the first info transformer, invalidated every base type sequence at each switch between namer and typer and cost a partial compile of the library a third of its allocation, the member-type cache that the serial compiler also switches off inside completers, and the package-level import whose registration makes the stock compiler's output depend on unit order. On the Scala 3 side, the mtags queue-reset bug, the unguarded 16-bit run id and cancellation that reaches the typer.

## Caveats

One machine, 6 performance and 12 efficiency cores, so the independent-compile ceiling and every speed-up above are that machine's. Warm JVMs throughout. Classpath, sources and output in memory, which removes this laptop's slow disk and the file-system half of a real build. ParallelGC with a 12 GB heap for the single-compile figures and 31 GB with a 20 GB young generation for the build; GC settings moved the build results by as much as the split did. The one-thread parallel compile is 4 to 13 percent slower than serial. The oracle's residue is small but not zero. Scala 2 is the spike; the design decisions are shared with Scala 3, the numbers are not.
