# Parallel typer x-ray

A page that shows what each thread of a `scalac -Yparallel-typer -Ystop-after:pickler` run did and when, so that the serial parts and the waits stand out: the stretches where at most one thread works, why (a serial step, a convoy on the completion lock, the tail of the bodies, GC), which holds of the lock others waited on, and how the run scales with the number of threads. It ports the batch and thread-scaling views of the Scala 3 x-ray (`presentation-compiler/test/dotty/tools/pc/xray` in retronym/scala3, `ParallelBatch.Timeline`) and adds the lock's holds and waits, which Scala 3 does not record.

![4 threads on src/library](xray-library-4.png)

![8 threads, 4 ms of the bodies: every worker waiting, the holds short](xray-library-8-zoom.png)

## Running it

```bash
sbt dist/mkQuick                                   # once
tools/parallel-typer/xray/run.sh library           # or reflect, compiler, a file of scalac arguments
open tools/parallel-typer/xray/index.html
```

`run.sh <corpus> ["1 2 4 8"]` builds the corpus's arguments as `oracle.sh` does, compiles `XrayBench.scala` against a snapshot of `build/quick`, and runs one JVM (`-Xss64m -Xms6g -Xmx6g`, plus `JAVA_OPTS`) that compiles the corpus `WARMUP` times (default 14, cycling through the configurations, unrecorded), then once per configuration with the recording on: `serial` (no `-Yparallel-typer`, the baseline of the speed-ups) and each thread count; the default list is 1 2 4 8 and, on Apple silicon, the number of the fastest cores (`hw.perflevel0.physicalcpu`). It writes one JSON per run under `$OUT` (default `/tmp/parallel-typer-xray/<corpus>`), all of them as `timeline.data.js` beside the page, and prints a text summary (`summary.js`, which runs the page's own analysis under node). `OVERHEAD=<pairs> run.sh library 4` instead alternates compilations with the recording off and on and prints the medians.

A single run from the command line: `-Dscalac.parallelTyper.xray=<file.json>` writes the recording of each `Run` to that file. The page loads `timeline.data.js` from beside it (it works from `file://`), and *Load…* or a drop on the page adds run JSON files or another `timeline.data.js`; a run replaces a loaded run of the same corpus and configuration.

## Reading the page

The buttons at the top pick the run; the numbers under them are its wall time, its speed-up against the serial compiler, the time outside the bodies (everything that runs on the main thread only), the bodies' wall time, the time during which at most one thread did CPU work, the summed lock wait, and the speed-up Amdahl allows at this thread count (see below).

**Parallelism over time** is the first thing to look at. The blue area is the number of threads doing CPU work at each instant: each span counts with the ratio of its thread's CPU time to its wall time (so a descheduled or monitor-blocked thread counts less), and a thread waiting for the completion lock counts as zero. Where at most one thread works, the background is shaded by the cause and labelled with its duration: a serial step (setup, parser, namer, packageobjects, the signatures pre-pass, the annotation sub-pass, lazy-val forcing, superaccessors, extmethods, pickler), a *lock convoy* (during the bodies, one thread working and the others waiting for the lock), the *tail* (the other workers are out of units), or GC. Stretches too narrow to shade show in the ribbon under the chart. The chips under it total each cause over the run; a click zooms to its longest stretch. The chart shares its zoom with the timeline.

**Timeline**: a lane per thread (main, then the workers, then GC pauses), the phases above them. A span is a unit in a step, coloured by step; its opacity is its CPU time over its wall time. Over the spans, the lock: a dark band is a hold (solid when someone waited on it, faint when it only took longer than the threshold), red is waiting for the lock (hatched when wide enough). When serialisation sets in it looks like a staircase: one lane dark, the others red. Zoomed out, where a wait or hold is narrower than a pixel, each pixel column shows the share of its time spent waiting (red, by opacity) and holding (dark); zoomed in below 20 µs a pixel, each wait and hold is drawn. Hover shows the unit, the step, wall and CPU time, the lock time inside the span and the ThreadMXBean blocked/parked time; on a wait, the site, what it was completing and the holds it spanned (thread, site, symbol). Click a wait to outline it and the holds it waited on, joined by lines. Wheel or pinch zooms, horizontal scroll or shift-wheel or drag pans, double-click resets; the minimap under it shows the whole run. The step chips hide steps; *lock site* keeps one site's holds and waits (and draws them one by one at any zoom).

**Where the time went**: the run's wall time as one bar. Outside the bodies, wall time by step. Inside the bodies (from the first worker span to the last worker running out of units), each worker's time divided by the number of workers: *work* (CPU in a unit), *hold, others waiting* (work while holding the lock with another thread queued), *descheduled or blocked* (in a unit, neither on CPU nor waiting for the lock), *lock wait*, *idle worker*, and GC. The note under it gives the serial fraction and the Amdahl bounds, and the measured speed-up.

The Amdahl bounds come from the one-thread run: `S` is its time outside the bodies, `P` its bodies, `H` the time its units held the lock. The bound at `N` threads is `(S + P) / (S + P / N)`, and counting the lock as serial too, `(S + P) / (S + H + (P - H) / N)`. The second is the honest one while every completion takes one lock.

**Tables**: lock sites (outermost acquisitions, total and longest hold, waits, total and longest wait, how many threads waited there; a click filters the timeline), *waited on* (the holds others waited on, grouped by what the holder was completing and its site, with the wait each caused: the overlap of every wait with every hold it spanned), the slowest units of the bodies, and the units by time under the lock (a click zooms to the unit).

**Thread scaling**: every loaded run of the corpus as a bar on one time scale (so the bars shrink as the run gets faster), a speed-up plot against the serial compiler with the ideal line and the two Amdahl curves, and a table of the components per run.

## What is recorded

`scala.reflect.internal.util.ParallelTyperTimeline`, installed in `ParallelTyperTimeline.current` (null otherwise; every hook tests it and does nothing else). The hooks are lines marked `// xray`: `CompletionLock.acquire`/`release` (and `acquire(what)`, called with the symbol by `Symbol.completeInfoUnderLock`), `Global.applyPhase`, the phase loop of `Run.compileUnitsInternal`, and in `ParallelTyper` the pre-pass per unit, the annotation sub-pass, lazy-val forcing and a worker finding the queue empty.

- Spans: `Global.applyPhase` per unit and phase (the typer's bodies on the workers are the `typer` spans), the pre-pass (`signatures`, per unit), `annotations` and `lazy vals` (one each). Wall time, the thread's CPU time (`ThreadMXBean.getCurrentThreadCpuTime`), its blocked and waited times (`ThreadInfo.getBlockedTime`/`getWaitedTime`, milliseconds, with thread contention monitoring switched on while recording: blocked is entering a monitor, which covers the reporter, `uniques`, the name table and lazy-val initialisers; waited is parked, which includes the completion lock), and the completion lock's wait and hold time on the thread inside the span. Main-thread time outside any unit is added by the page (setup before the first phase, a phase's own work outside its units).
- The lock: a call site is the first frame outside the lock's plumbing (`StackWalker`, labels cached per class and method). A walk costs a few microseconds, so it is taken for one outermost acquisition in 16 (`scalac.parallelTyper.xray.siteSample`), before every wait, and after the release of every recorded hold; never while the lock is held. Per site: acquisitions, hold total and maximum, waits, wait total and maximum, the set of threads that waited. Every wait: thread, site, the symbol it was for (only `Symbol.info`'s completions name one), start and end. Every hold that took longer than `scalac.parallelTyper.xray.holdMicros` (default 50) or had a thread queued at its release (`ReentrantLock.hasQueuedThreads`: such a thread arrived during the hold); a wait's holder is then found as the recorded holds that overlap it, which is exact because the lock's holds are disjoint and a hold someone waited on is always recorded. Each thread keeps its own holds and waits (a hold is written after the unlock), and the per-site totals are summed when the JSON is written: exact for waits and recorded holds, estimated from the sample for the short uncontended holds.
- Phases (main thread), GC pauses (`com.sun.management.gc.notification`, concurrent cycles excluded), and the moment each worker found no more units (idle until the typer phase ends).
- Metadata: corpus, configuration, flags, JVM and its arguments, processors, heap, collectors, OS, and on macOS the CPU and the core counts per performance level (`sysctl hw.perflevelN`).

`-Dscalac.parallelTyper.xray.sites=false` skips the stack walk (every site is then one), `-Dscalac.parallelTyper.xray.threadInfo=false` the ThreadMXBean thread info.

### JSON

One object per run; times are integer microseconds from the start of the recording, durations integer microseconds; strings are indices into `strings`, threads into `threads`; records are flat arrays with a fixed stride.

| field | stride | record |
|---|---|---|
| `version`, `startEpochMillis`, `wallUs`, `holdThresholdUs` | | |
| `meta` | | object of strings |
| `spans` | 10 | thread, step, label, start, end, CPU, blocked ms, parked ms (-1: not measured), lock wait, lock hold |
| `phases` | 3 | name, start, end |
| `idle` | 2 | thread, start (until the typer phase ends) |
| `gc` | 3 | label, start, end |
| `holds` | 6 | thread, site, what (-1: none), start, end, contended (1/0) |
| `waits` | 5 | thread, site, what (-1: none), start, end |
| `sites` | 8 | site, acquisitions, hold total, hold max, waits, wait total, wait max, waiters (bit per thread index) |
| `threads` | | names, main first |
| `strings` | | |

A library run at 4 threads is about 3 MB (50k holds, 20k waits).

## Cost

Off: one static field read and a null test per lock acquisition and per unit and phase.

On, measured with `OVERHEAD=14 run.sh library <threads>` (14 alternating pairs after 10 warm-up compilations, M5 Pro laptop shared with other work, so noisy): one thread, medians 1905 ms off and 1874 ms on (no measurable cost); 8 threads, 1655 ms off and 1789 ms on (+8%; minima 1256 and 1492). Two earlier designs show where the cost hides: a stack walk per outermost acquisition (about 3 µs, 130k per run) cost a fifth of a one-thread run, hence the sampled sites; and recording a contended hold before the unlock (walking its site under the lock) lengthened exactly the holds others queue on, +38% at 8 threads, hence the per-thread records written after the release. Thread contention monitoring is switched on only while recording.

## Known gaps

- The holds below the threshold that nobody waited on are not recorded one by one; their per-site totals are estimated from the sample (marked ≈ in the page). Every wait and every recorded hold has its exact site.
- Only `Symbol.completeInfoUnderLock` says what it completes; the other sites (`TypeRef.baseTypeSeq`, `TypeSymbol.tpe`, ...) are named by site alone. Passing the type or symbol at those call sites would be a line each in `Types.scala` and `Symbols.scala`.
- Blocked time on the reporter's, `uniques`' and the name table's monitors and on lazy-val initialisers is visible only per span, summed (ThreadMXBean blocked time, millisecond granularity), not per monitor or per instant.
- The parallelism chart weights a span by its average CPU over wall time, so a thread descheduled for part of a span shows as a uniformly lower contribution over the whole span.
- A wait's time that overlaps no other thread's hold is hand-over latency (the lock free, the next owner not yet running): the page counts it as lock wait and the *Waited on* table cannot attribute it.
- The phases after the typer and the parser and namer are recorded per unit but are serial by design; the page labels them, it does not split them further.
- `timeline.data.js` for six library runs is about 15 MB; it is not committed.
- A link can open a view: `index.html#run=8&from=1100&to=1104&theme=dark`.
