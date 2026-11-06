#!/usr/bin/env bash
# Record an x-ray of -Yparallel-typer on a corpus with async-profiler flame graph data, for index.html.
#
#   tools/parallel-typer/xray/profile.sh <library|reflect|compiler|argsfile> [threads, default 4]
#
# Records in one warm JVM, under the patched async-profiler of jvm-perf-lab (never a stock one: it lacks the spans):
# `serial` (without a profile: the baseline of the speed-ups), the thread count with a CPU profile, and the thread
# count again with a wall clock profile (the two engines cannot run together on macOS). A profile covers REPEATS
# compilations, projected onto the one of median length (XrayProfile.scala). Writes
# tools/parallel-typer/xray/profiles/<corpus>/: the raw run-<config>.jfr, run-<config>.json with the page's profile
# data, and flame-<threads>-{cpu,wall}.html from jfrconv --spans. Regenerates timeline.data.js from every corpus under
# profiles/. Needs `sbt dist/mkQuick`.
# Environment: PERFLAB (default ~/code/retronym/jvm-perf-lab), WARMUP (default 14 compilations), CPU_INTERVAL (1ms),
# WALL_INTERVAL (2ms), REPEATS (10), JAVA_OPTS, OVERHEAD=<rounds> (instead: nothing on, timeline, timeline with each profile),
# FRAMES=<rounds> (instead: CPU profiles of serial and the thread count in turn, REPEATS compilations each, as collapsed
# stacks under OUT, compared by frames.py; with FRAMES_EVENT="--alloc --total" and -Dxray.frames.alloc in JAVA_OPTS, bytes allocated), CLASSES=<dir> (the compiler to record, with compiler/ library/ reflect/).
set -euo pipefail
JAVA="${JAVA:-java}" # the JVM to record on, e.g. a patched JDK's bin/java
# GC: the collector's options (default -XX:+UseParallelGC: a batch compile wants throughput, and G1's concurrent
# threads were a tenth of the CPU samples of a recording)
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../../.." && pwd)"
CORPUS="$1"
THREADS="${2:-4}"
NAME="${XRAY_NAME:-$(basename "$CORPUS")}" # XRAY_NAME: a name for this recording other than the corpus (a variant of it)
WARMUP="${WARMUP:-14}"
REPEATS="${REPEATS:-10}"
CPU_INTERVAL="${CPU_INTERVAL:-1ms}"
WALL_INTERVAL="${WALL_INTERVAL:-2ms}"
ASYNC="${PERFLAB:-$HOME/code/retronym/jvm-perf-lab}/tools/build/async-profiler/build"
LIB="$ASYNC/lib/libasyncProfiler.dylib"; [ -f "$LIB" ] || LIB="$ASYNC/lib/libasyncProfiler.so"
[ -f "$LIB" ] || { echo "no $LIB: mise run tools:async-profiler in jvm-perf-lab" >&2; exit 1; }
OUT="${OUT:-$HERE/profiles/$NAME}"
WORK="${WORK:-/tmp/parallel-typer-xray/$NAME-profile}"
mkdir -p "$OUT" "$WORK"
QUICK="$ROOT/build/quick/classes"
if [ -n "${CLASSES:-}" ]; then SNAP="$CLASSES"; else
  [ -d "$QUICK/compiler" ] || { echo "no $QUICK/compiler: run sbt dist/mkQuick" >&2; exit 1; }
  # a snapshot of the compiler, so that a build during the run does not change it
  SNAP="$WORK/compiler-snapshot"; rm -rf "$SNAP"; mkdir -p "$SNAP"
  for d in compiler library reflect; do cp -R "$QUICK/$d" "$SNAP/$d"; done
fi
JARS="$(grep '^TOOL_CLASSPATH=' "$ROOT/build/quick/bin/scalac" | head -1 | sed 's/^TOOL_CLASSPATH="//; s/"$//' | tr ':' '\n' | grep '\.jar$' | paste -sd: -)"
CP="$SNAP/compiler:$SNAP/library:$SNAP/reflect:$JARS"
PROFILER_JARS="$ASYNC/jar/async-profiler.jar:$ASYNC/jar/jfr-converter.jar"

args="$WORK/args"
case "$CORPUS" in
  library)
    { cat "$ROOT/tools/parallel-typer/library.opts"; echo "-sourcepath"; echo "$ROOT/src/library";
      find "$ROOT/src/library" -name '*.scala' -o -name '*.java' | sort; } > "$args" ;;
  reflect)
    { cat "$ROOT/tools/parallel-typer/reflect.opts"; echo "-classpath"; echo "$SNAP/library";
      find "$ROOT/src/reflect" -name '*.scala' -o -name '*.java' | sort; } > "$args" ;;
  compiler)
    { cat "$ROOT/tools/parallel-typer/compiler.opts"; echo "-classpath"; echo "$SNAP/library:$SNAP/reflect:$(cat "$ROOT/tools/parallel-typer/compiler.cp" 2>/dev/null)";
      find "$ROOT/src/compiler" -name '*.scala' -o -name '*.java' | sort; } > "$args" ;;
  *) cp "$CORPUS" "$args" ;;
esac

BIN="$WORK/bench-classes"; rm -rf "$BIN"; mkdir -p "$BIN"
"$JAVA" -cp "$CP:$PROFILER_JARS" -Dscala.usejavacp=true scala.tools.nsc.Main -nowarn -d "$BIN" "$HERE/XrayBench.scala" "$HERE/XrayMemory.scala" "$HERE/XrayProfile.scala"

JVM=("$JAVA" -Xss64m -Xms${HEAP:-12g} -Xmx${HEAP:-12g} ${GC:--XX:+UseParallelGC} "-agentpath:$LIB" -XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints
  -Dscalac.parallelTyper.xray.completionSpans=true ${JAVA_OPTS:-} -cp "$BIN:$CP:$PROFILER_JARS")
if [ -n "${OVERHEAD:-}" ]; then
  exec "${JVM[@]}" XrayProfile --overhead "$args" "$THREADS" "$WARMUP" "$OVERHEAD" "$CPU_INTERVAL" "$WALL_INTERVAL" "$WORK"
fi
if [ -n "${FRAMES:-}" ]; then
  mkdir -p "$OUT"
  "${JVM[@]}" XrayProfile --frames "$args" "$OUT" "$WARMUP" "$FRAMES" "$REPEATS" "$CPU_INTERVAL" serial $THREADS
  for c in serial $THREADS; do
    for j in "$OUT"/cmp-$c-*.jfr; do "$ASYNC/bin/jfrconv" -o collapsed ${FRAMES_EVENT:---cpu} --spans --threads "$j" "${j%.jfr}.collapsed"; rm -f "$j"; gzip -f "${j%.jfr}.collapsed"; done
  done
  exit 0
fi
# [threads] may be a list ("1 2 4 8"): each count is then recorded with a CPU profile and with a wall clock profile.
# The raw JFR and the full flame graph pages are kept for the last count only (they are 50-180 MB each).
configs=(serial); for t in $THREADS; do configs+=("$t" "$t-wall"); done
LAST="${THREADS##* }"
"${JVM[@]}" XrayProfile "$args" "$OUT" "$NAME" "$WARMUP" "$REPEATS" "$CPU_INTERVAL" "$WALL_INTERVAL" "${configs[@]}"
JFRCONV=("$ASYNC/bin/jfrconv" --spans --threads --span-config "$HERE/span-config.json")
"${JFRCONV[@]}" --cpu "$OUT/run-$LAST.jfr" "$OUT/flame-$LAST-cpu.html"
"${JFRCONV[@]}" --wall "$OUT/run-$LAST-wall.jfr" "$OUT/flame-$LAST-wall.html"
"${JFRCONV[@]}" --alloc --total "$OUT/run-$LAST.jfr" "$OUT/flame-$LAST-alloc.html"
for t in $THREADS; do [ "$t" = "$LAST" ] || rm -f "$OUT/run-$t.jfr" "$OUT/run-$t-wall.jfr"; done
rm -rf "$WORK"
# for the page: a file of timelines per corpus and a file per profile, loaded when its run is shown (split.py)
python3 -I "$HERE/split.py"
du -sh "$HERE/data" "$OUT"/*
node "$HERE/summary.js" "$OUT/timeline.data.js"
echo "cd $HERE && python3 -m http.server, then open index.html"
