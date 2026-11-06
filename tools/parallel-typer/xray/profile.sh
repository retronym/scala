#!/usr/bin/env bash
# Record an x-ray of -Yparallel-typer on a corpus with async-profiler flame graph data, for index.html.
#
#   tools/parallel-typer/xray/profile.sh <library|reflect|compiler|argsfile> [threads, default 4]
#
# Records in one warm JVM, under the patched async-profiler of jvm-perf-lab (never a stock one: it lacks the spans):
# `serial` (without a profile: the baseline of the speed-ups), the thread count with a CPU profile, and the thread
# count again with a wall clock profile (the two engines cannot run together on macOS). Writes
# tools/parallel-typer/xray/profiles/<corpus>/: the raw run-<config>.jfr, run-<config>.json with the page's profile
# data, and flame-<threads>-{cpu,wall}.html from jfrconv --spans. Regenerates timeline.data.js from every corpus under
# profiles/. Needs `sbt dist/mkQuick`.
# Environment: PERFLAB (default ~/code/retronym/jvm-perf-lab), WARMUP (default 14 compilations), CPU_INTERVAL (1ms),
# WALL_INTERVAL (2ms), JAVA_OPTS, OVERHEAD=<rounds> (instead: nothing on, timeline, timeline with each profile).
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../../.." && pwd)"
CORPUS="$1"
THREADS="${2:-4}"
NAME="$(basename "$CORPUS")"
WARMUP="${WARMUP:-14}"
CPU_INTERVAL="${CPU_INTERVAL:-1ms}"
WALL_INTERVAL="${WALL_INTERVAL:-2ms}"
ASYNC="${PERFLAB:-$HOME/code/retronym/jvm-perf-lab}/tools/build/async-profiler/build"
LIB="$ASYNC/lib/libasyncProfiler.dylib"; [ -f "$LIB" ] || LIB="$ASYNC/lib/libasyncProfiler.so"
[ -f "$LIB" ] || { echo "no $LIB: mise run tools:async-profiler in jvm-perf-lab" >&2; exit 1; }
OUT="$HERE/profiles/$NAME"
WORK="${WORK:-/tmp/parallel-typer-xray/$NAME-profile}"
mkdir -p "$OUT" "$WORK"
QUICK="$ROOT/build/quick/classes"
[ -d "$QUICK/compiler" ] || { echo "no $QUICK/compiler: run sbt dist/mkQuick" >&2; exit 1; }
# a snapshot of the compiler, so that a build during the run does not change it
SNAP="$WORK/compiler-snapshot"; rm -rf "$SNAP"; mkdir -p "$SNAP"
for d in compiler library reflect; do cp -R "$QUICK/$d" "$SNAP/$d"; done
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
java -cp "$CP:$PROFILER_JARS" -Dscala.usejavacp=true scala.tools.nsc.Main -nowarn -d "$BIN" "$HERE/XrayBench.scala" "$HERE/XrayProfile.scala"

JVM=(java -Xss64m -Xms6g -Xmx6g "-agentpath:$LIB" -XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints
  -Dscalac.parallelTyper.xray.completionSpans=true ${JAVA_OPTS:-} -cp "$BIN:$CP:$PROFILER_JARS")
if [ -n "${OVERHEAD:-}" ]; then
  exec "${JVM[@]}" XrayProfile --overhead "$args" "$THREADS" "$WARMUP" "$OVERHEAD" "$CPU_INTERVAL" "$WALL_INTERVAL" "$WORK"
fi
"${JVM[@]}" XrayProfile "$args" "$OUT" "$NAME" "$WARMUP" "$CPU_INTERVAL" "$WALL_INTERVAL" serial "$THREADS" "$THREADS-wall"
JFRCONV=("$ASYNC/bin/jfrconv" --spans --threads --span-config "$HERE/span-config.json")
"${JFRCONV[@]}" --cpu "$OUT/run-$THREADS.jfr" "$OUT/flame-$THREADS-cpu.html"
"${JFRCONV[@]}" --wall "$OUT/run-$THREADS-wall.jfr" "$OUT/flame-$THREADS-wall.html"
rm -rf "$WORK"
cat "$HERE"/profiles/*/timeline.data.js > "$HERE/timeline.data.js"
du -sh "$HERE/timeline.data.js" "$OUT"/*
node "$HERE/summary.js" "$OUT/timeline.data.js"
echo "cd $HERE && python3 -m http.server, then open index.html"
