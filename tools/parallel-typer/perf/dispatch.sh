#!/usr/bin/env bash
# CPU profiles of one compiler build on one corpus, serial and at N threads, for counting interface dispatch and type
# checks (dispatch.py). Unlike xray/profile.sh, the serial compiler is profiled too, and there is no wall clock recording.
#
#   tools/parallel-typer/perf/dispatch.sh <root with build/quick> <args file> <out dir> [threads=8]
#
# The args file is given rather than built from the tree, so that two builds compile the same sources against the same
# classpath. Environment: JAVA, JAVA_OPTS, PERFLAB, WARMUP (default 14), REPEATS (default 10), HEAP (default 12g).
set -euo pipefail
ROOT="$(cd "$1" && pwd)"; ARGS="$2"; OUT="$3"; THREADS="${4:-8}"
HERE="$(cd "$(dirname "$0")" && pwd)"; XRAY="$HERE/../xray"
JAVA="${JAVA:-java}"
ASYNC="${PERFLAB:-$HOME/code/retronym/jvm-perf-lab}/tools/build/async-profiler/build"
LIB="$ASYNC/lib/libasyncProfiler.dylib"; [ -f "$LIB" ] || LIB="$ASYNC/lib/libasyncProfiler.so"
PROFILER_JARS="$ASYNC/jar/async-profiler.jar:$ASYNC/jar/jfr-converter.jar"
QUICK="$ROOT/build/quick/classes"
JARS="$(grep '^TOOL_CLASSPATH=' "$ROOT/build/quick/bin/scalac" | head -1 | sed 's/^TOOL_CLASSPATH="//; s/"$//' | tr ':' '\n' | grep '\.jar$' | paste -sd: -)"
CP="$QUICK/compiler:$QUICK/library:$QUICK/reflect:$JARS"
mkdir -p "$OUT"; BIN="$OUT/bench-classes"; rm -rf "$BIN"; mkdir -p "$BIN"
"$JAVA" -cp "$CP:$PROFILER_JARS" -Dscala.usejavacp=true scala.tools.nsc.Main -nowarn -d "$BIN" "$XRAY/XrayBench.scala" "$XRAY/XrayMemory.scala" "$XRAY/XrayProfile.scala"
"$JAVA" -Xss64m -Xms${HEAP:-12g} -Xmx${HEAP:-12g} ${GC:--XX:+UseParallelGC} "-agentpath:$LIB" -XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints ${JAVA_OPTS:-} -cp "$BIN:$CP:$PROFILER_JARS" \
  XrayProfile "$ARGS" "$OUT" "$(basename "$ARGS")" "${WARMUP:-14}" "${REPEATS:-10}" 1ms 2ms serial-cpu "$THREADS"
for c in serial-cpu "$THREADS"; do
  "$ASYNC/bin/jfrconv" --cpu -o collapsed "$OUT/run-$c.jfr" "$OUT/run-$c.collapsed"
done
rm -rf "$BIN"
