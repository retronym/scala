#!/usr/bin/env bash
# Record x-ray timelines of -Yparallel-typer on a corpus, one per thread count, in one warm JVM, and summarise them.
#
#   tools/parallel-typer/xray/run.sh <library|reflect|compiler|argsfile> ["1 2 4 8"]
#
# Always records `serial` (no -Yparallel-typer) as well, as the baseline of the speed-ups. Writes
# tools/parallel-typer/xray/timeline.data.js (read by index.html beside it) and one JSON per run under $OUT
# (default /tmp/parallel-typer-xray/<corpus>). Needs `sbt dist/mkQuick`.
# Environment: WARMUP (default 12 compilations), JAVA_OPTS, OVERHEAD=<pairs> (instead: timeline on vs off, at the
# largest thread count), ROUNDS=<n> (instead: the configurations in turn, n recordings each, compared by step with
# rounds.py), CLASSES=<dir> (the compiler to record, a directory with compiler/ library/ reflect/, instead of a
# snapshot of build/quick: to compare two builds).
set -euo pipefail
JAVA="${JAVA:-java}" # the JVM to record on, e.g. a patched JDK's bin/java
# GC: the collector's options (default -XX:+UseParallelGC: a batch compile wants throughput, and G1's concurrent
# threads were a tenth of the CPU samples of a recording)
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../../.." && pwd)"
CORPUS="$1"
# by default 1 2 4 8 and, on Apple silicon, the number of the fastest cores
PCORES="$(sysctl -n hw.perflevel0.physicalcpu 2>/dev/null || true)"
THREADS="${2:-$(printf '%s\n' 1 2 4 8 $PCORES | sort -nu | paste -sd' ' -)}"
WARMUP="${WARMUP:-12}"
OUT="${OUT:-/tmp/parallel-typer-xray/$(basename "$CORPUS")}"
mkdir -p "$OUT"
QUICK="$ROOT/build/quick/classes"
if [ -n "${CLASSES:-}" ]; then SNAP="$CLASSES"; else
  [ -d "$QUICK/compiler" ] || { echo "no $QUICK/compiler: run sbt dist/mkQuick" >&2; exit 1; }
  # a snapshot of the compiler, so that a build during the run does not change it
  SNAP="$OUT/compiler-snapshot"; rm -rf "$SNAP"; mkdir -p "$SNAP"
  for d in compiler library reflect; do cp -R "$QUICK/$d" "$SNAP/$d"; done
fi
JARS="$(grep '^TOOL_CLASSPATH=' "$ROOT/build/quick/bin/scalac" | head -1 | sed 's/^TOOL_CLASSPATH="//; s/"$//' | tr ':' '\n' | grep '\.jar$' | paste -sd: -)"
CP="$SNAP/compiler:$SNAP/library:$SNAP/reflect:$JARS"

# the corpus as the oracle builds it (tools/parallel-typer/oracle.sh)
args="$OUT/args"
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

# the recorder, compiled against the snapshot
BIN="$OUT/bench-classes"; rm -rf "$BIN"; mkdir -p "$BIN"
"$JAVA" -cp "$CP" -Dscala.usejavacp=true scala.tools.nsc.Main -d "$BIN" "$HERE/XrayBench.scala" "$HERE/XrayMemory.scala"

JVM=("$JAVA" -Xss64m -Xms${HEAP:-12g} -Xmx${HEAP:-12g} ${GC:--XX:+UseParallelGC} ${JAVA_OPTS:-} -cp "$BIN:$CP")
if [ -n "${OVERHEAD:-}" ]; then
  last="${THREADS##* }"
  exec "${JVM[@]}" XrayBench --overhead "$args" "$last" "$WARMUP" "$OVERHEAD"
fi
configs="serial $THREADS"
if [ -n "${ROUNDS:-}" ]; then exec "${JVM[@]}" XrayBench --rounds "$args" "$OUT" "$(basename "$CORPUS")" "$WARMUP" "$ROUNDS" $configs; fi
"${JVM[@]}" XrayBench "$args" "$OUT" "$(basename "$CORPUS")" "$WARMUP" $configs
cp "$OUT/timeline.data.js" "$HERE/timeline.data.js"
echo
node "$HERE/summary.js" "$HERE/timeline.data.js"
echo
echo "open $HERE/index.html"
