#!/usr/bin/env bash
# The machine's own limit, for the scaling charts: k independent serial compilations at once in one JVM (XrayIndep.scala).
#
#   tools/parallel-typer/xray/indep.sh <library|reflect|compiler|argsfile> ["1 2 4 6 8"]
#
# Environment: as run.sh (JAVA, HEAP, GC, JAVA_OPTS), WARMUP (default 8 compilations), ROUNDS (default 3 batches per k).
set -euo pipefail
JAVA="${JAVA:-java}"
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../../.." && pwd)"
CORPUS="$1"; KS="${2:-1 2 4 6 8}"
WORK="${WORK:-/tmp/parallel-typer-xray/$(basename "$CORPUS")-indep}"
mkdir -p "$WORK"
QUICK="$ROOT/build/quick/classes"
SNAP="$WORK/compiler-snapshot"; rm -rf "$SNAP"; mkdir -p "$SNAP"
for d in compiler library reflect; do cp -R "$QUICK/$d" "$SNAP/$d"; done
JARS="$(grep '^TOOL_CLASSPATH=' "$ROOT/build/quick/bin/scalac" | head -1 | sed 's/^TOOL_CLASSPATH="//; s/"$//' | tr ':' '\n' | grep '\.jar$' | paste -sd: -)"
CP="$SNAP/compiler:$SNAP/library:$SNAP/reflect:$JARS"
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
"$JAVA" -cp "$CP" -Dscala.usejavacp=true scala.tools.nsc.Main -nowarn -d "$BIN" "$HERE/XrayBench.scala" "$HERE/XrayMemory.scala" "$HERE/XrayIndep.scala"
"$JAVA" -Xss64m -Xms${HEAP:-12g} -Xmx${HEAP:-12g} ${GC:--XX:+UseParallelGC} ${JAVA_OPTS:-} -cp "$BIN:$CP" XrayIndep "$args" "${WARMUP:-8}" "${ROUNDS:-3}" $KS
