#!/usr/bin/env bash
# In-JVM race finder (Stress.scala): tools/parallel-typer/stress.sh <corpus> [iterations=500] [threads=8] [scalac options...]
# <corpus> as for oracle.sh. JAVA_OPTS is passed through. Uses the quick compiler in place (no snapshot).
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
CORPUS="$1"; N="${2:-500}"; THREADS="${3:-8}"; shift; shift || true; shift || true
TMP="$(mktemp -d)"; Q="$ROOT/build/quick/classes"
JARS="$(grep '^TOOL_CLASSPATH=' "$ROOT/build/quick/bin/scalac" | head -1 | sed 's/^TOOL_CLASSPATH="//; s/"$//' | tr ':' '\n' | grep '\.jar$' | paste -sd: -)"
"$ROOT/build/quick/bin/scalac" -d "$TMP" -cp "$Q/compiler" "$ROOT/tools/parallel-typer/Stress.scala" || exit 2
if [ -d "$ROOT/tools/parallel-typer/corpus/$CORPUS" ]; then
  { echo -deprecation; echo -feature; find "$ROOT/tools/parallel-typer/corpus/$CORPUS" -name '*.scala' -o -name '*.java' | sort; } > "$TMP/args"
elif [ -d "$CORPUS" ]; then { echo -deprecation; echo -feature; find -L "$CORPUS" -name '*.scala' -o -name '*.java' | sort; } > "$TMP/args"
else cp "$CORPUS" "$TMP/args"; fi
java -Xss64m -Xmx4g ${JAVA_OPTS:-} -cp "$Q/compiler:$Q/library:$Q/reflect:$JARS:$TMP" Stress "$TMP/args" "$N" "$THREADS" "$@"
status=$?
rm -rf "$TMP"
exit $status
