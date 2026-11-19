#!/usr/bin/env bash
# In-JVM race finder (Stress.scala): tools/parallel-typer/stress.sh <corpus> [iterations=500] [threads=8] [scalac options...]
# <corpus> as for oracle.sh. JAVA_OPTS is passed through. Uses the quick compiler and the sources in place (no snapshot).
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
CORPUS="$1"; N="${2:-500}"; THREADS="${3:-8}"; shift; shift || true; shift || true
TMP="$(mktemp -d)"; Q="$ROOT/build/quick/classes"
JARS="$(grep '^TOOL_CLASSPATH=' "$ROOT/build/quick/bin/scalac" | head -1 | sed 's/^TOOL_CLASSPATH="//; s/"$//' | tr ':' '\n' | grep '\.jar$' | paste -sd: -)"
"$ROOT/build/quick/bin/scalac" -d "$TMP" -cp "$Q/compiler" "$ROOT/tools/parallel-typer/Stress.scala" || exit 2
if [ "$CORPUS" = library ]; then
  { cat "$ROOT/tools/parallel-typer/library.opts"; echo "-sourcepath"; echo "$ROOT/src/library"; find "$ROOT/src/library" -name '*.scala' -o -name '*.java' | sort; } > "$TMP/args"
elif [ "$CORPUS" = reflect ]; then
  { cat "$ROOT/tools/parallel-typer/reflect.opts"; echo "-classpath"; echo "$Q/library"; find "$ROOT/src/reflect" -name '*.scala' -o -name '*.java' | sort; } > "$TMP/args"
elif [ "$CORPUS" = compiler ]; then
  { cat "$ROOT/tools/parallel-typer/compiler.opts"; echo "-classpath"; echo "$Q/library:$Q/reflect:$(cat "$ROOT/tools/parallel-typer/compiler.cp" 2>/dev/null)"; find "$ROOT/src/compiler" -name '*.scala' -o -name '*.java' | sort; } > "$TMP/args"
elif [ -d "$ROOT/tools/parallel-typer/corpus/$CORPUS" ]; then
  { echo -deprecation; echo -feature; find "$ROOT/tools/parallel-typer/corpus/$CORPUS" -name '*.scala' -o -name '*.java' | sort; } > "$TMP/args"
elif [ -d "$CORPUS" ]; then { echo -deprecation; echo -feature; find -L "$CORPUS" -name '*.scala' -o -name '*.java' | sort; } > "$TMP/args"
else cp "$CORPUS" "$TMP/args"; fi
java -Xss64m -Xmx4g ${JAVA_OPTS:-} -cp "$Q/compiler:$Q/library:$Q/reflect:$JARS:$TMP" Stress "$TMP/args" "$N" "$THREADS" "$@"
status=$?
rm -rf "$TMP"
exit $status
