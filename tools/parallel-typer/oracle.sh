#!/usr/bin/env bash
# The oracle of -Yparallel-typer: typecheck a corpus serially and in parallel up to the pickler, and compare the
# diagnostics (sorted) and the pickles (.sig files, byte for byte). Repeats the parallel run to find flaky failures.
#
#   tools/parallel-typer/oracle.sh <corpus> [threads=8] [repeats=1] [extra scalac options...]
#
# <corpus> is `library`, `reflect`, `compiler`, or a file of scalac arguments (options and sources, one per line).
# Needs `sbt dist/mkQuick`. Results go to $OUT (default /tmp/parallel-typer-oracle/<corpus>); the exit status is the
# number of parallel runs that differed from the serial one. JAVA_OPTS is passed through (e.g. -Xss, -Dscalac.parallelTyper.*).
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
SCALAC="$ROOT/build/quick/bin/scalac"
CORPUS="$1"; THREADS="${2:-8}"; REPEATS="${3:-1}"; shift; shift || true; shift || true
OUT="${OUT:-/tmp/parallel-typer-oracle/$(basename "$CORPUS")}"
rm -rf "$OUT"; mkdir -p "$OUT"
QUICK="$ROOT/build/quick/classes"

args="$OUT/args"
case "$CORPUS" in
  library)
    { cat "$ROOT/tools/parallel-typer/library.opts"; echo "-sourcepath"; echo "$ROOT/src/library";
      find "$ROOT/src/library" -name '*.scala' -o -name '*.java' | sort; } > "$args" ;;
  reflect)
    { cat "$ROOT/tools/parallel-typer/reflect.opts"; echo "-classpath"; echo "$QUICK/library";
      find "$ROOT/src/reflect" -name '*.scala' -o -name '*.java' | sort; } > "$args" ;;
  compiler)
    { cat "$ROOT/tools/parallel-typer/compiler.opts"; echo "-classpath"; echo "$QUICK/library:$QUICK/reflect:$(cat "$ROOT/tools/parallel-typer/compiler.cp" 2>/dev/null)";
      find "$ROOT/src/compiler" -name '*.scala' -o -name '*.java' | sort; } > "$args" ;;
  *) cp "$CORPUS" "$args" ;;
esac

run() { # <name> <extra options...>
  local name="$1"; shift
  rm -rf "$OUT/$name"; mkdir -p "$OUT/$name/sig" "$OUT/$name/classes"
  local t0=$(date +%s.%N)
  "$SCALAC" -J-Xss64m -J-Xmx4g -d "$OUT/$name/classes" -Ystop-after:pickler -Ypickle-write "$OUT/$name/sig" "$@" "@$args" > "$OUT/$name/out.txt" 2>&1
  local status=$?
  local t1=$(date +%s.%N)
  echo "$status" > "$OUT/$name/status"
  # diagnostics, sorted, without the parallel typer's own log lines
  grep -v '^\[parallel-typer\]' "$OUT/$name/out.txt" | sort > "$OUT/$name/diagnostics.txt"
  printf '%-12s status %s  %.1f s\n' "$name" "$status" "$(echo "$t1 - $t0" | bc)"
}

compare() { # <name>: 0 if it agrees with serial
  local name="$1" bad=0
  if ! cmp -s "$OUT/serial/status" "$OUT/$name/status"; then echo "  $name: exit status differs"; bad=1; fi
  if ! diff -q "$OUT/serial/diagnostics.txt" "$OUT/$name/diagnostics.txt" > /dev/null; then
    echo "  $name: diagnostics differ ($OUT/$name/diagnostics.diff)"
    diff "$OUT/serial/diagnostics.txt" "$OUT/$name/diagnostics.txt" > "$OUT/$name/diagnostics.diff"; bad=1
  fi
  local sigdiff
  sigdiff=$(diff -rq "$OUT/serial/sig" "$OUT/$name/sig" 2>&1)
  if [ -n "$sigdiff" ]; then
    echo "  $name: $(echo "$sigdiff" | wc -l | tr -d ' ') pickles differ ($OUT/$name/sig.diff)"; echo "$sigdiff" > "$OUT/$name/sig.diff"; bad=1
  fi
  if grep -q 'Exception\|error: \[parallel-typer\]\|still running after' "$OUT/$name/out.txt"; then echo "  $name: crash or hang signature in output"; bad=1; fi
  return $bad
}

run serial "$@"
echo "  $(find "$OUT/serial/sig" -name '*.sig' | wc -l | tr -d ' ') pickles, $(wc -l < "$OUT/serial/diagnostics.txt" | tr -d ' ') diagnostic lines"
failures=0
run one -Yparallel-typer -J-Dscalac.parallelTyper.oneThread "$@"
compare one || failures=$((failures+1))
for i in $(seq 1 "$REPEATS"); do
  run "par$i" -Yparallel-typer -Yparallel-typer-threads "$THREADS" -J-Dscalac.parallelTyper.log "$@"
  if compare "par$i"; then rm -rf "$OUT/par$i/sig" "$OUT/par$i/classes"; else failures=$((failures+1)); fi
done
echo "$failures of $((REPEATS+1)) runs differ from serial"
exit $failures
