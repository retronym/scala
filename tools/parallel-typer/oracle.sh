#!/usr/bin/env bash
# The oracle of -Yparallel-typer: typecheck a corpus serially and in parallel up to the pickler, and compare the
# diagnostics (sorted) and the pickles (.sig files, byte for byte). Repeats the parallel run to find flaky failures.
#
#   tools/parallel-typer/oracle.sh <corpus> [threads=8] [repeats=1] [extra scalac options...]
#
# <corpus> is `library`, `reflect`, `compiler`, a directory under tools/parallel-typer/corpus, another directory of
# sources, or a file of scalac arguments (options and sources, one per line).
# With FULL=1, every phase runs (no -Ystop-after) and the class files are compared byte for byte instead of the pickles.
# Needs `sbt dist/mkQuick`. Results go to $OUT (default /tmp/parallel-typer-oracle/<corpus>); the exit status is the
# number of parallel runs that differed from the serial one. JAVA_OPTS is passed through (e.g. -Xss, -Dscalac.parallelTyper.*).
set -uo pipefail
ROOT="${ORACLE_ROOT:-$(cd "$(dirname "$0")/../.." && pwd)}"
# bash reads a script as it runs it: run from a copy, so that editing this file does not disturb a run in progress
if [ -z "${ORACLE_ROOT:-}" ]; then
  copy="$(mktemp -t oracle.XXXXXX)"; cp "$0" "$copy"; ORACLE_ROOT="$ROOT" exec bash "$copy" "$@"
fi
CORPUS="$1"; THREADS="${2:-8}"; REPEATS="${3:-1}"; shift; shift || true; shift || true
OUT="${OUT:-/tmp/parallel-typer-oracle/$(basename "$CORPUS")}"
rm -rf "$OUT"; mkdir -p "$OUT"
QUICK="$ROOT/build/quick/classes"
# a snapshot of the compiler, so that a build during the run does not change it
SNAP="$OUT/compiler-snapshot"; mkdir -p "$SNAP"
for d in compiler library reflect; do cp -R "$QUICK/$d" "$SNAP/$d"; done
JARS="$(grep '^TOOL_CLASSPATH=' "$ROOT/build/quick/bin/scalac" | head -1 | sed 's/^TOOL_CLASSPATH="//; s/"$//' | tr ':' '\n' | grep '\.jar$' | paste -sd: -)"
SCALAC_CP="$SNAP/compiler:$SNAP/library:$SNAP/reflect:$JARS"

FULL="${FULL:-}"
ORDERED="${ORDERED:-}"; case "$(basename "$CORPUS")" in parser-*|late-neg-*) ORDERED=1;; esac
args="$OUT/args"
# the sources of an in-repo corpus are copied too, so that editing them during a run does not change it
SRC="$OUT/src-snapshot"
snapsrc() { mkdir -p "$SRC"; cp -R "$ROOT/src/$1" "$SRC/$1"; }
case "$CORPUS" in
  library)
    snapsrc library
    { cat "$ROOT/tools/parallel-typer/library.opts"; echo "-sourcepath"; echo "$SRC/library";
      find "$SRC/library" -name '*.scala' -o -name '*.java' | sort; } > "$args" ;;
  reflect)
    snapsrc reflect
    { cat "$ROOT/tools/parallel-typer/reflect.opts"; echo "-classpath"; echo "$SNAP/library";
      find "$SRC/reflect" -name '*.scala' -o -name '*.java' | sort; } > "$args" ;;
  compiler)
    snapsrc compiler
    { cat "$ROOT/tools/parallel-typer/compiler.opts"; echo "-classpath"; echo "$SNAP/library:$SNAP/reflect:$(cat "$ROOT/tools/parallel-typer/compiler.cp" 2>/dev/null)";
      find "$SRC/compiler" -name '*.scala' -o -name '*.java' | sort; } > "$args" ;;
  *) if [ -d "$ROOT/tools/parallel-typer/corpus/$CORPUS" ]; then # a small corpus of the oracle's own
       { echo -deprecation; echo -feature; find "$ROOT/tools/parallel-typer/corpus/$CORPUS" -name '*.scala' -o -name '*.java' | sort; } > "$args"
     elif [ -d "$CORPUS" ]; then # a directory of sources, e.g. a compiler-benchmark corpus
       { echo -deprecation; echo -feature; find -L "$CORPUS" -name '*.scala' -o -name '*.java' | sort; } > "$args"
     else cp "$CORPUS" "$args"; fi ;;
esac

run() { # <name> <options...>: options starting with -J go to the JVM
  local name="$1"; shift
  rm -rf "$OUT/$name"; mkdir -p "$OUT/$name/sig" "$OUT/$name/classes"
  local jvm=() opts=() stop=()
  [ -n "$FULL" ] || stop=(-Ystop-after:pickler -Ypickle-write "$OUT/$name/sig")
  for o in "$@"; do case "$o" in -J*) jvm+=("${o#-J}");; *) opts+=("$o");; esac; done
  local t0=$(date +%s.%N)
  # a run that takes longer than $RUN_TIMEOUT seconds (default 600) is killed: a hang is a failure (status 124)
  timeout "${RUN_TIMEOUT:-600}" java -Xss64m -Xmx4g ${JAVA_OPTS:-} "${jvm[@]}" -cp "$SCALAC_CP" -Dscala.usejavacp=true scala.tools.nsc.Main \
    -d "$OUT/$name/classes" ${stop[@]+"${stop[@]}"} "${opts[@]}" "@$args" > "$OUT/$name/out.txt" 2>&1
  local status=$?
  local t1=$(date +%s.%N)
  echo "$status" > "$OUT/$name/status"
  # diagnostics, sorted, without the parallel typer's own log lines
  grep -v '^\[parallel-typer\]' "$OUT/$name/out.txt" | sort > "$OUT/$name/diagnostics.txt"
  grep -v '^\[parallel-typer\]' "$OUT/$name/out.txt" > "$OUT/$name/ordered.txt"
  printf '%-12s status %s  %.1f s\n' "$name" "$status" "$(echo "$t1 - $t0" | bc)"
}

compare() { # <name> [<reference>=serial]: 0 if it agrees with the reference run
  local name="$1" ref="${2:-serial}" bad=0
  if ! cmp -s "$OUT/$ref/status" "$OUT/$name/status"; then echo "  $name: exit status differs from $ref"; bad=1; fi
  if ! diff -q "$OUT/$ref/diagnostics.txt" "$OUT/$name/diagnostics.txt" > /dev/null; then
    echo "  $name: diagnostics differ from $ref ($OUT/$name/diagnostics-$ref.diff)"
    diff "$OUT/$ref/diagnostics.txt" "$OUT/$name/diagnostics.txt" > "$OUT/$name/diagnostics-$ref.diff"; bad=1
  fi
  # the order of the diagnostics too, for the corpora that are about it (ORDERED=1, or a corpus named parser-*)
  if [ -n "$ORDERED" ] && ! cmp -s "$OUT/$ref/ordered.txt" "$OUT/$name/ordered.txt"; then
    echo "  $name: the order of the diagnostics differs from $ref ($OUT/$name/ordered-$ref.diff)"
    diff "$OUT/$ref/ordered.txt" "$OUT/$name/ordered.txt" > "$OUT/$name/ordered-$ref.diff"; bad=1
  fi
  local sigdiff
  sigdiff=$(diff -rq "$OUT/$ref/sig" "$OUT/$name/sig" 2>&1)
  if [ -n "$sigdiff" ]; then
    echo "  $name: $(echo "$sigdiff" | wc -l | tr -d ' ') pickles differ from $ref ($OUT/$name/sig-$ref.diff)"; echo "$sigdiff" > "$OUT/$name/sig-$ref.diff"; bad=1
  fi
  if [ -n "$FULL" ]; then
    local clsdiff
    clsdiff=$(diff -rq "$OUT/$ref/classes" "$OUT/$name/classes" 2>&1)
    if [ -n "$clsdiff" ]; then
      echo "  $name: $(echo "$clsdiff" | wc -l | tr -d ' ') class files differ from $ref ($OUT/$name/classes-$ref.diff)"; echo "$clsdiff" > "$OUT/$name/classes-$ref.diff"; bad=1
    fi
  fi
  if grep -q 'Exception\|error: \[parallel-typer\]\|still running after' "$OUT/$name/out.txt"; then echo "  $name: crash or hang signature in output"; bad=1; fi
  return $bad
}

# Differences from `serial` that `one` shares are order effects of the parallel path (signatures first); differences
# of a parallel run from `one` are effects of concurrency.
run serial "$@"
if [ -n "${SERIAL_CHECK:-}" ]; then # the oracle's own check: serial against serial must agree
  run serial2 "$@"; if compare serial2; then echo "serial against serial: agree"; else echo "serial against serial: DIFFER"; exit 1; fi
fi
echo "  $(find "$OUT/serial/sig" -name '*.sig' | wc -l | tr -d ' ') pickles, $(find "$OUT/serial/classes" -name '*.class' | wc -l | tr -d ' ') class files, $(wc -l < "$OUT/serial/diagnostics.txt" | tr -d ' ') diagnostic lines"
failures=0; orderOnly=0
run one -Yparallel-typer -J-Dscalac.parallelTyper.oneThread "$@"
oneDiffers=0; compare one || oneDiffers=1
for i in $(seq 1 "$REPEATS"); do
  run "par$i" -Yparallel-typer -Yparallel-typer-threads "$THREADS" -J-Dscalac.parallelTyper.log "$@"
  if compare "par$i" one; then
    compare "par$i" serial > /dev/null || orderOnly=$((orderOnly+1))
    rm -rf "$OUT/par$i/sig" "$OUT/par$i/classes"
  else failures=$((failures+1)); compare "par$i" serial > /dev/null; fi
done
echo "one-thread run differs from serial: $oneDiffers; $failures of $REPEATS parallel runs differ from the one-thread run; $orderOnly agree with it but differ from serial (order effects)"
exit $((failures + oneDiffers))
