#!/usr/bin/env bash
# usage: run.sh <build/pack dir of the scala under test> <seconds for RangeDiff> [seed]
# Compiles the harnesses with that build and runs them against its library. Output in $OUT (default: a temp dir).
set -u
PACK=$1; SECS=${2:-120}; SEED=${3:-1}
HERE=$(cd "$(dirname "$0")" && pwd); OUT=${OUT:-$(mktemp -d)}
mkdir -p "$OUT/classes"
(cd "$HERE/ordinary" && python3 gen_ops.py)
"$PACK/bin/scalac" -nowarn -d "$OUT/classes" "$HERE"/*.scala "$HERE"/ordinary/*.scala || exit 1
CP="$PACK/lib/scala-library.jar:$OUT/classes"
# quarantine=1 leaves out the inputs that reproduce the recorded Range findings, so that a run gets past them
java -Xmx1g -cp "$CP" RangeDiff "$SEED" "$SECS" ${QUARANTINE:+quarantine=1} > "$OUT/range.log" 2>&1
sed -n '/^HANG/p;/^====/,$p' "$OUT/range.log" | cut -c1-400
java -Xmx2g ${JACOCO_AGENT:+-javaagent:$JACOCO_AGENT=destfile=$OUT/ordinary.exec,includes=scala.*} -cp "$CP" OrdDiff all "$SEED" "$SECS" > "$OUT/ordinary.log" 2>&1
sed -n '/^HANG/p;/^====/,$p' "$OUT/ordinary.log" | cut -c1-400
# ALIAS=1 also runs the self-aliasing matrix (about 4 minutes); it exits with 3 after a cell that did not terminate
# and is restarted at the next cell
if [ -n "${ALIAS:-}" ]; then
  : > "$OUT/alias.tsv"
  while :; do
    n=$(wc -l < "$OUT/alias.tsv")
    java -Xmx512m -cp "$CP" AliasMatrix "$OUT/alias.tsv" "$n" > "$OUT/alias.err" 2>&1; rc=$?
    [ $rc -eq 0 ] && break
    [ $rc -eq 3 ] || java -Xmx512m -cp "$CP" AliasMatrix "$OUT/alias.tsv" "$n" mark
  done
  python3 "$HERE/matrix.py" "$OUT/alias.tsv" > "$OUT/alias-matrix.md"
fi
echo "output: $OUT (range.log, ordinary.log, alias.tsv, alias-matrix.md)"
