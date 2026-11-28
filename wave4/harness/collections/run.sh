#!/usr/bin/env bash
# usage: run.sh <build/pack dir of the scala under test> <seconds per family> [seed]
# Compiles both harnesses with that build and runs every family against its library.
# With JACOCO_AGENT=<org.jacoco.agent-*-runtime.jar> set, each run also writes <out>/<family>.exec.
set -eu
PACK=$1; SECS=${2:-60}; SEED=${3:-1}
HERE=$(cd "$(dirname "$0")" && pwd); OUT=${OUT:-$(mktemp -d)}
mkdir -p "$OUT/classes"
"$PACK/bin/scalac" -nowarn -d "$OUT/classes" "$HERE/SeqDiff.scala" "$HERE/CollDiff.scala"
agent() { if [ -n "${JACOCO_AGENT:-}" ]; then echo "-javaagent:$JACOCO_AGENT=destfile=$OUT/$1.exec,includes=scala.*"; fi; }
run() { tag=$1; shift; java -Xmx400m -Xss512k $(agent $tag) -cp "$PACK/lib/scala-library.jar:$OUT/classes" "$@" > "$OUT/$tag.log" 2>&1 || true; sed -n '/^quarantined/,$p;/^====/,$p' "$OUT/$tag.log" | grep -v exc-type | awk '!seen[$0]++'; }
run seq SeqDiff "$SEED" "$SECS"
for f in buf set mset map mmap; do run $f CollDiff $f "$SEED" "$SECS"; done
echo "logs, with a shrunk counterexample per signature (lines starting NEW): $OUT"
