#!/usr/bin/env bash
# Run from the finding directory. Prints the evidence to stdout.
set -uo pipefail
export JAVA_HOME="${JAVA_HOME:-$HOME/.sdkman/candidates/java/21.0.12+1.1-tem}"; export PATH="$JAVA_HOME/bin:$PATH"
INT=/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack/bin
SCALAC="${SCALAC:-$INT/scalac}"
OUT="$(mktemp -d)"; trap 'rm -rf "$OUT"' EXIT
for c in "repro/T.scala:" "repro/T.scala:-Ycheck:typer" "repro/t498.scala:-Ycheck:all"; do
  f=${c%%:*}; fl=${c#*:}
  s=$(date +%s)
  timeout 30 "$SCALAC" -nowarn -d "$OUT" $fl $f > "$OUT/log" 2>&1; rc=$?
  echo "scalac $fl $f: exit $rc after $(( $(date +%s) - s ))s (124 means killed by the 30 s timeout)"
done
