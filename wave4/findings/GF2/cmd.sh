#!/usr/bin/env bash
# Run from the finding directory. Prints the evidence to stdout.
set -uo pipefail
export JAVA_HOME="${JAVA_HOME:-$HOME/.sdkman/candidates/java/21.0.12+1.1-tem}"; export PATH="$JAVA_HOME/bin:$PATH"
INT=/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack/bin
SCALAC="${SCALAC:-$INT/scalac}"; SCALA="${SCALA:-$INT/scala}"
OUT="$(mktemp -d)"; trap 'rm -rf "$OUT"' EXIT
timeout 60 javac -d "$OUT" repro/Ann.java
if timeout 120 "$SCALAC" -nowarn -cp "$OUT" -d "$OUT" repro/Test.scala > "$OUT/log" 2>&1; then
  timeout 60 "$SCALA" -cp "$OUT" Test
else
  grep -c 'annotation argument needs to be a constant' "$OUT/log" | sed 's/$/ errors: annotation argument needs to be a constant/'
fi
