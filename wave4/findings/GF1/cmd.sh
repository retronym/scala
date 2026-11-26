#!/usr/bin/env bash
# Run from the finding directory. Prints the evidence to stdout.
set -uo pipefail
export JAVA_HOME="${JAVA_HOME:-$HOME/.sdkman/candidates/java/21.0.12+1.1-tem}"; export PATH="$JAVA_HOME/bin:$PATH"
INT=/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack/bin
SCALAC="${SCALAC:-$INT/scalac}"; SCALA="${SCALA:-$INT/scala}"
OUT="$(mktemp -d)"; trap 'rm -rf "$OUT"' EXIT
if timeout 120 "$SCALAC" -nowarn -d "$OUT" repro/Test.scala > "$OUT/log" 2>&1; then
  timeout 60 "$SCALA" -cp "$OUT" Test
else
  grep -m1 -A1 'assertion failed' "$OUT/log" | sed 's/^ *//'
fi
