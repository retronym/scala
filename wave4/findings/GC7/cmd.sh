#!/usr/bin/env bash
# Run from the finding directory. Prints the evidence to stdout.
set -euo pipefail
export JAVA_HOME="${JAVA_HOME:-$HOME/.sdkman/candidates/java/21.0.12+1.1-tem}"; export PATH="$JAVA_HOME/bin:$PATH"
INT=/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack/bin
SCALAC="${SCALAC:-$INT/scalac}"; SCALA="${SCALA:-$INT/scala}"
OUT="$(mktemp -d)"; trap 'rm -rf "$OUT"' EXIT
timeout 120 "$SCALAC" -nowarn -d "$OUT" repro/Test.scala
timeout 120 "$SCALA" -cp "$OUT" Test
