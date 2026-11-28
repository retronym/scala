#!/usr/bin/env bash
# Run from the finding directory. Prints the evidence to stdout.
set -uo pipefail
export JAVA_HOME="${JAVA_HOME:-$HOME/.sdkman/candidates/java/21.0.12+1.1-tem}"; export PATH="$JAVA_HOME/bin:$PATH"
INT=/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack/bin
SCALAC="${SCALAC:-$INT/scalac}"
OUT="$(mktemp -d)"; trap 'rm -rf "$OUT"' EXIT
for mode in default -Ydelambdafy:inline; do
  echo "== $mode"
  if [ "$mode" = default ]; then fl=""; else fl="$mode"; fi
  if timeout 120 "$SCALAC" -nowarn -d "$OUT" $fl repro/T.scala > "$OUT/compile.log" 2>&1; then echo "compiled"; else echo "scalac failed:"; head -n 3 "$OUT/compile.log"; fi
done
