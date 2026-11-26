#!/usr/bin/env bash
# A return from inside a method argument (receiver on the operand stack) in a try/finally that has another return
export JAVA_HOME=${JAVA_HOME:-~/.sdkman/candidates/java/21.0.12+1.1-tem}
SCALAC=${SCALAC:-/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack/bin/scalac}
SCALA=${SCALA:-/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack/bin/scala}
D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
cd "$(dirname "$0")"
timeout 120 "$SCALAC" -nowarn -d "$D" repro/G.scala 2>&1 | head -3
timeout 60 "$SCALA" -cp "$D" G 2>&1 | head -2
