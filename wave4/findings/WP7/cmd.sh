#!/usr/bin/env bash
# A value class constructed from a literal in one branch of an if, the other from a variable, used as a receiver
export JAVA_HOME=${JAVA_HOME:-~/.sdkman/candidates/java/21.0.12+1.1-tem}
SCALAC=${SCALAC:-/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack/bin/scalac}
SCALA=${SCALA:-/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack/bin/scala}
D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
cd "$(dirname "$0")"
timeout 120 "$SCALAC" -nowarn -d "$D" repro/Test.scala 2>&1 | grep -E "^error|mergePrefixAndArgs|during phase" | head -3
timeout 60 "$SCALA" -cp "$D" Test 2>&1 | head -1
