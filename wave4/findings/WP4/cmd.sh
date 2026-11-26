#!/usr/bin/env bash
# -Ycheck:all on a by-name argument that reads a trait's lazy val of a type-parameter type, from a subclass; then the program's output
export JAVA_HOME=${JAVA_HOME:-~/.sdkman/candidates/java/21.0.12+1.1-tem}
SCALAC=${SCALAC:-/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack/bin/scalac}
SCALA=${SCALA:-/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack/bin/scala}
D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
cd "$(dirname "$0")"
timeout 120 "$SCALAC" -Ycheck:all -d "$D" repro/Test.scala 2>&1 | grep -E "^Caught|Tree has erroneous|during phase|anonfun.*=" | sort | uniq -c
timeout 60 "$SCALA" -cp "$D" Test
