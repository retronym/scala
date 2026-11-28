#!/usr/bin/env bash
set -eu
cd "$(dirname "$0")"
export JAVA_HOME=${JAVA_HOME:-$HOME/.sdkman/candidates/java/21.0.12+1.1-tem}; export PATH=$JAVA_HOME/bin:$PATH
SCALAC=${SCALAC:-/Users/jzaugg-virtus/code/.worktrees/scala/w4-int/build/pack/bin/scalac}
SCALA=${SCALA:-/Users/jzaugg-virtus/code/.worktrees/scala/w4-int/build/pack/bin/scala}
T=$(mktemp -d); trap 'rm -r "$T"' EXIT
timeout 120 "$SCALAC" -d "$T" repro/Test.scala
timeout 60 "$SCALA" -cp "$T" Test
