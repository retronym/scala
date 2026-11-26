#!/usr/bin/env bash
set -e
cd "$(dirname "$0")"
SCALAC=${SCALAC:-/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack/bin/scalac}
SCALA=${SCALA:-/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack/bin/scala}
export JAVA_HOME=${JAVA_HOME:-$HOME/.sdkman/candidates/java/21.0.12+1.1-tem}
export PATH=$JAVA_HOME/bin:$PATH
out=$(mktemp -d); trap 'rm -rf "$out"' EXIT
timeout 120 "$SCALAC" -nowarn -d "$out" repro/S.scala
timeout 60 "$SCALA" -cp "$out" S
