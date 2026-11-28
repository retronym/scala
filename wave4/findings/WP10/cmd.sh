#!/usr/bin/env bash
# An if with a constant condition whose surviving branch is a call with a `: _*` argument is rejected by refchecks.
set -u
cd "$(dirname "$0")"
export JAVA_HOME=${JAVA_HOME:-~/.sdkman/candidates/java/21.0.12+1.1-tem}
SCALAC=${SCALAC:-/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack/bin/scalac}
SCALA=${SCALA:-/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack/bin/scala}
OUT=$(mktemp -d); trap 'rm -rf "$OUT"' EXIT
timeout 120 "$SCALAC" -d "$OUT" repro/Test.scala 2>&1 | sed "s|$PWD/||" && timeout 60 "$SCALA" -cp "$OUT" Test
