#!/usr/bin/env bash
# A partial function with a value-class result whose body is a guarded match with a try arm: run without and with -opt:local
export JAVA_HOME=${JAVA_HOME:-~/.sdkman/candidates/java/21.0.12+1.1-tem}
SCALAC=${SCALAC:-/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack/bin/scalac}
SCALA=${SCALA:-/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack/bin/scala}
cd "$(dirname "$0")"
for fl in "" "-opt:local"; do
  D=$(mktemp -d); echo "-- scalac $fl"
  timeout 120 "$SCALAC" $fl -d "$D" repro/Test.scala 2>&1 | grep -m1 -o "error: .*" | cut -c1-120
  [ -f "$D/Test.class" ] && timeout 60 "$SCALA" -cp "$D" Test 2>&1 | head -1 | cut -c1-120
  rm -rf "$D"
done
