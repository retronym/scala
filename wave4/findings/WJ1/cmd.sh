#!/usr/bin/env bash
# Run from the finding directory. A macro prints, at compile time, the annotations scalac sees on
# Java classes, (A) parsed from the Java sources in a joint compilation and (B) read from javac's class files.
set -uo pipefail
export JAVA_HOME="${JAVA_HOME:-$HOME/.sdkman/candidates/java/21.0.12+1.1-tem}"; export PATH="$JAVA_HOME/bin:$PATH"
INT=/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack/bin
SCALAC="${SCALAC:-$INT/scalac}"; SCALA="${SCALA:-$INT/scala}"
OUT="$(mktemp -d)" || exit 1
trap 'case "$OUT" in */tmp*|*/T/*) rm -rf -- "$OUT";; esac' EXIT
mkdir "$OUT/m" "$OUT/a" "$OUT/b"
timeout 120 $SCALAC -d "$OUT/m" repro/Macro_1.scala || exit 1
echo "--- (A) joint: scalac Test_2.scala *.java"
timeout 120 $SCALAC -cp "$OUT/m" -d "$OUT/a" repro/Test_2.scala repro/*.java 2>&1 && javac -nowarn -d "$OUT/a" repro/*.java && timeout 60 $SCALA -cp "$OUT/m:$OUT/a" Test 2>&1
echo "--- (B) separate: javac *.java, then scalac Test_2.scala"
javac -nowarn -d "$OUT/b" repro/*.java
timeout 120 $SCALAC -cp "$OUT/m:$OUT/b" -d "$OUT/b" repro/Test_2.scala 2>&1 && timeout 60 $SCALA -cp "$OUT/m:$OUT/b" Test 2>&1
