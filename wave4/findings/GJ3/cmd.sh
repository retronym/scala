#!/usr/bin/env bash
# Run from the finding directory. Prints the evidence to stdout: the same Scala source compiled
# (A) together with the Java sources and (B) against the class files javac makes from them.
# SCALAC and SCALA may be several words (coursier launch scalac:2.13.18 --).
set -uo pipefail
export JAVA_HOME="${JAVA_HOME:-$HOME/.sdkman/candidates/java/21.0.12+1.1-tem}"; export PATH="$JAVA_HOME/bin:$PATH"
INT=/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack/bin
SCALAC="${SCALAC:-$INT/scalac}"; SCALA="${SCALA:-$INT/scala}"
FLAGS="${FLAGS:--deprecation}"
OUT="$(mktemp -d)" || exit 1
trap 'case "$OUT" in */tmp*|*/T/*) rm -rf -- "$OUT";; esac' EXIT
mkdir "$OUT/a" "$OUT/b"
echo "--- (A) joint: scalac Test.scala *.java, then javac"
if timeout 120 $SCALAC $FLAGS -d "$OUT/a" repro/Test.scala repro/*.java 2>&1; then
  javac -nowarn -d "$OUT/a" repro/*.java && timeout 60 $SCALA -cp "$OUT/a" Test 2>&1
else echo "scalac failed"; fi
echo "--- (B) separate: javac *.java, then scalac Test.scala"
javac -nowarn -d "$OUT/b" repro/*.java
if timeout 120 $SCALAC $FLAGS -cp "$OUT/b" -d "$OUT/b" repro/Test.scala 2>&1; then
  timeout 60 $SCALA -cp "$OUT/b" Test 2>&1
else echo "scalac failed"; fi
