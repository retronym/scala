#!/usr/bin/env bash
# usage: run.sh <build/pack> <family|all> <seconds> [seed] ; OUT=<dir> JACOCO=1 to record coverage into $OUT/<tag>.exec
set -eu
PACK=$1; FAM=$2; SECS=$3; SEED=${4:-1}
export JAVA_HOME=${JAVA_HOME:-$HOME/.sdkman/candidates/java/21.0.12+1.1-tem}; export PATH=$JAVA_HOME/bin:$PATH
HERE=$(cd "$(dirname "$0")" && pwd); OUT=${OUT:-$(mktemp -d)}; mkdir -p "$OUT/classes"
if [ ! -e "$OUT/classes/LibDiff.class" ] || [ -n "$(find "$HERE" -name '*.scala' -newer "$OUT/classes/LibDiff.class")" ]; then
  "$PACK/bin/scalac" -nowarn -deprecation:false -d "$OUT/classes" "$HERE"/*.scala
fi
J=~/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jacoco/org.jacoco.agent/0.8.13/org.jacoco.agent-0.8.13-runtime.jar
AG=""; if [ -n "${JACOCO:-}" ]; then AG="-javaagent:$J=destfile=$OUT/${TAG:-run}.exec,includes=scala.*"; fi
timeout 240 java -Xmx2g -Xss4m $AG -cp "$PACK/lib/scala-library.jar:$OUT/classes" LibDiff "$FAM" "$SEED" "$SECS"
