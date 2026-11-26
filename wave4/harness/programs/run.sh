#!/usr/bin/env bash
# usage: run.sh <first seed> <seconds> <out dir> [noshrink] [dump]
# Env: PACK (build under test, default w3-int), JACOCO_EXEC (write coverage there), W4P_JAVA_OPTS (-Dw4p.threads=6 -Dw4p.progPar=2 -Dw4p.spec=0 ...)
set -eu
SEED=$1; SECS=$2; OUT=$3; shift 3
HERE=$(cd "$(dirname "$0")" && pwd); DRV=$HERE/../driver
export JAVA_HOME=~/.sdkman/candidates/java/21.0.12+1.1-tem; export PATH=$JAVA_HOME/bin:$PATH
PACK=${PACK:-/Users/jzaugg-virtus/code/.worktrees/scala/w3-int/build/pack}
M2=~/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2
V18=$M2/org/scala-lang/scala-compiler/2.13.18/scala-compiler-2.13.18.jar:$M2/org/scala-lang/scala-reflect/2.13.18/scala-reflect-2.13.18.jar:$M2/org/scala-lang/scala-library/2.13.18/scala-library-2.13.18.jar
B=$OUT/../bridge-$(basename "$PACK" | tr / _)-$(echo "$PACK" | md5 -q | cut -c1-6)
mkdir -p "$OUT" "$B"
bash "$DRV/build-bridge.sh" "$B" int "$PACK"; bash "$DRV/build-bridge.sh" "$B" v18 "$V18"
CL=$B/classes; mkdir -p "$CL"
[ -f "$CL/w4p/Hunt.class" ] && [ -z "$(find "$DRV" "$HERE" -name "*.scala" -newer "$CL/w4p/Hunt.class")" ] || "$PACK/bin/scalac" -nowarn -d "$CL" "$DRV/Driver.scala" "$HERE/Gen.scala" "$HERE/Hunt.scala"
AG=""; [ -n "${JACOCO_EXEC:-}" ] && AG="-javaagent:$M2/org/jacoco/org.jacoco.agent/0.8.13/org.jacoco.agent-0.8.13-runtime.jar=destfile=$JACOCO_EXEC,includes=scala.*"
java -Xmx2g -Xss16m $AG ${W4P_JAVA_OPTS:-} -Dw4p.bridge="$B" -Dw4p.int="$PACK" -Dw4p.v18="$V18" \
  -cp "$PACK/lib/scala-library.jar:$CL" w4p.Hunt "$SEED" "$SECS" "$OUT" "$@"
