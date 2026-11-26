#!/usr/bin/env bash
# usage: run.sh <build/pack dir> <first seed> <seconds> <out dir> [ycheck] [noshrink]
set -eu
PACK=$1; SEED=$2; SECS=$3; OUT=$4; shift 4
HERE=$(cd "$(dirname "$0")" && pwd)
export JAVA_HOME=~/.sdkman/candidates/java/21.0.12+1.1-tem; export PATH=$JAVA_HOME/bin:$PATH
mkdir -p "$OUT/classes" "$OUT/work"
[ -f "$OUT/classes/w4e/Driver.class" ] || "$PACK/bin/scalac" -nowarn -d "$OUT/classes" "$HERE/Gen.scala" "$HERE/Driver.scala"
java -Xmx2g -Xss8m ${W4E_JAVA_OPTS:-} -Dw4e.lib="$PACK/lib/scala-library.jar" -Dw4e.work="$OUT/work" \
  -cp "$PACK/lib/scala-library.jar:$PACK/lib/scala-reflect.jar:$PACK/lib/scala-compiler.jar:$OUT/classes" w4e.Driver "$SEED" "$SECS" "$OUT" "$@"
