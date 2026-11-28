#!/usr/bin/env bash
# usage: run.sh <build/pack> <seed list> <out dir> <configs,comma> <part> <parts> <seconds>   (JVM options through W4S_JAVA_OPTS)
# <build/pack> may also be a directory of jars (2.13.18): it needs scala-library, scala-reflect, scala-compiler jars.
set -u
PACK=$1; LIST=$2; OUT=$3; CFG=$4; PART=$5; PARTS=$6; SECS=$7
HERE=$(cd "$(dirname "$0")" && pwd)
export JAVA_HOME=~/.sdkman/candidates/java/21.0.12+1.1-tem; export PATH=$JAVA_HOME/bin:$PATH
if [ -f "$PACK/lib/scala-compiler.jar" ]; then JARS="$PACK/lib/scala-library.jar:$PACK/lib/scala-reflect.jar:$PACK/lib/scala-compiler.jar"; else JARS=$(cat "$PACK/jars.txt"); fi
mkdir -p "$OUT/work$PART"
java -Xmx2g -Xss8m ${W4S_JAVA_OPTS:-} -Dw4s.jars="$JARS" -Dw4s.work="$OUT/work$PART" -cp "$JARS:$W4S_CLASSES" w4s.Driver "$LIST" "$OUT" "$CFG" "$PART" "$PARTS" "$SECS"
