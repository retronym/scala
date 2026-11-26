#!/usr/bin/env bash
# usage: build-bridge.sh <out dir> <name> <build/pack dir | colon-separated compiler,reflect,library jars>
# Compiles W4Bridge with that compiler into <out dir>/<name>, for Compiler(name, pack, <out dir>).
set -eu
OUT=$1; NAME=$2; PACK=$3; HERE=$(cd "$(dirname "$0")" && pwd)
export JAVA_HOME=${JAVA_HOME:-~/.sdkman/candidates/java/21.0.12+1.1-tem}
[ -f "$OUT/$NAME/scala/tools/nsc/W4Bridge.class" ] && exit 0
mkdir -p "$OUT/$NAME"
if [ -d "$PACK/lib" ]; then CP="$PACK/lib/scala-compiler.jar:$PACK/lib/scala-reflect.jar:$PACK/lib/scala-library.jar"; else CP=$PACK; fi
"$JAVA_HOME/bin/java" -cp "$CP" scala.tools.nsc.Main -usejavacp -nowarn -d "$OUT/$NAME" "$HERE/Bridge.scala"
