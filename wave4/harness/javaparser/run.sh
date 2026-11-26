#!/usr/bin/env bash
# usage: run.sh <build/pack dir under test> <cases> <seed> [features]   (OUT=<dir> to keep the work directory)
set -eu
PACK=$1; N=${2:-100}; SEED=${3:-1}; FEATS=${4:-K,E,R,S,An,G,I,B,M}
HERE=$(cd "$(dirname "$0")" && pwd); OUT=${OUT:-$(mktemp -d)}
mkdir -p "$OUT/classes" "$OUT/cases" "$OUT/work"
[ -f "$OUT/classes/Driver.class" ] || "$PACK/bin/scalac" -nowarn -d "$OUT/classes" "$HERE/Driver.scala"
python3 "$HERE/gen.py" "$OUT/cases" "$N" "$SEED" "$FEATS"
CP=$(ls "$PACK"/lib/scala-library.jar "$PACK"/lib/scala-reflect.jar "$PACK"/lib/scala-compiler.jar | tr '\n' ':')
java -Xmx2g -Xss4m -cp "$CP$OUT/classes" Driver "$OUT/cases" "$OUT/work" > "$OUT/log.txt" 2>&1 || true
tail -1 "$OUT/log.txt"; echo "log: $OUT/log.txt"
