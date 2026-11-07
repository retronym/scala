#!/bin/sh
# Builds the library with sbt (for the classpath), then typechecks src/reflect and src/compiler with a Global and
# extracts tools/state-map/state-map.json, state-map.data.js (for index.html) and worklist.md.
# SBT_OPTS defaults to -Xmx4g. Extra arguments go to StateMap.scala (e.g. --github URL).
set -e
cd "$(dirname "$0")/../.."
export SBT_OPTS="${SBT_OPTS:--Xmx4g}"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
sbt -batch 'export compiler/dependencyClasspath' 'show compiler/Compile/scalacOptions' > "$TMP/sbt.log" 2>&1 || { cat "$TMP/sbt.log"; exit 1; }
# the reflect classes are left out: their sources are typechecked with the compiler's
CP=$(grep '^/' "$TMP/sbt.log" | tail -1 | tr ':' '\n' | grep -v '/build/quick/classes/reflect$' | paste -sd: -)
sed -n 's/^\[info\] \* //p' "$TMP/sbt.log" > "$TMP/opts"
TIP=$(git log -1 --format=%H -- src/reflect src/compiler)
scala-cli run tools/state-map/StateMap.scala -- \
  --repo . --cp "$CP" --opts-file "$TMP/opts" --tip "$TIP" --out tools/state-map/state-map.json "$@"
