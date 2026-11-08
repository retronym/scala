#!/usr/bin/env bash
# Clean builds of a synthetic project of K independent copies of one corpus (synth_modules.py) at every split M x T of
# 8 and 16 threads, plus one compile at a time on 1, 2 and 4 threads (each module's own speed-up, for the cost model), in one warm JVM per K, with the ScalaTest build's JVM settings. Writes
# build-synth-<corpus>-<K>.data.js beside build.html.
#
#   JAVA=<patched JDK 26>/bin/java tools/parallel-typer/xray/synth.sh [corpus] [K...]     (default: scalap 16 64)
#
# Environment: PACKAGE (the corpus's own package, when its files share none: org.apache.pekko for pekko), KEEP (packages under it from the classpath: org.apache.pekko.protobufv3), WORK (/tmp/parallel-typer-xray/synth), WARMUP (3), ROUNDS (3), MAX_LOAD (8), SLOT (default
# /tmp/parallel-typer-xray/slot.sh timing). Needs `sbt dist/mkQuick`; the compiler is snapshotted first.
set -uo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../../.." && pwd)"
JAVA="${JAVA:-java}"
CORPUS="${1:-scalap}"; shift || true
KS="${*:-16 64}"
WORK="${WORK:-/tmp/parallel-typer-xray/synth}"
MAX_LOAD="${MAX_LOAD:-8}"
SLOT="${SLOT-/tmp/parallel-typer-xray/slot.sh timing}"
export LABEL="${LABEL:-synth-$CORPUS}"
TOKENS="trad:1x1 trad:1x2 trad:1x4 trad:1x8 trad:2x4 trad:4x2 trad:8x1 trad:1x16 trad:2x8 trad:4x4 trad:8x2 trad:16x1"

S="$WORK/snap"; mkdir -p "$S" "$WORK/bin"
for d in compiler library reflect; do rm -rf "${S:?}/$d"; cp -R "$ROOT/build/quick/classes/$d" "$S/$d"; done
JARS="$(grep '^TOOL_CLASSPATH=' "$ROOT/build/quick/bin/scalac" | head -1 | sed 's/^TOOL_CLASSPATH="//; s/"$//' | tr ':' '\n' | grep '\.jar$' | paste -sd: -)"
CP="$S/compiler:$S/library:$S/reflect:$JARS"
"$JAVA" -cp "$CP" -Dscala.usejavacp=true scala.tools.nsc.Main -nowarn -d "$WORK/bin" "$HERE/XrayBench.scala" "$HERE/XrayMemory.scala" "$HERE/XrayBuild.scala" || exit 1

PAR="-Dscalac.parallelTyper.parallelParser=true -Dscalac.parallelTyper.loadPackages=true -Dscalac.parallelTyper.parallelPostTyper=true -Dscalac.parallelTyper.parallelLatePhases=true"
gate='while [ "$(sysctl -n vm.loadavg | awk "{print int(\$2)}")" -ge "$MAX_LOAD" ]; do sleep 10; done; echo "== load $(sysctl -n vm.loadavg) $(date +%H:%M:%S)"; exec "$@"'
for k in $KS; do
  D="$WORK/$CORPUS-$k"
  [ -f "$D/modules" ] || python3 "$HERE/synth_modules.py" "/tmp/parallel-typer-xray/corpora/$CORPUS" "$k" "$D" ${PACKAGE:+--package "$PACKAGE"} ${KEEP:+--keep "$KEEP"} -J "$S/compiler" -J "$S/reflect" || exit 1
  OUT="$WORK/$CORPUS-$k.out"
  MAX_LOAD="$MAX_LOAD" $SLOT bash -c "$gate" _ "$JAVA" -Xss64m -Xms31g -Xmx31g -Xmn20g -XX:+UseParallelGC $PAR -cp "$WORK/bin:$CP" \
    scala.tools.nsc.xray.XrayBuild "$D/modules" trad:8x1 "${WARMUP:-3}" "${ROUNDS:-3}" $TOKENS | tee "$OUT" | grep -v '^R '
  if grep -q 'errors [1-9]' "$OUT"; then echo "compile errors in $OUT: no report" >&2; exit 1; fi
  # the cost model refitted to this build, and ScalaTest's terms applied unchanged
  (cd "$HERE/sim" && FIT_STRATEGY=trad FIT_OUT="$WORK/fit-$CORPUS-$k.json" python3 fit.py "$D/modules" "$OUT" &&
    FIT_STRATEGY=trad FIT_FIXED=0.05,1,0.01,2 FIT_OUT="$WORK/fit-scalatest-$CORPUS-$k.json" python3 fit.py "$D/modules" "$OUT") | grep -v '^pipe:'
  FIT="$WORK/fit-$CORPUS-$k.json" FIT_FIXED="$WORK/fit-scalatest-$CORPUS-$k.json" BUILD_KEY="synth-$CORPUS-$k" BUILD_DATA="$HERE/build-synth-$CORPUS-$k.data.js" python3 "$HERE/build_report.py" "$D/modules" "$OUT"
done
