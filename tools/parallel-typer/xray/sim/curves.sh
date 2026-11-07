#!/usr/bin/env bash
# The simulator's curves: the parallel compiler's speed-up by thread count, whole compiles and up to the pickler, and how
# N independent compiles slow each other with a young generation sized for the load.
#
#   JAVA=<patched JDK 26>/bin/java CP=<compiler snapshot, its jars and XrayScale's classes> tools/parallel-typer/xray/sim/curves.sh <dir of corpus args files> [corpus...]
#
# Takes the machine for each run through SLOT (default /tmp/parallel-typer-xray/slot.sh timing).
set -uo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ARGS="$1"; shift
CORPORA="${*:-pekko compiler}"
SLOT="${SLOT-/tmp/parallel-typer-xray/slot.sh timing}"
export LABEL="${LABEL:-sim-curves}"
PAR="-Dscalac.parallelTyper.parallelParser=true -Dscalac.parallelTyper.loadPackages=true -Dscalac.parallelTyper.parallelPostTyper=true -Dscalac.parallelTyper.parallelLatePhases=true"
gate='while [ "$(sysctl -n vm.loadavg | awk "{print int(\$2)}")" -ge 8 ]; do sleep 10; done; exec "$@"'
m() { local out="$1"; shift; $SLOT bash -c "$gate" _ "$JAVA" -Xss64m -XX:+UseParallelGC "$@" | grep '^R ' > "$HERE/curves/$out.out"; }
for c in $CORPORA; do
  m "$c-full" -Xmx24g -Xms24g -Xmn16g $PAR -Dxray.full=true -cp "$CP" XrayScale "$ARGS/$c" 10 3 k1 p1 p2 p4 p6 p8
  m "$c-pre" -Xmx24g -Xms24g -Xmn16g $PAR -cp "$CP" XrayScale "$ARGS/$c" 10 3 k1 p1 p2 p4 p6 p8
  m "$c-load" -Xmx31g -Xms31g -Xmn24g -XX:-UseAdaptiveSizePolicy -XX:+AlwaysPreTouch -Dxray.full=true -cp "$CP" XrayScale "$ARGS/$c" 10 3 k1 k2 k4 k6 k8 k12 k16
done
