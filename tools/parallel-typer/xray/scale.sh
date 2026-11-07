#!/usr/bin/env bash
# The x-ray's "N independent compilations" section: measure, then write indep-scaling.data.js beside the page. A re-run
# replaces the configurations it measures and keeps the others (scale_report.py).
#
#   tools/parallel-typer/xray/scale.sh [step...]    steps: floor shared fork heap par hw spin (default: all)
#
# Environment: JAVA (the patched JDK 26), CORPORA (default "pekko compiler"), HW_CORPORA (default pekko), ROUNDS (3),
# WARMUP (8), WORK (/tmp/parallel-typer-xray/indep-scaling), SLOT (takes the machine for each measurement; default
# /tmp/parallel-typer-xray/slot.sh timing, if it exists), RESULTS, INDEP_DATA, MAX_LOAD (8).
# A corpus is a name with an args file at /tmp/parallel-typer-xray/corpora/<name>, or `compiler`, from src/compiler.
# Needs `sbt dist/mkQuick`; the compiler and its sources are snapshotted first, so a build during the run does not change them.
# Removes its logs and recordings as it goes; the results stay in scaling-results.jsonl.
set -uo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../../.." && pwd)"
export JAVA="${JAVA:-java}"
WORK="${WORK:-/tmp/parallel-typer-xray/indep-scaling}"
STEPS="${*:-floor shared fork heap par hw spin}"
CORPORA="${CORPORA:-pekko compiler}"
HW_CORPORA="${HW_CORPORA:-pekko}"
R="--rounds ${ROUNDS:-3} --warmup ${WARMUP:-8}"
MAX_LOAD="${MAX_LOAD:-8}"
[ -d "$ROOT/build/quick/classes/compiler" ] || { echo "no build/quick: run sbt dist/mkQuick" >&2; exit 1; }
if [ -z "${SLOT+x}" ]; then [ -x /tmp/parallel-typer-xray/slot.sh ] && SLOT="/tmp/parallel-typer-xray/slot.sh timing" || SLOT=""; fi
export LABEL="${LABEL:-indep-scaling}"
export COMMIT="$(git -C "$ROOT" rev-parse --short=10 HEAD)"

rm -rf "$WORK"; mkdir -p "$WORK/logs" "$WORK/snap" "$WORK/bin" "$WORK/src"
export LOGDIR="$WORK/logs"
for d in compiler library reflect; do cp -R "$ROOT/build/quick/classes/$d" "$WORK/snap/$d"; done
cp -R "$ROOT/src/compiler" "$WORK/src/compiler"
JARS="$(grep '^TOOL_CLASSPATH=' "$ROOT/build/quick/bin/scalac" | head -1 | sed 's/^TOOL_CLASSPATH="//; s/"$//' | tr ':' '\n' | grep '\.jar$' | paste -sd: -)"
S="$WORK/snap"; CP="$S/compiler:$S/library:$S/reflect:$JARS"
"$JAVA" -cp "$CP" -Dscala.usejavacp=true scala.tools.nsc.Main -nowarn -d "$WORK/bin" "$HERE/XrayBench.scala" "$HERE/XrayMemory.scala" "$HERE/XrayScale.scala" || exit 1
LAB="${PERFLAB:-$HOME/code/retronym/jvm-perf-lab}/tools/build/async-profiler/build"
export SNAP="$WORK/bin:$LAB/jar/async-profiler.jar:$CP"
for c in $CORPORA; do
  if [ "$c" = compiler ]; then
    { cat "$ROOT/tools/parallel-typer/compiler.opts"; echo "-classpath"; echo "$S/library:$S/reflect:$(cat "$ROOT/tools/parallel-typer/compiler.cp" 2>/dev/null)";
      find "$WORK/src/compiler" -name '*.scala' -o -name '*.java' | sort; } > "$WORK/$c"
  else cp "/tmp/parallel-typer-xray/corpora/$c" "$WORK/$c" || exit 1; fi
done

# m <tag> <scale.py args...>: one measurement, alone on the machine, once the one-minute load has settled
inner='while [ "$(sysctl -n vm.loadavg | awk "{print int(\$2)}")" -ge "$MAX_LOAD" ]; do sleep 10; done; echo "== $TAG load $(sysctl -n vm.loadavg) $(date +%H:%M:%S)"; exec "$@"'
m() { local tag="$1"; shift; TAG="$tag" MAX_LOAD="$MAX_LOAD" $SLOT bash -c "$inner" _ python3 "$HERE/scale.py" "$@"; }
has() { case " $STEPS " in *" $1 "*) return 0;; *) return 1;; esac; }

PAR="-Dscalac.parallelTyper.parallelParser=true -Dscalac.parallelTyper.loadPackages=true -Dscalac.parallelTyper.parallelPostTyper=true -Dscalac.parallelTyper.parallelLatePhases=true"
# 31 GB, where a 38 GB heap would turn compressed oops off and allocate 40% more
NOPAUSE="-Xmn30g -XX:SurvivorRatio=60 -XX:-UseAdaptiveSizePolicy -XX:+AlwaysPreTouch"

for c in $CORPORA; do
  A="$WORK/$c"
  if has floor; then for h in 8 6 5 4 3; do m "floor-${h}g" shared --args "$A" --heap "${h}g" --tokens k8 $R; done; fi
  if has shared; then
    m shared12 shared --args "$A" --heap 12g $R
    f="$(python3 "$HERE/scale_report.py" --floor "$c")" && m sharedfloor shared --args "$A" --heap "${f}g" $R
  fi
  if has fork; then for n in 1 2 4 6 8; do m fork4 fork --args "$A" --heap 4g --n "$n" $R; done; fi
  if has heap; then
    JAVA_OPTS="-Xmn16g" m young16 shared --args "$A" --heap 24g --tokens k1,k8 $R
    JAVA_OPTS="$NOPAUSE" m nopause shared --args "$A" --heap 31g --tokens k1,k8 $R
    for n in 1 8; do JAVA_OPTS="-Xmn3g -XX:-UseAdaptiveSizePolicy" m fork4-xmn3 fork --args "$A" --heap 4g --n "$n" $R; done
  fi
  if has par; then JAVA_OPTS="$PAR" m par12 shared --args "$A" --heap 12g --tokens k1,k8,p8 $R; fi
done
if has hw; then
  for c in $HW_CORPORA; do
    m hw-12g hw --args "$WORK/$c" --heap 12g --tokens k1,k8 --setup "12 GB" $R
    JAVA_OPTS="$NOPAUSE" m hw-nopause hw --args "$WORK/$c" --heap 31g --tokens k1,k8 --setup "no collections" $R
  done
fi
if has spin; then m spin spin; fi
python3 "$HERE/scale_report.py" --check
rm -rf "$WORK"
