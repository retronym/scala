#!/usr/bin/env bash
# Warm-JVM benchmark of -Yparallel-typer on a corpus (as the oracle names them), from a snapshot of the quick compiler:
#   tools/parallel-typer/bench.sh <corpus> <out-file> [warmup=6] [runs=5] [configs...=serial one 2 4 6 8]
# Prints the medians and, per thread count, the parallel typer's breakdown (signatures, bodies, lock).
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
CORPUS="$1"; OUTF="$2"; WARM="${3:-6}"; RUNS="${4:-5}"; shift 4 2>/dev/null || shift $#
CONFIGS="${*:-serial one 2 4 6 8}"
TMP="$(mktemp -d)"; Q="$ROOT/build/quick/classes"
for d in compiler library reflect; do cp -R "$Q/$d" "$TMP/$d"; done
JARS="$(grep '^TOOL_CLASSPATH=' "$ROOT/build/quick/bin/scalac" | head -1 | sed 's/^TOOL_CLASSPATH="//; s/"$//' | tr ':' '\n' | grep '\.jar$' | paste -sd: -)"
mkdir -p "$TMP/tools" && "$ROOT/build/quick/bin/scalac" -d "$TMP/tools" -cp "$Q/compiler" "$ROOT/tools/parallel-typer/Bench.scala"
# the corpus's argument file, as the oracle makes it
ARGS="$TMP/args"
OUT="$TMP/oracle" bash "$ROOT/tools/parallel-typer/oracle.sh" "$CORPUS" 1 0 > /dev/null 2>&1 || true
cp "$TMP/oracle/args" "$ARGS"
java -Xss64m -Xmx6g ${JAVA_OPTS:-} -Dscalac.parallelTyper.log -cp "$TMP/compiler:$TMP/library:$TMP/reflect:$JARS:$TMP/tools" Bench "$ARGS" "$WARM" "$RUNS" $CONFIGS > "$OUTF" 2>&1
grep median "$OUTF"
python3 - "$OUTF" "$WARM" <<'PY'
import sys, re, statistics
warm = int(sys.argv[2]); res = {}; sig = None
for l in open(sys.argv[1]).read().splitlines():
  m = re.search(r'signatures completed serially in ([\d.]+)', l)
  if m: sig = float(m.group(1))
  m = re.search(r'units typed in ([\d.]+) ms on (\d+) threads; completion lock: held ([\d.]+) ms in (\d+) outermost acquisitions, waited ([\d.]+)', l)
  if m: res.setdefault(m.group(2), []).append((sig, float(m.group(1)), float(m.group(3)), float(m.group(4)), float(m.group(5))))
for k, v in res.items():
  v = v[warm:] or v
  print('%s threads: signatures %.0f ms, bodies %.0f ms, lock held %.0f ms in %.0f acquisitions, waited %.0f ms' % ((k,) + tuple(statistics.median(x[i] for x in v) for i in range(5))))
PY
rm -rf "$TMP"
