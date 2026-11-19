#!/usr/bin/env bash
# tools/parallel-typer/cross-global/run.sh <mode args of RoundBench or Ripple>, with $OUT for scratch space. See README.md.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"; D="$ROOT/tools/parallel-typer/cross-global"
OUT="${OUT:-${TMPDIR:-/tmp}/cross-global}"; Q="$ROOT/build/quick/classes"; A="$OUT/args"; T="$OUT/tools"
AP="${AP:-$HOME/code/retronym/jvm-perf-lab/tools/build/async-profiler/build}"
JARS="$(grep '^TOOL_CLASSPATH=' "$ROOT/build/quick/bin/scalac" | head -1 | sed 's/^TOOL_CLASSPATH="//; s/"$//' | tr ':' '\n' | grep '\.jar$' | paste -sd: -)"
mkdir -p "$A" "$T"
RCP="$Q/compiler:$Q/library:$Q/reflect:$JARS:$T:$AP/jar/async-profiler.jar"
if [ ! -f "$T/.built" ] || [ "$D/RoundBench.scala" -nt "$T/.built" ] || [ "$D/Ripple.scala" -nt "$T/.built" ]; then
  "$ROOT/build/quick/bin/scalac" -nowarn -d "$T" -cp "$Q/compiler:$Q/reflect:$AP/jar/async-profiler.jar" "$D/RoundBench.scala" "$D/Ripple.scala"; touch "$T/.built"
fi
ALL="$Q/library:$Q/reflect:$Q/compiler:$JARS"
srcs() { for d in "$@"; do find "$ROOT/src/$d" -name '*.scala' -o -name '*.java'; done | sort; }
{ echo -nowarn; echo -cp; echo "$ALL"; echo "$ROOT/src/compiler/scala/tools/nsc/transform/Flatten.scala"; } > "$A/round1.args"
{ echo -nowarn; echo -cp; echo "$ALL"; find "$ROOT/src/compiler/scala/tools/nsc/transform" -maxdepth 1 -name '*.scala' | sort; } > "$A/round20.args"
{ echo -nowarn; echo -cp; echo "$ALL"; srcs scalap; } > "$A/scalap.args"
{ echo -nowarn; echo -cp; echo "$ALL"; srcs scaladoc; } > "$A/scaladoc.args"
{ echo -nowarn; echo -sourcepath; echo "$ROOT/src/library"; srcs library; } > "$A/m-library.args"
JARSCP="$JARS:$Q/sharing-macros"
{ echo -nowarn; echo -language:experimental.macros; echo -cp; echo "$Q/library:$Q/sharing-macros"; srcs reflect sharing; } > "$A/m-reflect.args"
{ echo -nowarn; echo -cp; echo "$Q/library:$Q/reflect:$JARSCP"; srcs compiler; } > "$A/m-compiler.args"
{ echo -nowarn; echo -cp; echo "$ALL"; srcs interactive; } > "$A/m-interactive.args"
{ echo -nowarn; echo -cp; echo "$ALL:$Q/interactive"; srcs repl; } > "$A/m-repl.args"
{ echo -nowarn; echo -cp; echo "$ALL:$Q/interactive:$Q/repl"; srcs repl-frontend; } > "$A/m-replFrontend.args"
{ echo -nowarn; echo -cp; echo "$ALL"; srcs scaladoc; } > "$A/m-scaladoc.args"
{ echo -nowarn; echo -cp; echo "$ALL"; srcs scalap; } > "$A/m-scalap.args"
{ echo -nowarn; echo -language:experimental.macros; echo -cp; echo "$Q/library:$JARSCP"; } > "$A/joint-opts.args"
MODE="$1"; shift
case "$MODE" in
  ripple) exec java -Xss64m -cp "$RCP" Ripple "$Q/library" "$OUT/ripple" ;;
  *) cd "$A"; exec java -Xss64m -Xms4g -Xmx10g ${JAVA_OPTS:-} -Drb.out="$OUT/out" -cp "$RCP" RoundBench "$MODE" "$@" ;;
esac
