#!/usr/bin/env bash
# Reports of other threads while a macro calls c.parse. A macro must be compiled apart from its uses, so this is not a
# corpus: it compiles one, writes the sources that use it, and runs the stress harness on them.
#   tools/parallel-typer/macro-parse.sh [iterations=60] [threads=8]
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
T="$(mktemp -d)"; mkdir -p "$T/m" "$T/src"
cat > "$T/m.scala" <<'SRC'
package mp
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
object M {
  def parsed: Int = macro impl
  def impl(c: Context): c.Tree = c.parse("{ " + (1 to 200).map(i => s"val a$i = $i + 1").mkString("; ") + "; 1 }")
}
SRC
"$ROOT/build/quick/bin/scalac" -d "$T/m" "$T/m.scala"
for i in $(seq 0 15); do
  { echo "package u"; echo "object U$i {"
    for k in $(seq 0 19); do echo "  def p$k = mp.M.parsed"; echo "  def w$k(x: Int) = try x catch { case _ => 2 }"; done
    echo "}"; } > "$T/src/U$i.scala"
done
{ echo -cp; echo "$T/m"; ls "$T"/src/*.scala; } > "$T/args"
"$ROOT/tools/parallel-typer/stress.sh" "$T/args" "${1:-60}" "${2:-8}"
