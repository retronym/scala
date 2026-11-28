#!/usr/bin/env bash
# Compile a compiler-benchmark corpus phase-major and with -Yunit-major-from:<phase>, then
# compare the class files byte for byte.
#   compare.sh <corpus: scalap|re2s|scala> <phase> [extra scalac options...]
# Phase-major output is cached per corpus+options in $OUT/<corpus>-base.
set -euo pipefail
here=$(cd "$(dirname "$0")/../.." && pwd)
corpus=$1; phase=$2; shift 2
CORPORA=${CORPORA:-$HOME/code/scala/compiler-benchmark/corpus}
OUT=${OUT:-${TMPDIR:-/tmp}/unit-major}
dir=$CORPORA/$corpus/latest/
opts=(-nowarn "$@")
[ "$corpus" = scala ] || opts+=(-classpath "$here/build/quick/classes/library:$here/build/quick/classes/reflect:$here/build/quick/classes/compiler")
case $corpus in
  scala)
    asm=$(find "$HOME/code/retronym/jvm-perf-lab/labs/scala2/target/deps" -name 'scala-asm-*.jar' | head -1)
    opts+=(-sourcepath "$dir/library" -classpath "$asm") ;;
esac
mkdir -p "$OUT"
find "$dir" \( -name '*.scala' -o -name '*.java' \) | sort > "$OUT/$corpus.files"
# Run quick's compiler directly rather than build/quick/bin/scalac, which puts quick's library on
# the compilation classpath (-usejavacp): the scala corpus brings its own library sources.
q=$here/build/quick/classes
scalac() { "${JAVA_HOME:+$JAVA_HOME/bin/}java" ${JAVA_OPTS:--Xmx6g -Xss8m} -cp "$q/compiler:$q/reflect:$q/library:$(sed -n "s/^TOOL_CLASSPATH=\"\(.*\)\"$/\1/p" "$here/build/quick/bin/scalac" | tr : "\n" | grep scala-asm)" scala.tools.nsc.Main "$@"; }
key=$(echo "${opts[*]}" | shasum | cut -c1-8)
base=$OUT/$corpus-base-$key
if [ ! -d "$base" ]; then
  mkdir -p "$base.tmp"
  scalac "${opts[@]}" -d "$base.tmp" @"$OUT/$corpus.files"
  mv "$base.tmp" "$base"
fi
new=$OUT/$corpus-$phase-$key
rm -rf "$new"; mkdir -p "$new"
scalac "${opts[@]}" -Yunit-major-from:"$phase" -d "$new" @"$OUT/$corpus.files"
n=$(find "$base" -name '*.class' | wc -l | tr -d ' ')
if diff -rq "$base" "$new" > "$OUT/$corpus-$phase.diff"; then
  echo "$corpus from $phase: identical ($n classes)"
else
  echo "$corpus from $phase: $(wc -l < "$OUT/$corpus-$phase.diff" | tr -d ' ') differences of $n classes, see $OUT/$corpus-$phase.diff"
  exit 1
fi
