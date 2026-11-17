#!/usr/bin/env bash
# Spike harness: compile library, reflect and compiler with build/quick, with and without
# -Ynew-erasure, and compare the classfiles.
#   compare.sh <outdir> [extra scalac flags for the new run]
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
OUT=${1:?outdir}; shift
EXTRA=("$@")
SCALAC=${SCALAC:-"$ROOT/build/quick/bin/scalac"}
DEPS=${DEPS:?set DEPS to the compiler module dependency classpath (scala-asm, jna, ...)}

srcs() { find "$ROOT/src/$1" -name '*.scala' -o -name '*.java'; }

compile_all() {
  local tag=$1; shift
  local o="$OUT/$tag"
  rm -rf "$o"; mkdir -p "$o/library" "$o/reflect" "$o/compiler"
  local t0=$SECONDS
  "$SCALAC" "$@" -nowarn -d "$o/library" -sourcepath "$ROOT/src/library" $(srcs library) > "$o/library.log" 2>&1 || { echo "$tag library FAILED"; tail -30 "$o/library.log"; return 1; }
  "$SCALAC" "$@" -nowarn -d "$o/reflect" -classpath "$o/library" $(srcs reflect) > "$o/reflect.log" 2>&1 || { echo "$tag reflect FAILED"; tail -30 "$o/reflect.log"; return 1; }
  "$SCALAC" "$@" -nowarn -d "$o/compiler" -classpath "$o/library:$o/reflect:$DEPS" $(srcs compiler) > "$o/compiler.log" 2>&1 || { echo "$tag compiler FAILED"; tail -30 "$o/compiler.log"; return 1; }
  echo "$tag compiled in $((SECONDS - t0))s"
}

if [[ ! -d "$OUT/old/compiler" || -n "${REBUILD_OLD:-}" ]]; then compile_all old; fi
compile_all new -Ynew-erasure "${EXTRA[@]}"

cd "$OUT"
( cd old && find . -name '*.class' | sort ) > old.list
( cd new && find . -name '*.class' | sort ) > new.list
comm -3 old.list new.list > missing-or-extra.txt || true
: > differing.txt
while read -r f; do
  [[ -f "new/$f" ]] || continue
  cmp -s "old/$f" "new/$f" || echo "$f" >> differing.txt
done < old.list
echo "classes: $(wc -l < old.list) old, $(wc -l < new.list) new; differing: $(wc -l < differing.txt); missing/extra: $(wc -l < missing-or-extra.txt)"
mkdir -p diffs
while read -r f; do
  d="diffs/$(echo "$f" | tr '/' '_').diff"
  diff <(javap -c -p "old/$f" 2>&1) <(javap -c -p "new/$f" 2>&1) > "$d" || true
done < differing.txt
