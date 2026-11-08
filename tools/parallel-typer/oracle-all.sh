#!/usr/bin/env bash
# The oracle on every corpus: tools/parallel-typer/oracle-all.sh <out-dir> [threads=8] [repeats=5]
# Corpora: library, reflect, compiler, the corpora under tools/parallel-typer/corpus, and the compiler-benchmark corpora
# if $CB_CORPUS points at them (e.g. ~/code/scala/compiler-benchmark/corpus). One summary line per corpus.
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
OUTD="$1"; THREADS="${2:-8}"; REPEATS="${3:-5}"
corpora="library reflect compiler $(ls "$ROOT/tools/parallel-typer/corpus")"
if [ -n "${CB_CORPUS:-}" ]; then for c in better-files re2s scalap induction map-seq-inference patmat-exhaust-huge vector re2j; do corpora="$corpora $CB_CORPUS/$c/latest"; done; fi
for c in $corpora; do
  name=$(echo "$c" | sed 's|/latest$||; s|.*/||')
  OUT="$OUTD/$name" bash "$ROOT/tools/parallel-typer/oracle.sh" "$c" "$THREADS" "$REPEATS" > "$OUTD/$name.log" 2>&1
  echo "$name: $(tail -1 "$OUTD/$name.log")"
done
