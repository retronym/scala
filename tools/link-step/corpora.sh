#!/usr/bin/env bash
# Reproduces the runs in RESULTS.md. Needs `sbt dist/mkQuick` first.
#   tools/link-step/corpora.sh OUT_DIR [CATS_BUILD_DIR]
# CATS_BUILD_DIR defaults to ~/code/.integration/builds/cats (an exported cats build: per-module src/ and lib/).
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
out="$(mkdir -p "$1" && cd "$1" && pwd)"
cats_src="${2:-$HOME/code/.integration/builds/cats}"
q="$root/build/quick/classes"
toolcp="$(sed -n 's/^TOOL_CLASSPATH="\(.*\)"$/\1/p' "$root/build/quick/bin/scalac")"
deps="$q/library:$q/reflect:$q/compiler:$q/interactive:$q/repl:$q/scaladoc:$q/scalap:$toolcp"

for m in reflect compiler interactive scaladoc scalap; do
  "$here/run" --corpus "$q/$m" --cp "$deps" --label "$m" --out "$out" > "$out/$m.log" 2>&1
done

if [[ -d "$cats_src" ]]; then
  cats="$out/cats-classes"
  compat="$(ls "$cats_src"/coreJVM/lib/scalac-compat-annotation_*.jar)"
  plugins="$(sed -n 's/^scalac.options = .*\(-Xplugin:[^ ]*\) \(-Xplugin:[^ ]*\)$/\1 \2/p' "$cats_src/coreJVM/incOptions.properties")"
  opts="-nowarn -language:_ -Xsource:3 -release 8 $plugins"
  cp="$compat"
  for m in kernel core free; do
    if [[ ! -d "$cats/$m" ]]; then
      mkdir -p "$cats/$m"
      # shellcheck disable=SC2046,SC2086
      "$root/build/quick/bin/scalac" $opts -cp "$cp" -d "$cats/$m" $(find "$cats_src/${m}JVM" -name '*.scala')
    fi
    cp="$cp:$cats/$m"
  done
  for m in kernel core free; do
    "$here/run" --corpus "$cats/$m" --cp "$q/library:$q/reflect:$cp" --label "cats-$m" --out "$out" -- -Xsource:3 -release 8 > "$out/cats-$m.log" 2>&1
  done
fi
grep -h '' "$out"/*-summary.txt
