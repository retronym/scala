#!/usr/bin/env bash
# Re-check what the parallel typer spike costs: a perflab campaign (jvm-perf-lab's labs/scala2) pairing, within each
# fork, a baseline build, the spike without -Yparallel-typer, and the spike with -Yparallel-typer on one worker.
#
#   tools/parallel-typer/perf/compare.sh <spike-commit> [<baseline-commit>] [--typer|--full] [--scala] [--rounds N] [--dry-run]
#
# The baseline defaults to fd95c5c1c1 (retronym/scala#212, which the spike is built on). Each commit gets a detached
# worktree under $PTYPER_PERF_DIR (default ~/.cache/ptyper-perf), built with `sbt enableOptimizer compiler/compile`
# (the optimizer settings of the published jars). The campaign has four variants, paired by iteration in each fork:
#   base, aa (base in a second class loader: the noise floor), spike (no flag), and par1 (spike, -Yparallel-typer on
#   one worker). Corpora: re2s, and with --scala the `scala` corpus (library, reflect, compiler); not scalap, which
#   stops compiling once scala-reflect carries a top-level `io` package (the vendored scala-sharing).
# It is queued on the perflab server (the machine's own testbed); follow it on the index URL that perflab prints.
# Read the result with `perflab ab report campaigns/<name>.json` in the lab: the paired deltas of `spike` and `par1`
# against `base`, beside `aa`'s, which is how large a delta must be to count.
set -euo pipefail

spike=${1:?usage: compare.sh <spike-commit> [<baseline-commit>] [--typer|--full] [--rounds N] [--dry-run]}
shift
base=fd95c5c1c1
if [[ $# -gt 0 && $1 != --* ]]; then base=$1; shift; fi
mode=typer; rounds=4; dry=""; scala=""
while [[ $# -gt 0 ]]; do
  case $1 in
    --typer) mode=typer;;
    --full) mode=full;;
    --scala) scala=1;;
    --rounds) rounds=$2; shift;;
    --dry-run) dry=--dry-run;;
    *) echo "unknown argument $1" >&2; exit 2;;
  esac
  shift
done

repo=$(git rev-parse --show-toplevel)
lab=${PERFLAB_SCALA2_LAB:-$HOME/code/retronym/jvm-perf-lab/labs/scala2}
jvm=${PTYPER_PERF_JVM:-$HOME/code/.worktrees/jdk/jdk26-integration-branch-e9e67e/build/release/images/jdk/bin/java}
corpora=${COMPILER_BENCHMARK:-$HOME/code/scala/compiler-benchmark}/corpus
dir=${PTYPER_PERF_DIR:-$HOME/.cache/ptyper-perf}
export JAVA_HOME=${PTYPER_PERF_BUILD_JDK:-${JAVA_HOME:?set JAVA_HOME to a JDK 17-25 for the builds}}

worktree() { # <commit> -> path of an optimized build of it
  local sha; sha=$(git -C "$repo" rev-parse --short=10 "$1^{commit}")
  local wt=$dir/$sha
  if [[ ! -d $wt ]]; then git -C "$repo" worktree add --detach "$wt" "$sha" >&2; fi
  if [[ ! -f $wt/.ptyper-built ]]; then
    (cd "$wt" && sbt -batch enableOptimizer compiler/compile >&2) && touch "$wt/.ptyper-built"
  fi
  echo "$wt"
}
basewt=$(worktree "$base")
spikewt=$(worktree "$spike")
bs=$(basename "$basewt"); ss=$(basename "$spikewt")

opts="-p jdkHome=25.0.4-tem"
[[ $mode == typer ]] && opts="$opts -p scalacOptions=-Ystop-after:typer"
corpus_scala=""
[[ -n $scala ]] && corpus_scala=",
    \"scala\": \"-f 1 -bm ss -wi 24 -i 24 $opts\""
name="ptyper-guard-$mode-$ss"
spec=$lab/campaigns/$name.json
cat > "$spec" <<EOF
{
  "name": "$name",
  "description": ["What the parallel typer spike at \`$ss\` costs against \`$bs\` ($mode), paired within each fork. \`aa\` is \`base\` in a second class loader: the noise floor. Made by tools/parallel-typer/perf/compare.sh."],
  "variantDescriptions": {
    "base": "\`$bs\`",
    "aa": "\`base\` again: the A/A",
    "spike": "\`$ss\` without \`-Yparallel-typer\`",
    "par1": "\`$ss\` with \`-Yparallel-typer -Yparallel-typer-threads:1\`"
  },
  "benchmark": "CompileBenchmark",
  "profile": "none",
  "jvm": "$jvm",
  "jvmArgs": "-Xms8G -Xmx8G -Xss2M -XX:ReservedCodeCacheSize=1g -XX:-UseCodeCacheFlushing -Dcompilerlab.corpora=$corpora",
  "paired": {
    "base": "@build=$basewt",
    "aa": "@build=$basewt",
    "spike": "@build=$spikewt",
    "par1": "@build=$spikewt -Yparallel-typer -Yparallel-typer-threads:1"
  },
  "corpora": {
    "re2s": "-f 1 -wi 32 -w 2 -i 48 -r 2 $opts"$corpus_scala
  },
  "rounds": $rounds,
  "loadgate": {"maxCores": 8.0, "quietSecs": 30},
  "gcLog": true
}
EOF
echo "spec: $spec" >&2
cd "$lab"
SCALA_CHECKOUT=$basewt SCALA_RELEASE_OPT=1 PERFLAB_BUILD_MODE=launch timeout 20 perflab ab run "campaigns/$name.json" $dry || true
