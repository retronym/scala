#! /bin/bash

set -e

BASESHA=$1
HEADSSHA=$2
REPO=binary-diff
mkdir -p $REPO
function g() {
    git --git-dir "$REPO/.git" "$@"
}
function fail() {
    echo "$1" 1>&2
    exit 1
}
g init
g commit --allow-empty -m "dummy"
g co master
for b in code sigs; do
  g branch -D -f $b || true
  g branch $b
done

function jardiffPack() {
    CP=$(find "build/pack/lib" -name '*.jar' | paste -s -d: -)
    g co code
    jardiff -q -g $REPO "$CP"
    g co sigs
    jardiff -q -c -g $REPO "$CP"
}
git checkout $BASESHA
# sbt setupPublishCore clean dist/mkPack
# jardiffPack

git checkout $HEADSSHA
sbt setupPublishCore clean version dist/mkPack publishLocal
VERSION_FILE=target/version
[[ -f "$VERSION_FILE" ]] || fail "$VERSION_FILE not written by the SBT build"
V=$(cat "$VERSION_FILE")
jardiffPack

sbt -Dstarr.version=$V clean dist/mkPack
jardiffPack
