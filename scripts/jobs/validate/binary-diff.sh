#! /bin/bash

set -e

BASESHA=$1
HEADSSHA=$2
REPO=target/binary-diff
mkdir -p $REPO
function g() {
    git --git-dir "$REPO" "$@"
}
g init
git branch code
git branch sigs

function jardiffPack() {
    CP=$(ls "$1/*.jar" | paste -s -d :)
    g co code
    jardiff -g $REPO "$CP"
    g co sigs
    jardiff -c -g $REPO "$CP"
}
git checkout $BASESHA
sbt setupPublishCore clean dist/mkPack
jardiffPack
git checkout -

git checkout $HEADSSHA
sbt setupPublishCore clean dist/mkPack publishLocal
V=$(cat target/version)
jardiffPack

sbt -Dstarr.version=$V clean dist/mkPack
jardiffPack
git checkout -

