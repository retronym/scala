#! /bin/bash

set -e

BASESHA=$1
HEADSSHA=$2
DIR=jardiff
mkdir -p $DIR
REPO1=$DIR/sigs-repo
REPO2=$DIR/code-repo
PACK_DIR=build/pack

: ${CLEAN_CMD:=clean}

rm -rf "$REPO1"
rm -rf "$REPO2"

function cleanPack() {
    rm -rf "$PACK_DIR"
}
function fail() {
    echo "$1" 1>&2
    exit 1
}

function version() {
    VERSION_FILE=target/version
    if [[ -f "$VERSION_FILE" ]]; then
        cat "$VERSION_FILE"
    else
        # Backwards compatibility with older revisions that don't write target/version
        sbt setupPublishCore 'scala-dist/version' | tail -n 1 | sed 's/.* //'
    fi
}

function jardiffPack() {
    CP=$(find "build/pack/lib" -name '*.jar' | paste -s -d: -)
    NAME=$1
    if [[ "" -eq "$NAME" ]]; then
        jardiff -q -c -g $REPO1 "$CP"
        jardiff -q -g $REPO2 "$CP"        
    else
        jardiff    -c -g $REPO1 "$CP" > $DIR/sigs-$NAME.diff
        jardiff    -g $REPO2 "$CP" > $DIR/code-$NAME.diff
    fi
}

git checkout $BASESHA
cleanPack
sbt $CLEAN_CMD setupPublishCore publishLocal
BASEVERSION=$(version)
sbt -Dstarr.version=$BASEVERSION setupPublishCore $CLEAN_CMD scalaVersion dist/mkPack
jardiffPack baseline

git checkout $HEADSSHA
cleanPack
sbt -Dstarr.version=$BASEVERSION $CLEAN_CMD setupPublishCore dist/mkPack publishLocal
HEADVERSION=$(version)
jardiffPack regular

cleanPack
sbt -Dstarr.version=$HEADVERSION $CLEAN_CMD setupPublishCore dist/mkPack
jardiffPack bootstrap
