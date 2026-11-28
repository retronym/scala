#!/usr/bin/env bash
# usage: cov.sh <jacoco .exec> <out .xml> [pack]  -- XML report against the compiler jars, then line coverage per scope and the largest unreached methods
set -eu
EXEC=$1; XML=$2; PACK=${3:-/Users/jzaugg-virtus/code/.worktrees/scala/w4-int/build/pack}
export JAVA_HOME=~/.sdkman/candidates/java/21.0.12+1.1-tem
M2=~/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2
"$JAVA_HOME/bin/java" -jar $M2/org/jacoco/org.jacoco.cli/0.8.13/org.jacoco.cli-0.8.13-nodeps.jar report "$EXEC" \
  --classfiles "$PACK/lib/scala-compiler.jar" --classfiles "$PACK/lib/scala-reflect.jar" --xml "$XML" --quiet
python3 "$(dirname "$0")/cov.py" "$XML"
