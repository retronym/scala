#!/usr/bin/env bash
# usage: run.sh <build/pack dir under test> <cases> <seed> [features]   (OUT=<dir> to keep the work directory)
# With JACOCO=1 the driver JVM runs under the JaCoCo agent and <OUT>/cov.xml is written.
set -eu
PACK=$1; N=${2:-100}; SEED=${3:-1}; FEATS=${4:-K,E,R,S,An,G,I,B,M}
HERE=$(cd "$(dirname "$0")" && pwd); OUT=${OUT:-$(mktemp -d)}
export JAVA_HOME=${JAVA_HOME:-$HOME/.sdkman/candidates/java/21.0.12+1.1-tem}; export PATH=$JAVA_HOME/bin:$PATH
JC=$HOME/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jacoco
mkdir -p "$OUT/classes" "$OUT/cases" "$OUT/work"
[ -f "$OUT/classes/Driver.class" ] || "$PACK/bin/scalac" -nowarn -d "$OUT/classes" "$HERE/Driver.scala"
python3 "$HERE/gen.py" "$OUT/cases" "$N" "$SEED" "$FEATS"
CP=$(ls "$PACK"/lib/scala-library.jar "$PACK"/lib/scala-reflect.jar "$PACK"/lib/scala-compiler.jar | tr '\n' ':')
AG=""; [ -n "${JACOCO:-}" ] && AG="-javaagent:$JC/org.jacoco.agent/0.8.13/org.jacoco.agent-0.8.13-runtime.jar=destfile=$OUT/cov.exec,includes=scala.*"
timeout 600 java -Xmx2g -Xss4m $AG -cp "$CP$OUT/classes" Driver "$OUT/cases" "$OUT/work" > "$OUT/log.txt" 2>&1 || true
if [ -n "${JACOCO:-}" ]; then
  java -jar "$JC/org.jacoco.cli/0.8.13/org.jacoco.cli-0.8.13-nodeps.jar" report "$OUT/cov.exec" \
    --classfiles "$PACK/lib/scala-compiler.jar" --classfiles "$PACK/lib/scala-reflect.jar" --xml "$OUT/cov.xml" >/dev/null
  python3 "$HERE/cov.py" "$OUT/cov.xml"
fi
tail -1 "$OUT/log.txt"; echo "log: $OUT/log.txt"
