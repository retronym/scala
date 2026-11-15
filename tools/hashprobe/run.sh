#!/bin/bash
# usage: run.sh <mode> <corpus> <forks> <extra jvm flags...>
set -e
mode=$1; corpus=$2; forks=$3; shift 3
DUMPS=${DUMPS:?set DUMPS to a directory for the dumps}
D=$DUMPS/$corpus-$mode; rm -rf $D; mkdir -p $D
cd ~/code/retronym/jvm-perf-lab/labs/scala2
export JAVA_HOME=~/.sdkman/candidates/java/21.0.12+1.1-tem PERFLAB_BUILD_MODE=launch SCALA_CHECKOUT=${SCALA_CHECKOUT:?} COMPILER_BENCHMARK=$HOME/code/scala/compiler-benchmark
exec perflab --transcript $DUMPS/t-$corpus-$mode.txt run --local -- -jvm ${JVM:?path to bin/java} -f $forks -wi ${WI:-2} -i ${MI:-3} -w ${WT:-3s} -r ${MT:-3s} -p corpus=$corpus ${SCALAC_OPTS:+-p scalacOptions=$SCALAC_OPTS} ${JDKHOME:+-p jdkHome=$JDKHOME} -jvmArgsPrepend "-Dscala.uniques.layout=chunked -Dscalac.hashdump=$D $*" CompileBenchmark
