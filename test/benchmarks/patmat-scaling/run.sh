#!/bin/bash
# Sweeps one compiler over the shapes and sizes, appending to results/all.csv.
#
#   ./run.sh <label> <s2|s3> <compiler classpath> [extra JVM options]
#
# s2: the classpath holds scala-library, scala-reflect, scala-compiler and scala-asm (class directories or jars).
# s3: the classpath of a Scala 3 compiler (scala3-compiler, scala3-interfaces, tasty-core, scala3-library, scala-library).
#
# A compile can't be interrupted, so when one passes the hard timeout (-Dbench.hardMs, default 120 s) Bench records
# it, writes the shapes still to do to $REST and exits 3; this script then carries on in a fresh JVM.
set -u
cd "$(dirname "$0")"
LABEL=$1 FLAVOUR=$2 CP=$3 EXTRA=${4:-}
SHAPES=${SHAPES:-ctor,typ,tup,opt,guard,seq,ints}
NS=${NS:-8,16,32,64,128,256,512,1024,2048}
JOPTS="-Xms4g -Xmx4g -Xss16m"
REST=results/rest
mkdir -p results srcs
[ -f Gen.class ] && [ -f Bench.class ] || javac Gen.java Bench.java
java Gen ctor,typ,tup,opt,guard,seq,ints,ctorR,tupR,optR $NS srcs
shapes=$SHAPES
while [ -n "$shapes" ]; do
  rm -f $REST
  java $JOPTS $EXTRA -Dbench.restFile=$REST -cp .:$CP Bench $LABEL $FLAVOUR $shapes $NS srcs results/all.csv -usejavacp
  code=$?
  if [ $code = 3 ]; then shapes=$(cat $REST); else [ $code = 0 ] || echo "Bench $LABEL failed with $code"; shapes=""; fi
done
