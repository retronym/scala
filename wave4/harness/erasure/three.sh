#!/usr/bin/env bash
# three.sh File.scala [scalac flags] : compile and run Test on int, base, 2.13.18
export JAVA_HOME=~/.sdkman/candidates/java/21.0.12+1.1-tem; export PATH=$JAVA_HOME/bin:$PATH
S=/private/tmp/claude-501/-Users-jzaugg-virtus-code--worktrees-scala-eager-jackson-1fc898/75f6ecbb-3604-4315-b92e-a7e977e7671a/scratchpad/w4-erasure
. $S/cp18.env; f=$1; shift; n=$(basename $f .scala)
for b in int base; do d=$S/m/out-$n-$b; mkdir -p $d; echo "-- $n $b"; pk=/Users/jzaugg-virtus/code/.worktrees/scala/w3-$b/build/pack
 java -cp "$pk/lib/*" scala.tools.nsc.Main -usejavacp -nowarn "$@" -d $d $f 2>&1 | head -4 | cut -c1-220 && java -cp $pk/lib/scala-library.jar:$d Test 2>&1 | head -3 | cut -c1-200; done
d=$S/m/out-$n-18; mkdir -p $d; echo "-- $n 2.13.18"; java -cp "$CP18" scala.tools.nsc.Main -usejavacp -nowarn "$@" -d $d $f 2>&1 | head -4 | cut -c1-220; java -cp "$CP18$d" Test 2>&1 | head -3 | cut -c1-200
