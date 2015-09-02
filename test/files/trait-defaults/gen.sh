rm -rf classes
mkdir -p phases
mkdir -p classes

for p in erasure explicitouter flatten mixin
  do for i in *.scala
    do mkdir -p classes/$i
       scalac -d classes/$i -Xprint:$p $i > phases/$i.$p
    done
  done
