#!/bin/sh
#

rm -f others.txt therest.txt

grep '\$\$' $1 > double-dollar.txt
egrep '\$sp(\b|$)' $1 >specialized.txt
grep '\$scala\$' $1 > fqname.txt
grep '_\$eq$' $1 > setter.txt
grep '\$anonfun' $1 > anonfun.txt
egrep '\$[0-9]+$' $1 > anon-numbered.txt
grep '_setter_' $1 > trait-setter.txt

files=$(ls -1 *.txt |grep -v $1)

cat $files | sort -u > others.txt
comm -2 -3 $1 others.txt > therest.txt
