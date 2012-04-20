#!/bin/sh
#

rm -f others.txt therest.txt

grep '\$\$' $1 > double-dollar.txt
egrep '\$sp(\b|$)' $1 >specialized.txt
grep '\$$' $1 > module-class.txt
grep '\$class$' $1 > impl-class.txt
grep '\$anon' $1 | grep -v '\$anonfun' > anon.txt
grep '\$anonfun' $1 > anonfun.txt
egrep '\$[0-9]+$' $1 > anon-numbered.txt
grep -v '\$' $1 > alphanumeric.txt

files=$(ls -1 *.txt |grep -v $1 | grep -v others.txt)

cat $files | sort -u > others.txt
comm -2 -3 $1 others.txt > therest.txt
