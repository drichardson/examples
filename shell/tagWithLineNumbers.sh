#!/bin/sh

exec 3< $1
x=0
while read <&3 myline; do
 echo "$x:$myline";
 x=`expr $x + 1`;
done
