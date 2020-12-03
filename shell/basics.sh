#!/bin/sh
echo 'Beginning of basic.sh';

x='howdy';
echo 'x: $x';
echo "x: $x";

for x in "1 2 3 5 7 11 13";
do
 echo 'The value of $x is '$x;
done

for x in 1 2 3 5 7 11 13;
do
 echo 'The value of $x is '$x;
done

for x in 1 'a string' something 32;
do
 echo "The value of \$x is $x";
done

# Reverse the contents of a text file.
echo "Here is a file in normal order:"
echo "*******************************"
cat inputfile
echo "*******************************"
echo "Here is a file in reversed order:"
echo "*******************************"
./tagWithLineNumbers.sh inputfile | sort -r -n | sed 's/[0-9]*://'
