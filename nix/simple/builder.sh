#declare -xp
#echo foo > $out

export PATH="$coreutils/bin:$gcc/bin"
mkdir $out
gcc -o $out/simple $src