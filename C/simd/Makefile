all:
	gcc -S loop.c -o loop-no-optimizations.s
	gcc -S loop.c -O3 -o loop-o3.s
	gcc -S loop.c -O3 -march=native -o loop-o3-native.s
	gcc -S loop.c -O3 -mavx -o loop-o3-mavx.s

clean:
	$(RM) *.s
