TARGETS=fixed-size-arrays const-structs binary-tree
CC=gcc
#CC=clang
CFLAGS=-Wall -Werror -O3 -std=c11

all: $(TARGETS)

fixed-size-arrays: fixed-size-arrays.o fixed-size-arrays-other-module.o
	$(CC) -o $@ $^

const-structs: const-structs.o
	$(CC) -o $@ $^

binary-tree: binary-tree.o
	$(CC) -o $@ $^

clean:
	$(RM) $(TARGETS) *.o
