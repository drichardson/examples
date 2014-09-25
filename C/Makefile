TARGETS=fixed-size-arrays
CC=gcc
CC=clang
CFLAGS=-Wall -Werror -O3 -std=c11

all: $(TARGETS)

fixed-size-arrays: fixed-size-arrays.o fixed-size-arrays-other-module.o
	$(CC) -o $@ $^

clean:
	$(RM) $(TARGETS) *.o
