TARGETS=\
		binary-tree \
		const \
		const-structs \
		fixed-size-arrays \
		initialization \
		libcurl-test \
		setjmp \
		thread-local

#CC=clang
#CC=gcc

CFLAGS=-Wall -Werror -O3 -std=c17

all: $(TARGETS)

fixed-size-arrays: fixed-size-arrays.o fixed-size-arrays-other-module.o
	$(CC) -o $@ $^

const: const.o
	$(CC) -o $@ $^

const-structs: const-structs.o
	$(CC) -o $@ $^

binary-tree: binary-tree.o
	$(CC) -o $@ $^

initialization: initialization.o
	$(CC) -o $@ $^

libcurl-test: libcurl-test.o
	$(CC) -lcurl -o $@ $^

setjmp: setjmp.o
	$(CC) -lcurl -o $@ $^

thread-local: thread-local.o
	$(CC) -lpthread -o $@ $^

tags: *.c
	ctags -R . /usr/include

clean:
	$(RM) $(TARGETS) *.o tags
