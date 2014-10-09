CXXFLAGS=-std=c++1y -Wall -O3 -g
CFLAGS=-O3 -g -Wall -std=c11 -D_GNU_SOURCE -D_FORTIFY_SOURCE=2
TARGETS=thread_id capabilities fanotify inotify epoll perf-tester nolibc dir mkdir-p
CC=gcc

all: $(TARGETS)

thread_id: thread_id.o
	g++ -pthread $< -o $@ 

capabilities: capabilities.o
	gcc $< -o $@

fanotify: fanotify.o
	gcc $< -o $@

inotify: inotify.o
	gcc $< -o $@

epoll: epoll.o
	gcc $< -o $@

mkdir-p: mkdir-p.o
	gcc $< -o $@

dir: dir.o
	gcc $< -o $@

perf-tester.o: perf-tester.c
	gcc $(CFLAGS) -nostdinc -I/opt/include -I/usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.1/include -c -o $@ $<

perf-tester: perf-tester.o
	gcc -Wl,--rpath=/opt/lib -Wl,--dynamic-linker=/opt/lib/ld-2.20.so $< -o $@

nolibc: nolibc.c nolibc-syscall.S nolibc-start.S
	gcc -c nolibc-syscall.S -o nolibc-syscall.o
	gcc -c nolibc-start.S -o nolibc-start.o
	gcc -g -O0 -nostdinc -c nolibc.c -o nolibc.o -std=c11 -fno-builtin -Wall
	gcc -nostdlib nolibc.o nolibc-start.o nolibc-syscall.o -o nolibc

clean:
	$(RM) $(TARGETS) *.o
