CXXFLAGS=-std=c++1y -Wall -O3 -g
CFLAGS=-O3 -g -Wall -std=c11 -D_GNU_SOURCE -D_FORTIFY_SOURCE=2
TARGETS=thread_id capabilities fanotify inotify epoll perf-tester nolibc dir mkdir-p
CC=cc
CXX=c++

all: $(TARGETS)

thread_id: thread_id.o
	$(CXX) -pthread $< -o $@ 

capabilities: capabilities.o

fanotify: fanotify.o

inotify: inotify.o

epoll: epoll.o

mkdir-p: mkdir-p.o

dir: dir.o

perf-tester.o: perf-tester.c

perf-tester: perf-tester.o
	$(CC) -Wl,--rpath=/opt/lib -Wl,--dynamic-linker=/opt/lib/ld-2.20.so $< -o $@

nolibc: nolibc.c nolibc-syscall.S nolibc-start.S
	$(CC) -c nolibc-syscall.S -o nolibc-syscall.o
	$(CC) -c nolibc-start.S -o nolibc-start.o
	$(CC) -g -O0 -nostdinc -c nolibc.c -o nolibc.o -std=c11 -fno-builtin -Wall
	$(CC) -nostdlib nolibc.o nolibc-start.o nolibc-syscall.o -o nolibc

clean:
	$(RM) $(TARGETS) *.o
