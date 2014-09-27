CXXFLAGS=-std=c++1y -Wall -O0 -g
CFLAGS=-O0 -g -Wall -std=c11 -D_GNU_SOURCE=1
TARGETS=thread_id capabilities fanotify inotify epoll

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

clean:
	$(RM) $(TARGETS) *.o
