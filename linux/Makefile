CXXFLAGS=-std=c++1y -Wall -O0 -g
CFLAGS=-O0 -g -Wall
TARGETS=thread_id capabilities fanotify

all: $(TARGETS)

thread_id: thread_id.o
	g++ -pthread $< -o $@ 

capabilities: capabilities.o
	gcc $< -o $@

fanotify: fanotify.o
	gcc $< -o $@

clean:
	$(RM) $(TARGETS) *.o
