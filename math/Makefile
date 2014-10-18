CXXFLAGS=-g -Wall -std=c++11
CFLAGS=-g -Wall -std=c11

TARGETS=permutations sine

all: $(TARGETS)

permutations: permutations.o
	$(CXX) -o $@ $^

sine: sine.o
	$(CC) -o $@ $^

clean:
	$(RM) $(TARGETS) *.o

