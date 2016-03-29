TARGETS=main

OPTIMIZATION=-O0
DEBUG=-g
CXX=g++
CXXFLAGS=-std=c++14 -Wall ${DEBUG} ${OPTIMIZATION} -march=native -fstack-protector-all -I/usr/local/include

LINK.o=$(LINK.cc) # Use c++ link rule by default instead of c link rule

all: $(TARGETS)

main: main.o templates.o templates2.o

clean:
	$(RM) -r $(TARGETS) *.dSYM *.o
