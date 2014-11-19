TARGETS=hello

OPTIMIZATION=-O3
DEBUG=
CXX=g++
CXXFLAGS=-Wall ${DEBUG} ${OPTIMIZATION} -march=native

ifeq ($(CXX), clang++)
	CXXFLAGS+=-std=c++14
else
	CXXFLAGS+=-std=c++1y
endif

LINK.o=$(LINK.cc) # Use c++ link rule by default instead of c link rule

LDFLAGS=-framework OpenCL

all: $(TARGETS)

hello.o: hello.cc

clean:
	$(RM) -r $(TARGETS) *.dSYM a.txt *.o
