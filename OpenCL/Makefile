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

UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
LDFLAGS+=-lOpenCL
endif
ifeq ($(UNAME_S),Darwin)
LDFLAGS+=-framework OpenCL
endif


LINK.o=$(LINK.cc) # Use c++ link rule by default instead of c link rule


all: $(TARGETS)

hello.o: hello.cc

clean:
	$(RM) -r $(TARGETS) *.dSYM a.txt *.o
