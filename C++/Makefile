TARGETS=ofstream_test glog atomics

all: $(TARGETS)

CXXFLAGS=-std=c++1y

ofstream_test: ofstream_test.cc
	$(CXX) -g -O0 -o ofstream_test ofstream_test.cc

glog: glog.cc
	$(CXX) -g -O0 -o glog glog.cc -lglog

atomics: atomics.o
	$(CXX) -g -O0 -o $@ $<

clean:
	$(RM) -r $(TARGETS) *.dSYM a.txt *.o
