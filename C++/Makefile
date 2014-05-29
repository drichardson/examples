TARGETS=ofstream_test templates atomics
CXXFLAGS=-std=c++1y -Wall -g -O0

all: $(TARGETS)

ofstream_test: ofstream_test.o
	$(CXX) -o $@ $<

glog: glog.cc
	$(CXX) -g -O0 -o glog glog.cc -lglog

atomics: atomics.o
	$(CXX) -o $@ $<
	
templates: templates.o
	$(CXX) -o $@ $<

clean:
	$(RM) -r $(TARGETS) *.dSYM a.txt *.o
