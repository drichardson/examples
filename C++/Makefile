TARGETS=ofstream_test templates atomics system_error array \
		function function.S condition priority_q chrono \
		crash playground typeid casting placement_new \
		initialization ctor auto_decltype operators \
		literals template_meta_programming


//CXX=clang++
CXX=g++
CXXFLAGS=-Wall -g -O0 -march=native -pthread

ifeq ($(CXX), clang++)
	CXXFLAGS+=-std=c++14
else
	CXXFLAGS+=-std=c++1y
endif

LINK.o=$(LINK.cc) # Use c++ link rule by default instead of c link rule

all: $(TARGETS)

glog: glog.cc
	$(CXX) -g -O0 -o glog glog.cc -lglog

function.S: function.cc
	$(CXX) -S $(CXXFLAGS) -o $@ $^

clean:
	$(RM) -r $(TARGETS) *.dSYM a.txt *.o *.S
