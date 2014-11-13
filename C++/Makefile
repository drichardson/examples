TARGETS=ofstream_test templates atomics system_error array \
		function function.S condition priority_q chrono \
		crash playground typeid casting

CXXFLAGS=-std=c++1y -Wall -g -O0 -march=native -pthread
LINK.o=$(LINK.cc) # Use c++ link rule by default instead of c link rule

all: $(TARGETS)

glog: glog.cc
	$(CXX) -g -O0 -o glog glog.cc -lglog

function.S: function.cc
	$(CXX) -S $(CXXFLAGS) -o $@ $^

clean:
	$(RM) -r $(TARGETS) *.dSYM a.txt *.o *.S
