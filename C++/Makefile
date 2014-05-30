TARGETS=ofstream_test templates atomics system_error array \
		function function.S condition priority_q chrono

CXXFLAGS=-std=c++1y -Wall -g -O0 -march=native -pthread
#CXXFLAGS=-std=c++1y -Wall -O3 -march=native -pthread

all: $(TARGETS)

ofstream_test: ofstream_test.o
	$(CXX) -o $@ $<

glog: glog.cc
	$(CXX) -g -O0 -o glog glog.cc -lglog

atomics: atomics.o
	$(CXX) -o $@ $<
	
templates: templates.o
	$(CXX) -o $@ $<

system_error: system_error.o
	$(CXX) -o $@ $<
	
array: array.o
	$(CXX) -o $@ $<
	
function: function.o
	$(CXX) -pthread -o $@ $<

function.S: function.cc
	$(CXX) -S $(CXXFLAGS) -o $@ $<

condition: condition.o
	$(CXX) -pthread -o $@ $<

priority_q: priority_q.o
	$(CXX) -o $@ $<

chrono: chrono.o
	$(CXX) -o $@ $<

clean:
	$(RM) -r $(TARGETS) *.dSYM a.txt *.o *.S
