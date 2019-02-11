TARGETS=ofstream_test templates atomics system_error array \
		function function.S condition priority_q chrono \
		crash playground typeid casting placement_new \
		initialization ctor auto_decltype operators \
		literals template_meta_programming sort_main \
		heap_main bigsort ifstream vector graph destructors \
		stack_protector autoptr hash primes max_subarray \
		prime_factorization rod_cutting references \
		template_specialization scoped_enums template_convertible \
		pointers power iterator


//CXX=clang++
OPTIMIZATION=-O3
DEBUG=-g
CXX=g++
CXXFLAGS=-std=c++14 -Wall ${DEBUG} ${OPTIMIZATION} -march=native -pthread -fstack-protector-all -I/usr/local/include

LINK.o=$(LINK.cc) # Use c++ link rule by default instead of c link rule

all: $(TARGETS)

analyze: clean
	scan-build make

glog: glog.cc
	$(CXX) -g -O0 -o glog glog.cc -lglog

function.S: function.cc
	$(CXX) -S $(CXXFLAGS) -o $@ $^

clean:
	$(RM) -r $(TARGETS) *.dSYM a.txt *.o *.S
