TARGETS=\
		access_specifiers \
		array \
		atomics \
		auto_decltype \
		bigsort \
		casting  \
		chrono \
		condition \
		crash \
		ctor \
		destructors \
		function \
		function.S \
		graph \
		hash \
		heap_main \
		ifstream \
		initialization \
		iterator \
		literals \
		max_subarray \
		ofstream_test \
		operators \
		placement_new \
		playground \
		pointers \
		power \
		prime_factorization \
		primes \
		priority_q \
		rod_cutting references \
		scoped_enums \
		sort_main \
		stack_protector \
		system_error \
		template_convertible \
		template_meta_programming \
		template_specialization \
		templates \
		typeid \
		vector


//CXX=clang++
OPTIMIZATION=-O3
DEBUG=-g
CXX=g++
CXXFLAGS=-std=c++17 -Wall -Werror ${DEBUG} ${OPTIMIZATION} -march=native -pthread -fstack-protector-all -I/usr/local/include

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
