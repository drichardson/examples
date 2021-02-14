TARGETS=\
	access_specifiers \
	array \
	atomics \
	auto_decltype \
	bigsort \
	casting  \
	chrono \
	condition \
	const \
	copying \
	crash \
	ctor \
	ctor-init-list \
	destructors \
	function \
	function.S \
	graph \
	hash \
	heap_main \
	ifstream \
	inheritance \
	initialization \
	iterator \
	literals \
	max_subarray \
	namespaces \
	ofstream_test \
	operators \
	override \
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
	strings \
	system_error \
	template_convertible \
	template_curiously_recurring_template_pattern \
	template_forward_decl \
	template_meta_programming \
	template_specialization \
	templates \
	typeid \
	vector \
	virtual


//CXX=clang++
OPTIMIZATION=-O3
DEBUG=-g
CXX=g++
CXXFLAGS=-std=c++17 -Wall ${DEBUG} ${OPTIMIZATION} -march=native -pthread -fstack-protector-all 

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
