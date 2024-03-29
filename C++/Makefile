# Most targets can be built with either g++ or clang++. However, some targets
# require "-arch x86_64" which requires clang++.

#CXX=g++
#CXX=clang++

OPTIMIZATION=-O3
DEBUG=-g
CXXFLAGS=-std=c++20 -Wall -Wextra ${DEBUG} ${OPTIMIZATION} -pthread -fstack-protector-all 

TARGETS=\
	access_specifiers \
	array \
	atomics \
	attributes \
	auto_decltype \
	bigsort \
	casting  \
	chrono \
	concepts \
	condition \
	const \
	copying \
	crash \
	ctor \
	ctor-init-list \
	destructors \
	graph \
	hash \
	heap_main \
	ifstream \
	inheritance \
	initialization \
	io \
	iterator \
	list \
	literals \
	max_subarray \
	namespaces \
	numeric_limits \
	ofstream_test \
	operators \
	override \
	permuation \
	placement_new \
	playground \
	pointers \
	power \
	prime_factorization \
	primes \
	priority_q \
	random \
	rod_cutting references \
	scoped_enums \
	sort_main \
	stack_protector \
	strings \
	switch \
	system_error \
	template_convertible \
	template_curiously_recurring_template_pattern \
	template_forward_decl \
	template_meta_programming \
	template_specialization \
	templates \
	typeid \
	unordered_map \
	vector \
	virtual

CLANG_REQUIRED_TARGETS=\
	function \
	function.S

ifeq ($(CXX),clang++)
	TARGETS += $(CLANG_REQUIRED_TARGETS)
endif

LINK.o=$(LINK.cc) # Use c++ link rule by default instead of c link rule

all: $(TARGETS)

analyze: clean
	scan-build make

glog: glog.cc
	$(CXX) -g -O0 -o glog glog.cc -lglog

function: function.cc
	$(CXX) $(CXXFLAGS) -arch x86_64 -o $@ $^

function.S: function.cc
	$(CXX) -S $(CXXFLAGS) -arch x86_64 -o $@ $^

clean:
	$(RM) -r $(TARGETS) *.dSYM a.txt *.o *.S
