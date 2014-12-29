# To add a target:
# 1. Add all source files to SRCS
# 2. Create a target rule of the form <target>: <dep1.o> <dep2.o> ... <depN.o>
# 3. Add the target to TARGETS

# All sources need to be listed here for header dependencies to
# be generated.
SRCS=list_driver.cc vector_driver.cc vector_resize_stress.cc \
	 queue_driver.cc stack_driver.cc # circular_buffer_driver.cc

TARGETS=list_driver vector_driver vector_resize_stress \
		queue_driver stack_driver # circular_buffer_driver

# Default all target comes first.
all: $(TARGETS)

CXXFLAGS=-std=c++03 -Wall -O0 -g -march=native -pthread -fstack-protector-all

# Use C++ compiler to link
LINK.o=$(LINK.cc)

# Pattern rule for C++ files (.cc) produces header dependencies
# and object file.
%.o : %.cc
	$(CXX) $(CXXFLAGS) -MM -MF $(patsubst %.o,%.P,$@) $<
	$(CXX) $(CXXFLAGS) -c -o $@ $<

# Include C++ header dependencies, if they exist.
-include $(SRCS:.cc=.P)

# Target object file dependencies
list_driver: list_driver.o
vector_driver: vector_driver.o
vector_resize_stress: vector_resize_stress.o
queue_driver: queue_driver.o
stack_driver: stack_driver.o
#circular_buffer_driver: circular_buffer_driver.o

clean:
	$(RM) $(SRCS:.cc=.o) $(SRCS:.cc=.P) $(TARGETS)
