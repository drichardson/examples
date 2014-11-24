TARGETS=gpio_userspace cross_compile_hello_world
RPI_SSH_DEST=doug@192.168.1.31:/home/doug

# XTOOLSDIR is where you've extracted your cross compilation
# environment. You can get one for ArchLinuxARM here:
# http://archlinuxarm.org/developers/distcc-cross-compiling
# or create your own using crosstool-ng
XTOOLS_BIN=~/x-tools6h/arm-unknown-linux-gnueabihf/bin
OPTIMIZATION=-O3
DEBUG=
CXX=$(XTOOLS_BIN)/g++
CXXFLAGS=-Wall ${DEBUG} ${OPTIMIZATION} -pthread
CXXFLAGS+=-std=c++1y

LINK.o=$(LINK.cc) # Use c++ link rule by default instead of c link rule

rsync: all
	rsync $(TARGETS) gpio_script.sh $(RPI_SSH_DEST)

all: $(TARGETS)

clean:
	$(RM) -r $(TARGETS) *.o
