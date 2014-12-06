TARGETS=gpio_userspace cross_compile_hello_world hy_srf05_ultrasonic_ranging_sensor 
RPI_SSH_DEST=doug@192.168.1.16:/home/doug
#RPI_SSH_DEST=pi@192.168.1.34:/home/pi

OPTIMIZATION=-O3
DEBUG=
CXXFLAGS=-Wall ${DEBUG} ${OPTIMIZATION} -pthread
CXXFLAGS+=-std=c++1y

# CROSS_COMPILE is the prefix of your cross compilation
# tools. You can get one for ArchLinuxARM here:
# http://archlinuxarm.org/developers/distcc-cross-compiling
# or create your own using crosstool-ng
CROSS_COMPILE?=~/x-tools6h/arm-unknown-linux-gnueabihf/bin/
CXX=$(CROSS_COMPILE)g++

LINK.o=$(LINK.cc) # Use c++ link rule by default instead of c link rule

rsync: all
	rsync $(TARGETS) gpio_script.sh $(RPI_SSH_DEST)

all: $(TARGETS)

gpio_userspace: pins.o file_descriptor.o

hy_srf05_ultrasonic_ranging_sensor: pins.o file_descriptor.o

clean:
	$(RM) -r $(TARGETS) *.o
