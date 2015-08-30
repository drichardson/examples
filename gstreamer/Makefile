PACKAGES=glib-2.0 gstreamer-1.0
CFLAGS=$(shell pkg-config --cflags $(PACKAGES))
LDFLAGS=$(shell pkg-config --libs $(PACKAGES))
TARGETS=test1 test2

all: $(TARGETS)
test1: test1.o
test2: test2.o

clean:
	$(RM) *.o $(TARGETS)
