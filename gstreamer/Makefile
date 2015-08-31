PACKAGES=glib-2.0 gstreamer-1.0
CFLAGS=$(shell pkg-config --cflags $(PACKAGES))
LDFLAGS=$(shell pkg-config --libs $(PACKAGES))
TARGETS=test1 test2 test3 test4 test5 test6

all: $(TARGETS)
test1: test1.o
test2: test2.o
test3: test3.o
test4: test4.o
test5: test5.o
test6: test6.o

clean:
	$(RM) *.o $(TARGETS)
