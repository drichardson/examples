PACKAGES=glib-2.0 gstreamer-1.0
INCLUDES=$(shell pkg-config --cflags $(PACKAGES))
CFLAGS=$(INCLUDES) -g -Wall
LDFLAGS=$(shell pkg-config --libs $(PACKAGES))
TARGETS=test1 test2 test3 test4 test5 test6 test7 test8 hello

all: $(TARGETS) 
test1: test1.o
test2: test2.o
test3: test3.o
test4: test4.o
test5: test5.o
test6: test6.o
test7: test7.o
test8: test8.o
hello: hello.o

.clang_complete:
	echo $(INCLUDES) > $@


clean:
	$(RM) *.o $(TARGETS)
