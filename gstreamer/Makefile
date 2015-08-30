PACKAGES=glib-2.0 gstreamer-1.0
CFLAGS=$(shell pkg-config --cflags $(PACKAGES))
LDFLAGS=$(shell pkg-config --libs $(PACKAGES))

test1: test1.o

clean:
	$(RM) test1.o test1
