LDFLAGS=-L/usr/local/opt/jpeg-turbo/lib
CPPFLAGS=-I/usr/local/opt/jpeg-turbo/include
CFLAGS=-g -O0

TARGETS=test1

all: $(TARGETS)

test1: test1.o
	$(CC) $^ -o $@ $(LDFLAGS) -lturbojpeg

clean:
	$(RM) $(TARGETS) *.o


