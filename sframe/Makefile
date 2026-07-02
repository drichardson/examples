CC=gcc
CFLAGS=-Wa,--gsframe

foo: foo.o bar.o

objdump-demo: foo
	echo bar.o objdump
	objdump --sframe bar.o
	echo
	echo foo.o objdump
	objdump --sframe foo.o
	echo
	echo foo objdump
	objdump --sframe foo

readelf-demo: foo
	readelf --sframe foo

clean:
	$(RM) *.o foo
