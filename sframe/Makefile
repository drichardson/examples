CC=gcc
CFLAGS=-Wa,--gsframe

foo: foo.o bar.o

show: foo
	echo bar.o objdump
	objdump --sframe bar.o
	echo
	echo foo.o objdump
	objdump --sframe foo.o
	echo
	echo foo objdump
	objdump --sframe foo


clean:
	$(RM) *.o foo
