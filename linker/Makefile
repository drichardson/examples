all: foo.so foo

foo.so: foo.c
	$(CC) -o foo.so -shared -fpic foo.c

foo: foo.c main.c
	$(CC) foo.c main.c -o foo

clean:
	$(RM) foo foo.so

