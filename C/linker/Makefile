
CC=/Users/doug/android-ndk-r8-clang/bin/clang

all: foo.so foo

foo.so: foo.c
	$(CC) -o foo.so -shared -fpic foo.c -Wl,--export-dynamic

foo: foo.c main.c
	$(CC) foo.c main.c -o foo -Wl,--export-dynamic

clean:
	$(RM) foo foo.so

