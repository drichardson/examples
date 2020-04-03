# nmake Makefile

all: FindFile.exe

FindFile.exe: FindFile.c
	cl /Zi /TC /WX /O2 /favor:AMD64 /Og /arch:AVX512 FindFile.c

clean:
	del FindFile.obj FindFile.exe


