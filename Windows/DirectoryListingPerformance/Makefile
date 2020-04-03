# nmake Makefile

all: FindFile.exe

.c.obj:
	cl $*.c

FindFile.exe: FindFile.c
	cl /TC /WX /O2 /favor:AMD64 /Og /arch:AVX512 FindFile.c

clean:
	del FindFile.obj FindFile.exe


