# nmake Makefile

opts=/WX /Ot /O2 /favor:AMD64 /arch:AVX512
cc=cl $(opts)

all: FindFile.exe

FindFile.exe: FindFile.c
	$(cc) FindFile.c

clean:
	del FindFile.obj FindFile.exe


