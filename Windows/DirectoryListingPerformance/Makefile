# nmake Makefile

all: FindFile.exe

.c.obj:
	cl $*.c

FindFile.exe: FindFile.obj
	link /OUT:FindFile.exe FindFile.obj

clean:
	echo yes


