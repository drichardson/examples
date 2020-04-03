# nmake Makefile

defines=\
		/DNTDDI_VERSION=NTDDI_WIN10 \
		/DUNICODE \
		/DWIN32_LEAN_AND_MEAN \
		/DWINVER=_WIN32_WINNT_WIN10 \
		/D_UNICODE

opts=\
	 /O2 \
	 /Ot \
	 /WX \
	 /arch:AVX512 \
	 /favor:AMD64

cc=cl $(defines) $(opts)

all: FindFile.exe NtQueryDirectoryFile.exe

FindFile.exe: FindFile.c
	$(cc) FindFile.c

NtQueryDirectoryFile.exe: NtQueryDirectoryFile.c
	$(cc) NtQueryDirectoryFile.c

clean:
	del *.obj *.exe


