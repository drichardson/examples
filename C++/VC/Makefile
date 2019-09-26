all: hello.exe intrinsics.exe

clean:
	del *.obj *.exe

.cc.obj:
	cl /c /EHsc $*.cc

hello.exe: hello.obj
	link -out:hello.exe hello.obj

intrinsics.exe: intrinsics.obj
	link -out:intrinsics.exe intrinsics.obj

