CFLAGS=-m32
ASFLAGS=-m32

assembly_test: func.o main.o
	$(CC) $(CFLAGS) -o $@ $^

func.o: func.asm
	nasm -f elf32 func.asm -o func.o

clean:
	$(RM) assembly_test *.o

