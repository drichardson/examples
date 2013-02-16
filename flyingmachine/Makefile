
all: main.hex

main.o: main.c
	avr-gcc -c -g -O3 -Wall -mmcu=atmega8535 main.c -o main.o

main.elf: main.o
	avr-gcc main.o -mmcu=atmega8535 -o main.elf

main.hex: main.elf
	avr-objcopy -O ihex main.elf main.hex

install:
	avrdude -p atmega8535 -c stk500v2 -P /dev/ttyavr -U flash:w:main.hex:i

clean:
	rm -f *.o *.hex *.elf
