all: bignum random

bignum: bignum.o
	$(CC) -lcrypto -o $@ $<

random: random.o
	$(CC) -lcrypto -o $@ $<

clean:
	$(RM) *.o random bignum
