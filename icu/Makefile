CC=clang
LD=clang

INCLUDES=-I/opt/local/include
CFLAGS=-Wall -Werror $(INCLUDES)
LDFLAGS=-L/opt/local/lib -licui18n -licuuc -licudata

listLocales: listLocales.o
	$(LD) $(LDFLAGS) -o $@ $< 

clean:
	$(RM) listLocales.o listlocales