CFLAGS=-g -O0 -Wall

TARGETS=ephemeral \
		keepopen \
		acceptclose \
		prefork

all: $(TARGETS)

ephemeral: ephemeral.c

keepopen: keepopen.c

acceptclose: acceptclose.c

prefork: prefork.c

clean:
	$(RM) $(TARGETS)
