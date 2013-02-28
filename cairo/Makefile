TARGETS=rendertext
OBJS=rendertext.o
CFLAGS=-Wall -Werror -I/opt/local/include
LDFLAGS=-L/opt/local/lib -lcairo

rendertext: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS)

clean:
	$(RM) $(TARGETS) $(OBJS)