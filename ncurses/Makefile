CC=clang
CFLAGS=-Wall -Werror -g

hellocurses: hellocurses.o
	$(CC) -l ncurses $< -o $@

clean:
	$(RM) -f hellocurses hellocurses.o
