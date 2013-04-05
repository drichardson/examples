CC=clang
CFLAGS=-Wall -Werror -g

all: hellocurses hellocurses2

hellocurses: hellocurses.o
	$(CC) -l ncurses $< -o $@

hellocurses2: hellocurses2.o
	$(CC) -l ncurses $< -o $@

clean:
	$(RM) -f hellocurses hellocurses.o hellocurses2 hellocurses2.o
