#include <curses.h>
#include <locale.h>

int main(int argc, char** argv)
{
    initscr();
    printw("Hello World!!!");
    refresh();
    getch();
    endwin();
    return 0;
}

