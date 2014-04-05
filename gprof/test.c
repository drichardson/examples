#include <stdio.h>
#include <unistd.h>

static void foo(int x)
{
    if (x > 0) {
        puts("x > 0");
    } else {
        puts("x <= 0");
    }
}

static void bar(int x_low, int x_high)
{
    for(int i = x_low; i < x_high; ++i) {
        if (i % 2000 == 0) sleep(1);
        if (i % 8 == 0) {
            puts("evenly divisible by 8");
        } else {
            foo(i);
        }
    }
}

int main(int argc, char** argv)
{
    foo(1);
    bar(1, 5);
    bar(1000, 5000);
    return 0;
}

