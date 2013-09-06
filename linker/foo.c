#include <stdio.h>

static int x;

void foo(void) {
    printf("foo static x %p %d, address of foo self = %p\n", &x, x, foo); 
    x+=1;
}

