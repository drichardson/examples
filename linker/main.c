#include <dlfcn.h>
#include <stdio.h>

extern void foo(void);

int main(int arg, char**argv) {
    foo();
    printf("foo is %p\n", foo);

    void *h = dlopen("./foo.so", RTLD_NOW);
    foo();
    printf("h = %p\n", h);
    printf("foo is %p\n", foo);

    void (*s)(void) = dlsym(h, "foo");
    printf("s is %p\n", s);
    s();
}

