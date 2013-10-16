#include <stdio.h>
#include <time.h>

int main(int argc, char const** argv) {
    time_t t = 0;
    struct tm tm_buf;
    struct tm* tm = localtime_r(&t, &tm_buf);
    printf("%p\n", tm);
    tm = localtime_r(&t, &tm_buf);
    printf("%p\n", tm);
    tm = localtime_r(&t, &tm_buf);
    printf("%p\n", tm);
    tm = localtime_r(&t, &tm_buf);
    printf("%p\n", tm);
    return 0;
}

