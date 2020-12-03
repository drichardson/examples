// Example C program without linking to a C library.
// However, nolibc-syscall.S and nolibc-start.S are taken from
// glibc 2.20.
// This program only works on x86_64 Linux.


#include "/usr/include/asm/unistd_64.h"

extern long syscall(long syscall_number, long arg1, long arg2, long arg3, long arg4, long arg5, long arg6);

void exit(int rc) {
    syscall(__NR_exit, rc, 0, 0, 0, 0, 0);
    while(1); // never hit
}

long write(int fd, void const* data, int len) {
    return syscall(__NR_write, fd, (long)data, len, 0, 0, 0);
}

long strlen(char const* s) {
    long i;
    for(i = 0; s[i]; ++i);
    return i;
}

long put(char const* s) {
    long i = 0;
    long len = strlen(s);
    while (i < len) {
        long bw = write(1, s+i, len-i);
        if (bw == -1) return -1;
        i += bw;
    }
    return i;
}

void put_ulong(unsigned long l) {
    char buf[40];
    int i = (sizeof(buf)/sizeof(buf[0])) - 1;
    buf[i--] = 0;

    do {
        buf[i--] = '0' + (l % 10);
        l /= 10;
    } while(l);

    put(buf + i + 1);
}

void put_long(long l) {
    unsigned long ul;
    if (l < 0) {
        put("-");
        ul = l * -1;
    } else {
        ul = l;
    }
    put_ulong(ul);
}

static inline char nibble_to_char(unsigned char n) {
    if (n < 10) {
        return '0' + n;
    } else {
        return 'a' + n - 10;
    }
}

void put_hex(void const* p, unsigned long plen) {
    unsigned char const* u = (unsigned char const*)p;
    unsigned char const* uend = u+plen;
    char buf[3];
    buf[2] = 0;
    for(; u < uend; ++u) {
        buf[0] = nibble_to_char(*u >> 4);
        buf[1] = nibble_to_char(*u & 0x0f);
        put(buf);
    }
}

void __libc_start_main (int (*main) (int, char **),
        int argc, char **argv,
        void (*init) (void), void (*fini) (void),
        void (*rtld_fini) (void), void *stack_end) {
    exit(main(argc, argv));
}

// nolibc-start.S wants to set these.
void* __libc_csu_fini; 
void* __libc_csu_init;

int main(int argc, char const* const* argv) {
    put("argc = "); put_long(argc); put("\n");
    for(int i = 0; i < argc; ++i) {
        put("argv["); put_long(i); put("]="); put(argv[i]); put("\n");
    }

    put("__libc_csu_fini: "); put_hex(&__libc_csu_fini, sizeof(__libc_csu_fini)); put("\n");
    put("__libc_csu_init: "); put_hex(&__libc_csu_init, sizeof(__libc_csu_init)); put("\n");

    return 0;
}

