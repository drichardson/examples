#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/capability.h>
#include <string.h>
#include <unistd.h>

static void print_caps(cap_user_data_t d)
{
    printf("capget: effective: %X, inheritable: %X, permitted: %X\n",
            d->effective, d->inheritable, d->permitted);
}

int main()
{
    struct __user_cap_header_struct h = {0};
    struct __user_cap_data_struct d = {0};

    h.version = _LINUX_CAPABILITY_VERSION_3;
    h.pid = 0; // 0 is equivalent to getpid()

    puts("getting caps. Run as root or use setcap to set capabilities on this executable for different results.");
    int rc = capget(&h, &d);
    if (rc  == -1) {
        printf("capget failed. %s.\n", strerror(errno));
        exit(1);
    }

    print_caps(&d);

    //puts("adding chown");
    //d.effective |= CAP_TO_MASK(CAP_CHOWN);
    d.inheritable |= CAP_TO_MASK(CAP_FOWNER);
    print_caps(&d);
    rc = capset(&h, &d);
    if (rc == -1) {
        printf("capset failed: %s.\n", strerror(errno));
        exit(1);
    }


    puts("getting caps again");

    return 0;
}

