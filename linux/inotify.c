// inotify example. Monitor 2 directories for files that are closed
// after being opened for writing.
#include <stdio.h>
#include <sys/inotify.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>

int main(int argc, char const* const* argv) {
    if (argc != 3) {
        fprintf(stderr, "Usage: inotify <dir1> <dir2>\n");
        exit(1);
    }
    char const* dir1 = argv[1];
    char const* dir2 = argv[2];

    int fd = inotify_init1(IN_CLOEXEC);
    if (fd == -1) {
        perror("inotify_init1 failed");
        exit(1);
    }

    int wd1 = inotify_add_watch(fd, dir1, IN_CLOSE_WRITE);
    if (wd1 == -1) {
        perror("inotify_add_watch 1 failed");
        exit(1);
    }
    int wd2 = inotify_add_watch(fd, dir2, IN_CLOSE_WRITE);
    if (wd2 == -1) {
        perror("inotify_add_watch 2 failed");
        exit(1);
    }


    // declaration of buf from man (2) inotify
    char buf[4096]
        __attribute__ ((aligned(__alignof__(struct inotify_event))));

    while(1) {
        ssize_t len = read(fd, buf, sizeof(buf));
        if (len < 0) {
            perror("read failed.");
            if (errno != EAGAIN && errno != EINTR) {
                exit(1);
            }
        }

        struct inotify_event const* event = NULL;

        for(char *ptr = buf; ptr < buf + len; ptr += sizeof(struct inotify_event) + event->len) {
            event = (struct inotify_event*)ptr;

            if (event->wd == wd1) {
                printf("Event on %s. ", dir1);
            } else if(event->wd == wd2) {
                printf("Event on %s. ", dir2);
            } else {
                fprintf(stderr, "Unexpected wd %d\n", event->wd);
                exit(1);
            }

            if (event->mask & IN_CLOSE_WRITE) {
                printf("IN_CLOSE_WRITE ");
            } else if (event->mask & IN_DELETE_SELF) {
                printf("IN_DELETE_SELF ");
            } else if (event->mask & IN_MOVE_SELF) {
                printf("IN_MOVE_SELF ");
            } else if (event->mask & IN_IGNORED) {
                printf("IN_IGNORED ");
            } else {
                printf("Unexpected event mask: 0x%X\n", event->mask);
            }

            if (event->len) {
                printf("%s ", event->name);
            }

            if (event->mask & IN_ISDIR) {
                printf("[dir] ");
            } else {
                printf("[file] ");
            }

            putchar('\n');
        }
    }

    return 0;
}
