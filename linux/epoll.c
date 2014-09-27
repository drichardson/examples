// epoll example: open two fifos and wait on them
// for more data.
// Before running this program run something like this in terminal x:
// $ mkfifo /tmp/f1; cat > /tmp/f1
// and run something like this in terminal y:
// $ mkfifo /tmp/f2; cat > /tmp/f2
// and then invoke this program
// $ ./epoll /tmp/f1 /tmp/f2
// and then, while this program is running, write data
// to f1 and f2. The cat program above will flush the data
// to the fifo whe it receives a newline.

#include <fcntl.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/epoll.h>
#include <sys/stat.h>
#include <unistd.h>

int main(int argc, char const* const* argv) {
    if (argc != 3) {
        fprintf(stderr, "Missing argument.\n"
                "Usage: epoll <file1> <file2>\n");
        exit(1);
    }

    char const* file1 = argv[1];
    int fd1 = open(file1, O_RDONLY | O_NONBLOCK | O_CLOEXEC);
    if (fd1 == -1) {
        perror("open fd1 failed.");
        exit(1);
    } 

    char const* file2 = argv[2];
    int fd2 = open(file2, O_RDONLY | O_NONBLOCK | O_CLOEXEC);
    if (fd2 == -1) {
        perror("open fd2 failed.");
        exit(1);
    }

    int epoll_fd = epoll_create1(EPOLL_CLOEXEC);
    if (epoll_fd == -1) {
        perror("epoll_create1 failed");
        exit(1);
    }

    struct epoll_event ev;
    ev.events = EPOLLIN;
    ev.data.fd = fd1;

    int rc = epoll_ctl(epoll_fd, EPOLL_CTL_ADD, fd1, &ev);
    if (rc == -1) {
        perror("epoll_ctl 1 failed");
        exit(1);
    }

    ev.events = EPOLLIN;
    ev.data.fd = fd2;
    rc = epoll_ctl(epoll_fd, EPOLL_CTL_ADD, fd2, &ev);
    if (rc == -1) {
        perror("epoll_ctl 2 failed");
        exit(1);
    }

    while(1) {
        struct epoll_event events[10];
        puts("epoll_wait");
        int const event_count = epoll_wait(epoll_fd, events, sizeof(events)/sizeof(events[0]), 5000 /*ms*/);
        if (event_count== -1) {
            perror("epoll_wait failed");
            exit(1);
        }

        puts("return");
        if (event_count== 0) {
            printf("timeout\n");
            continue;
        }

        printf("Got %d events.\n", rc);

        for(int i = 0; i < event_count; ++i) {
            printf("Event %d: ptr=%p fd=%d u32=%" PRIu32 " u64=%" PRIu64 "\n",
                    i, events[i].data.ptr, events[i].data.fd, events[i].data.u32, events[i].data.u64);

            // read some data.
            char buf[500];
            ssize_t len = read(events[i].data.fd, buf, sizeof(buf));
            printf("read %zd bytes\n", len);
        }
    }

    return 0;
}

