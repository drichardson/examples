#include <stdio.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

int main(int argc, char** argv) {
    if (argc < 3) {
        fprintf(stderr, "Usage: scatter <filename> len1 len2 ... lenN\n");
        exit(1);
    }

    ssize_t total_length = 0;
    char const* filename = argv[1];
    char ** len_strings = argv + 2;
    int num_iovecs = argc - 2;
    struct iovec* iovecs = malloc(sizeof(struct iovec) * num_iovecs);
    for(int i = 0; i < num_iovecs; ++i) {
        int len = atoi(len_strings[i]);
        total_length += len;
        iovecs[i].iov_base = malloc(len+1);
        ((char*)iovecs[i].iov_base)[len] = 0;
        iovecs[i].iov_len = len;
    }

    int fd = open(filename, O_RDONLY);
    if (fd == -1) {
        fprintf(stderr, "Error opening file. %d: %s\n", errno, strerror(errno));
        exit(1);
    }

    ssize_t bytes_read = readv(fd, iovecs, num_iovecs);
    if (bytes_read == -1) {
        fprintf(stderr, "Error reading file. %d: %s\n", errno, strerror(errno));
        exit(1);
    }

    close(fd);

    if (bytes_read != total_length) {
        fprintf(stderr, "Partially read bytes. Got %zd but expected %zd.\n", bytes_read, total_length);
    }

    for(int i = 0; i < num_iovecs; ++i) {
        printf("# %d: %s\n", i, iovecs[i].iov_base);
    }

    free(iovecs);

    return 0;
}

