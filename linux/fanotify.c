// Monitors a mount point for changes using fanotify.
// Note: although you can specify any directory for monitoring,
// fanotify will actually monitor the entire mount point containing
// the directory. See inotify for a per-directory alternative.
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/fanotify.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

int main(int argc, char const* const* argv) {

    if (argc != 2) {
        fprintf(stderr, "Missing mount point to monitor.\n"
                "Usage: fanotify <mount_point_to_monitor>\n");
        exit(1);
    }

    char const* directory_to_monitor = argv[1];

    int fd = fanotify_init(FAN_CLASS_NOTIF | FAN_CLOEXEC, O_RDONLY | O_CLOEXEC);
    if (fd == -1) {
        perror("fanotify_init failed");
        exit(1);
    }

    int rc = fanotify_mark(fd,
            FAN_MARK_ADD | FAN_MARK_ONLYDIR | FAN_MARK_MOUNT,
            FAN_CLOSE_WRITE,
            AT_FDCWD,
            directory_to_monitor);

    if (rc == -1) {
        perror("fanotify_mark failed");
        exit(1);
    }

    while(1) {
        struct fanotify_event_metadata buf[200];
        ssize_t len = read(fd, &buf, sizeof(buf));
        if (len < 0) {
            perror("read failed");
            if (errno != EAGAIN && errno != EINTR) {
                exit(1);
            }
        }

        struct fanotify_event_metadata const* metadata = buf;
        for(; FAN_EVENT_OK(metadata, len); metadata = FAN_EVENT_NEXT(metadata, len))
        {
            if (metadata->vers != FANOTIFY_METADATA_VERSION) {
                fprintf(stderr, "Read metadata version %hhu != expected %d\n",
                        metadata->vers, FANOTIFY_METADATA_VERSION);
                // man page says to stop trying to use this fanotify file descriptor.
                exit(1);
            }

            if (metadata->fd == FAN_NOFD) {
                fprintf(stderr, "Queue overflow. Events dropped.\n");
            } else if (metadata->fd < 0) {
                fprintf(stderr, "Invalid metadata event file descriptor. %d\n", metadata->fd);
            } else {
                char proc_fd_link[100];
                snprintf(proc_fd_link, sizeof(proc_fd_link), "/proc/self/fd/%d", metadata->fd);
                char path[200];
                ssize_t rl_len = readlink(proc_fd_link, path, sizeof(path)-1);
                if (rl_len == -1) {
                    perror("readlink failed.");
                } else {
                    path[rl_len] = 0;
                    printf("%s was closed after being opened for writing.\n", path);
                }
                close(metadata->fd);
            }
        }
    }

    return 0;
}
