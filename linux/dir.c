#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/syscall.h>
#include <sys/stat.h>
#include <unistd.h>

static void posix_list_directory(char const* dirpath) {
    DIR* d = opendir(dirpath);
    if (d == NULL) {
        perror("Couldn't open directory.");
        return;
    }
    int name_max = pathconf(dirpath, _PC_NAME_MAX);
    errno = 0; // clear, so we can tell if pathconf == -1 means unlimited or erro
    if (name_max == -1) {
        if (errno) {
            perror("pathconf failed");
        } else {
            // no limit. This is kind of weird for a file system, so print a warning message.
            name_max = 4000;
            fprintf(stderr, "WARNING: file system says it has no limit on \
                    names. I need a limit so using %d\n", name_max); }
    }
    int len = offsetof(struct dirent, d_name) + name_max + 1;
    struct dirent* entbuf, *ent;
    entbuf = malloc(len);

    int i = 0, rc;
start:
    rc = readdir_r(d, entbuf, &ent);
    if (rc != 0) {
        fprintf(stderr, "readdir_r returned error %d: %s\n", rc, strerror(rc));
        goto end;
    }
    if(ent) {
        printf("%d: name=%s, inode=%" PRIu64 "\n", i, ent->d_name, ent->d_ino);
        goto start;
    }
end:

    free(entbuf);
    closedir(d);
}

struct linux_dirent {
    unsigned long  d_ino;     /* Inode number */
    unsigned long  d_off;     /* Offset to next linux_dirent */
    unsigned short d_reclen;  /* Length of this linux_dirent */
    char           d_name[];  /* Filename (null-terminated) */
    /* length is actually (d_reclen - 2 -
       offsetof(struct linux_dirent, d_name)) */
    /*
       char           pad;       // Zero padding byte
       char           d_type;    // File type (only since Linux
    // 2.6.4); offset is (d_reclen - 1)
    */

};

static int getdents_wrapper(unsigned int fd, struct linux_dirent *dirp,
        unsigned int count) {
    return syscall(SYS_getdents, fd, dirp, count);
}

static void linux_list_directory(char const* dir) {
    int fd = open(dir, O_RDONLY);
    if (fd == -1) {
        perror("Couldn't open directory.");
        return;
    }
    int len = 0x10000;
    struct linux_dirent* base = malloc(len);
    int rc;
    int count = 0;
    while((rc = getdents_wrapper(fd, base, len)) > 0) {
        int byte = 0;
        struct linux_dirent* p = base;
        while(byte < rc) {
            printf("%d: name=%s, inode=%lu\n", count, p->d_name, p->d_ino);
            ++count;
            byte += p->d_reclen;
            p = (struct linux_dirent*)(((char*)base) + byte);
        }
    }
    if (rc == -1) {
        perror("getdents_wrapper failed");
    }
    free(base);
    close(fd);
}

int main(int const argc, char const* const* argv) {

    if (argc != 2) {
        fprintf(stderr, "Usage: dir <dir>\n");
        exit(1);
    }

    // POSIX readdir
    puts("POSIX VERSION");
    posix_list_directory(argv[1]);

    puts("LINUX SYSCALL VERSION");
    linux_list_directory(argv[1]);

    return 0;
}

