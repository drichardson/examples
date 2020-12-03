#define _GNU_SOURCE 1
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

FILE* fp_log = NULL;

/*
 *	Open the console with retries.
 */
static
int console_open(char const* console_dev, int mode)
{
	int f, fd = -1;
	int m;

	/*
	 *	Open device in nonblocking mode.
	 */
	m = mode | O_NONBLOCK;

	/*
	 *	Retry the open five times.
	 */
	for(f = 0; f < 5; f++) {
		if ((fd = open(console_dev, m)) >= 0) break;
        fprintf(fp_log, "Retrying because console_open failed to open dev %s\n", console_dev);
		usleep(10000);
	}

	if (fd < 0) return fd;

    fputs("Setting original flags\n", fp_log);
	/*
	 *	Set original flags.
	 */
	if (m != mode)
  		fcntl(fd, F_SETFL, mode);
    fprintf(fp_log, "returning fd = %d\n", fd);
	return fd;
}



/*
 *	Print to the system console
 */
void print(char const* dev, char *s)
{
	int fd;

	if ((fd = console_open(dev, O_WRONLY|O_NOCTTY|O_NDELAY)) >= 0) {
		int rc = write(fd, s, strlen(s));
		int rc2 = close(fd);
        fprintf(fp_log, "In print, write returned %d, close %d\n", rc, rc2);
	} else {
        fprintf(fp_log, "Error printing because failed to open dev %s\n", dev);
    }
}

int main(int argc, char** argv) {
    FILE *fp = fopen("/doug-test.txt", "w");
    fp_log = fp;
    while(1) {
        print("/dev/console", "DOUGINIT: console\n");
        print("/dev/tty1", "DOUGINIT: tty1\n");
        fprintf(stderr, "DOUGINIT: stderr\n");
        fflush(stderr);
        fprintf(stdout, "DOUGINIT: stdout\n");
        fflush(stdout);
        if (fp) {
            fputs("Flushing and syncing\n", fp);
            fflush(fp);
            syncfs(fileno(fp));
        }
        sleep(5);
    }
    return 0;
}
