#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <stdlib.h>

int main()
{
    puts("parent: main");

    pid_t pid = fork();
    if (pid == 0) {
        // child
        const int to_sleep = 100;

#if 0
        exit(1);
#endif

#if 0
        puts("child: sleeping");
        sleep(to_sleep);
        puts("child: exec");
        execl("/usr/bin/ls", "/usr/bin/ls", NULL);
#endif

        puts("child: sleeping");
        sleep(to_sleep);
        exit(0);
    }

    if (pid == (pid_t)-1) {
        printf("parent: fork failed. %s\n", strerror(errno));
        exit(1);
    }

    printf("parent: parent pid is %d, child pid is %d\n", getpid(), pid);
    puts("parent: press return to waitpid");
    getchar();

    // parent
    pid_t result = waitpid(pid, NULL, 0);

    if (result == (pid_t)-1) {
        printf("waitpid failed with %s\n", strerror(errno));
        exit(1);
    }

    puts("waitpid succeeded");

    return 0;
}

