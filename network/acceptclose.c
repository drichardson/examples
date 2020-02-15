// ******************************************************************
// A server that accepts a connection and then immediately closes it.
// ******************************************************************

#include <arpa/inet.h>
#include <errno.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

int main(int argc, char** argv)
{
    if (argc != 3) {
        fprintf(stderr,
                "Expected 2 args but got %d.\n"
                "Usage: acceptclose PORT REUSE\n"
                "   REUSE can be 0 (do not reuse address) or 1 (reuse address)\n",
                argc-1);
        exit(1);
    }


    char* endptr = NULL;
    long long int portLong = strtol(argv[1], &endptr, 10);
    if (*endptr != 0) {
        fprintf(stderr, "Cruft at end of port string: %s\n", endptr);
        exit(1);
    }
    if (portLong > 65535 || portLong < 0) {
        fprintf(stderr, "Port %lld out of range.\n", portLong);
        exit(1);
    }
    
    unsigned short port = (unsigned short)portLong;

    const char* reuseOpt = argv[2];

    struct sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = INADDR_ANY;

    int server = socket(AF_INET, SOCK_STREAM, 0);
    if (server == -1) {
        fprintf(stderr, "socket failed for server with %d: %s\n", errno, strerror(errno));
        exit(1);
    }

    if (reuseOpt[0] == '1') {
        printf("Setting SO_REUSEADDR\n");
        int optval = 1;
        if (setsockopt(server, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval)) == -1) {
            fprintf(stderr, "setsockopt(SO_REUSEADDR) failed. %d: %s\n", errno, strerror(errno));
            exit(1);
        }

        optval = 1;
        if (setsockopt(server, SOL_SOCKET, SO_REUSEPORT, &optval, sizeof(optval)) == -1) {
            fprintf(stderr, "setsockopt(SO_REUSEPORT) failed. %d: %s\n", errno, strerror(errno));
            exit(1);
        }
    }

    printf("Binding on port %d\n", port);
    if (bind(server, (struct sockaddr*)&addr, sizeof(addr)) == -1) {
        fprintf(stderr, "bind failed: %d: %s\n", errno, strerror(errno));
        exit(1);
    }

    if (listen(server, 5) == -1) {
        fprintf(stderr, "listen failed: %d: %s\n", errno, strerror(errno));
        exit(1);
    }

    printf("Listening on port %d...\n", port);

    while(1) {
        memset(&addr, 0, sizeof(addr));
        socklen_t addrlen = sizeof(addr);
        int conn = accept(server, (struct sockaddr*)&addr, &addrlen);
        if (conn == -1) {
            fprintf(stderr, "accept failed: %d: %s\n", errno, strerror(errno));
            continue;
        }

        char buf[100];
        const char* displayString = inet_ntop(addr.sin_family, &addr.sin_addr, buf, sizeof(buf));
        buf[sizeof(buf)-1] = 0; // make sure 0 terminated
        if (displayString) {
            printf("Accepted connection from %s:%d\n", displayString, ntohs(addr.sin_port));
        } else {
            fprintf(stderr, "inet_ntop failed: %d: %s\n", errno, strerror(errno));
        }

        close(conn);

    }

    return 0;
}
