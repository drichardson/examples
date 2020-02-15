// ****************************************************************
// Demonstrates the address in use error that occurs when a server
// attempts to bind to an ephemeral port already allocated to a
// client.
//
// In the real world, this might show up as some process making
// a TCP connect to a server, then being assigned ephemeral port Y,
// and then while that port is still allocated a server attempts
// to bind to the same port Y.
//
// A server might attempt to do this if it has hard coded its
// port to a number in the ephemeral port range.
//
// https://en.wikipedia.org/wiki/Ephemeral_port
// ****************************************************************

#include <arpa/inet.h>
#include <errno.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

int main()
{
    //
    // Connect client to example.com
    //


    struct sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(8787);
    addr.sin_addr.s_addr = INADDR_ANY;

    int server = socket(AF_INET, SOCK_STREAM, 0);
    if (server == -1) {
        fprintf(stderr, "socket failed for server with %d: %s\n", errno, strerror(errno));
        exit(1);
    }

    if (bind(server, (struct sockaddr*)&addr, sizeof(addr)) == -1) {
        fprintf(stderr, "bind failed: %d: %s\n", errno, strerror(errno));
        exit(1);
    }

    if (listen(server, 5) == -1) {
        fprintf(stderr, "listen failed: %d: %s\n", errno, strerror(errno));
        exit(1);
    }

    printf("Listening on port %d...\n", ntohs(addr.sin_port));

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
