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
                "Expected 2 arguments, but got %d.\n"
                "Usage: keepopen HOST PORT\n",
                argc-1);
        exit(1);
    }

    const char* hostname = argv[1];
    const char* port = argv[2];

    //
    // Connect client to example.com
    //

    struct addrinfo hints;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;

    struct addrinfo *result = NULL;

    int rc = getaddrinfo(hostname, port, &hints, &result);
    if (rc != 0) {
        int saved_errno = errno;
        fprintf(stderr, "getaddrinfo failed for client with %d: %s\n", rc, gai_strerror(rc));
        if (rc == EAI_SYSTEM) {
            fprintf(stderr, "   underlying system error: %d: %s\n", saved_errno, strerror(saved_errno));
        }
        exit(1);
    }

    if (result == NULL) {
        fprintf(stderr, "No addresses found for example.com\n");
        exit(1);
    }

    int client = socket(AF_INET, SOCK_STREAM, 0);
    if (client == -1) {
        fprintf(stderr, "socket failed for client with %d: %s\n", errno, strerror(errno));
        exit(1);
    }

    // just connect to the first address returned for this test
    if (connect(client, result->ai_addr, result->ai_addrlen) == -1)
    {
        fprintf(stderr, "failed to connect to address. %d: %s\n", errno, strerror(errno));
        exit(1);
    }

    freeaddrinfo(result);
    result = NULL;

    printf("Connected to %s:%s...\n", hostname, port);

    //
    // Now look up the socket name to find out the TCP port we are using on the client side.
    //
    struct sockaddr_storage storage;
    socklen_t storage_len = sizeof(storage);
    if (getsockname(client, (struct sockaddr*)&storage, &storage_len) == -1) {
        fprintf(stderr, "getsockname failed %d: %s\n", errno, strerror(errno));
        exit(1);
    }
    if (storage.ss_family != AF_INET) {
        fprintf(stderr, "expected AF_INET(IPv4)... not sure what's going on here\n");
        exit(1);
    }
    struct sockaddr_in* addr = (struct sockaddr_in*)&storage;
    in_port_t client_port = ntohs(addr->sin_port);

    printf("Client (our side) was allocated port %d\n", client_port);

    printf("Sleeping to keep port open\n");
    sleep(9999999);

    return 0;
}
