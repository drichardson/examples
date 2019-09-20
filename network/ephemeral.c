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

    struct addrinfo hints;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;

    struct addrinfo *result = NULL;

    int rc = getaddrinfo("example.com", "80", &hints, &result);
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

    printf("Connected to example.com...\n");

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

    printf("Client was allocated port %d\n", client_port);

    //
    // Now try to listen on the same port that was just allocated to a client.
    //
    int server = socket(AF_INET, SOCK_STREAM, 0);
    if (server == -1) {
        fprintf(stderr, "socket failed for server with %d: %s\n", errno, strerror(errno));
        exit(1);
    }

    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(client_port);
    server_addr.sin_addr.s_addr = addr->sin_addr.s_addr; // listen only on the same address the client connect to example.com from
    if (bind(server, (struct sockaddr*)&server_addr, sizeof(server_addr)) == -1) {
        fprintf(stderr, "bind failed for server. %d: %s\n", errno, strerror(errno));
        exit(1);
    }

    if (listen(server, 3) == -1) {
        fprintf(stderr, "listen failed for server. %d: %s\n", errno, strerror(errno));
        exit(1);
    }

    printf("Server able to bind to client port. This is most unexpected.\n");

    printf("Sleeping\n");
    sleep(300);
    printf("Done sleeping\n");

    return 0;
}
