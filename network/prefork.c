// ******************************************************************
// A server that forks N children to handle requests.
// ******************************************************************

#include <arpa/inet.h>
#include <errno.h>
#include <netdb.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

void child_loop(int server, int childnum);
void parent_loop();

int main(int argc, char **argv)
{
	if (argc != 3)
	{
		fprintf(stderr,
			"Expected 2 args but got %d.\n"
			"Usage: acceptclose PORT REUSE\n"
			"   REUSE can be 0 (do not reuse address) or 1 (reuse "
			"address)\n",
			argc - 1);
		exit(1);
	}

	char *endptr = NULL;
	long long int portLong = strtol(argv[1], &endptr, 10);
	if (*endptr != 0)
	{
		fprintf(stderr, "Cruft at end of port string: %s\n", endptr);
		exit(1);
	}
	if (portLong > 65535 || portLong < 0)
	{
		fprintf(stderr, "Port %lld out of range.\n", portLong);
		exit(1);
	}

	unsigned short port = (unsigned short)portLong;

	const char *reuseOpt = argv[2];

	int server = socket(AF_INET, SOCK_STREAM, 0);
	if (server == -1)
	{
		fprintf(stderr,
			"socket failed for server with %d: %s\n",
			errno,
			strerror(errno));
		exit(1);
	}

	if (reuseOpt[0] == '1')
	{
		printf("Setting SO_REUSEADDR\n");
		int optval = 1;
		if (setsockopt(server,
			       SOL_SOCKET,
			       SO_REUSEADDR,
			       &optval,
			       sizeof(optval)) == -1)
		{
			fprintf(stderr,
				"setsockopt(SO_REUSEADDR) failed. %d: %s\n",
				errno,
				strerror(errno));
			exit(1);
		}

		optval = 1;
		if (setsockopt(server,
			       SOL_SOCKET,
			       SO_REUSEPORT,
			       &optval,
			       sizeof(optval)) == -1)
		{
			fprintf(stderr,
				"setsockopt(SO_REUSEPORT) failed. %d: %s\n",
				errno,
				strerror(errno));
			exit(1);
		}
	}

	struct sockaddr_in addr;
	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	addr.sin_addr.s_addr = INADDR_ANY;

	printf("Binding on port %d\n", port);
	if (bind(server, (struct sockaddr *)&addr, sizeof(addr)) == -1)
	{
		fprintf(
		    stderr, "bind failed: %d: %s\n", errno, strerror(errno));
		exit(1);
	}

	if (listen(server, 5) == -1)
	{
		fprintf(
		    stderr, "listen failed: %d: %s\n", errno, strerror(errno));
		exit(1);
	}

	printf("Listening on port %d...\n", port);
	pid_t children[3] = {0};
	int childnum = 0;
	for (; childnum < 3; ++childnum)
	{
		children[childnum] = fork();
		if (children[childnum] == 0)
		{
			break;
		}
	}

	bool const is_parent =
	    childnum == sizeof(children) / sizeof(children[0]);
	bool const is_child = !is_parent;

	if (is_child)
	{
		// This is a child process.
		child_loop(server, childnum);
	}
	else
	{
		// This is the parent process.
		parent_loop();
	}

	return 0;
}

void child_loop(int server, int childnum)
{
	unsigned int counter = 0;
	char outfile[1024];
	snprintf(outfile, sizeof(outfile), "/tmp/prefork_child_%d", childnum);
	FILE *out = fopen(outfile, "w");
	fprintf(out, "Starting child %d PID %u\n", childnum, getpid());
	while (1)
	{
		struct sockaddr_in addr;
		memset(&addr, 0, sizeof(addr));
		socklen_t addrlen = sizeof(addr);
		int conn = accept(server, (struct sockaddr *)&addr, &addrlen);
		if (conn == -1)
		{
			fprintf(out,
				"ERROR %u accept failed: %d: %s\n",
				counter,
				errno,
				strerror(errno));
			continue;
		}

		char buf[100];
		const char *displayString = inet_ntop(
		    addr.sin_family, &addr.sin_addr, buf, sizeof(buf));
		buf[sizeof(buf) - 1] = 0; // make sure 0 terminated
		if (displayString)
		{
			fprintf(out,
				"%u Accepted connection from %s:%d\n",
				counter,
				displayString,
				ntohs(addr.sin_port));
		}
		else
		{
			fprintf(out,
				"%u inet_ntop failed: %d: %s\n",
				counter,
				errno,
				strerror(errno));
		}

		close(conn);
		counter++;
	}
	fclose(out);
}

void parent_loop()
{
	unsigned int counter = 0;
	while (1)
	{
		printf("Parent sleeping %d", counter++);
		sleep(10);
	}
}
