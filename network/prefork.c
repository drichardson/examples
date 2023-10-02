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
#include <time.h>
#include <unistd.h>

struct timespec start_time;

void init_start_time()
{
	clock_gettime(CLOCK_MONOTONIC, &start_time);
}

double get_time_since_start()
{
	struct timespec current_time;
	clock_gettime(CLOCK_MONOTONIC, &current_time);
	return (current_time.tv_sec - start_time.tv_sec) +
	       (current_time.tv_nsec - start_time.tv_nsec) / 1e9;
}

void child_loop(int server, int childnum, char loadtype);
void parent_loop();
void load(double seconds);

int main(int argc, char **argv)
{
	if (argc != 3)
	{
		fprintf(stderr,
			"Expected 2 args but got %d.\n"
			"Usage: acceptclose PORT LOAD\n"
			"LOAD is a mode and can be:\n"
			"  n: no load\n"
			"  b: 100-300ms balanced across workers\n"
			"  s: 100-300ms skewed across workers\n",
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

	const char loadtype = argv[2][0];

	int server = socket(AF_INET, SOCK_STREAM, 0);
	if (server == -1)
	{
		fprintf(stderr,
			"socket failed for server with %d: %s\n",
			errno,
			strerror(errno));
		exit(1);
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
		child_loop(server, childnum, loadtype);
	}
	else
	{
		// This is the parent process.
		parent_loop();
	}

	return 0;
}

void child_loop(int server, int childnum, char loadtype)
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

		switch (loadtype)
		{
		case 'n':
			// no load
			break;
		case 'b':
			// balanced by worker
			load(counter % 10);
			break;
		case 's':
			// skewed across workers
			load(1 + childnum);
			break;
		default:
			fprintf(stderr, "INVALID LOAD TYPE\n");
			exit(1);
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

double delta_seconds(struct timespec t0, struct timespec t1)
{
	return (t1.tv_sec - t0.tv_sec) + (t1.tv_nsec - t0.tv_nsec) / 1e9;
}

void load(double seconds)
{
	struct timespec t0;
	clock_gettime(CLOCK_MONOTONIC, &t0);
	while (true)
	{
		struct timespec now;
		clock_gettime(CLOCK_MONOTONIC, &now);
		// printf("DELTA: %f\n", delta_seconds(t0, now));
		if (delta_seconds(t0, now) >= seconds)
		{
			break;
		}
	}
}
