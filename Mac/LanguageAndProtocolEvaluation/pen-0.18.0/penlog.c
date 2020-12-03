/*
   Copyright (C) 2000-2003  Ulric Eriksson <ulric@siag.nu>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#define MAXBUF 1024

int main(int argc, char **argv)
{
	int sk;
	struct sockaddr_in client, server;
	struct hostent *hp;
	char buf[MAXBUF];
	int n;

	if (argc < 3) {
		printf("Usage: %s server_ip server_port [my_ip]\n", argv[0]);
		exit(0);
	}

	if ((sk = socket(PF_INET, SOCK_DGRAM, 0)) < 0) {
		printf("Problem creating socket\n");
		exit(1);
	}

	if (argc > 3) {	/* set local address */
		hp = gethostbyname(argv[3]);
		if (hp == NULL) {
			fprintf(stderr, "Bogus local address %s\n", argv[3]);
			exit(1);
		}
		memcpy(&client.sin_addr.s_addr, hp->h_addr, hp->h_length);
		client.sin_port = 0;
		if (bind(sk, (struct sockaddr *)&client, sizeof client) < 0) {
			fprintf(stderr, "bind failure\n");
			exit(1);
		}
	}

	server.sin_family = AF_INET;
	hp = gethostbyname(argv[1]);
	if (hp == NULL) {
		fprintf(stderr, "Bogus server address %s\n", argv[1]);
		exit(1);
	}

	memcpy(&server.sin_addr.s_addr, hp->h_addr, hp->h_length);

	server.sin_port = htons(atoi(argv[2]));

	while (fgets(buf, sizeof buf, stdin)) {
		n = sendto(sk, buf, strlen(buf), 0,
			(struct sockaddr *) &server, sizeof server);

		if (n < 0) {
			perror("Problem sending data");
		}
	}

	return 0;
}
