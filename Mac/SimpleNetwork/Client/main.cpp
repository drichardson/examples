#include <iostream>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/types.h>

using namespace std;

int main (int argc, char * const argv[]) {

	cout << "Starting client..." << endl;
	
	int sockfd, numbytes;
	const int MAXDATASIZE = 100;
	char buf[MAXDATASIZE];
	struct hostent *he;
	struct sockaddr_in their_addr;
	
	he = gethostbyname("localhost");
	if(he == NULL) {
		herror("gethostbyname");
		exit(1);
	}
	
	sockfd = socket(PF_INET, SOCK_STREAM, 0);
	if(sockfd == -1) {
		perror("socket");
		exit(1);
	}
	
	their_addr.sin_family = AF_INET;
	their_addr.sin_port = htons(3333);
	their_addr.sin_addr = *((struct in_addr*)he->h_addr);
	memset(&(their_addr.sin_zero), 0, 8);
	
	if(connect(sockfd, (struct sockaddr*)&their_addr, sizeof(struct sockaddr)) == -1) {
		perror("connect");
		exit(1);
	}
	
	numbytes = recv(sockfd, buf, MAXDATASIZE-1, 0);
	if(numbytes == -1) {
		perror("recv");
		exit(1);
	}
	buf[numbytes] = 0;
	printf("Received: %s", buf);
	close(sockfd);
	
    return 0;
}
