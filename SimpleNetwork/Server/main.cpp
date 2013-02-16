#include <iostream>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

using namespace std;

int main (int argc, char * const argv[]) {
    
	cout << "Starting server..." << endl;
	
	int sockfd;
	struct sockaddr_in my_addr;
	struct sockaddr_in their_addr;
	socklen_t sin_size;
	
	sockfd = socket(PF_INET, SOCK_STREAM, 0);
	if(sockfd == -1) {
		perror("socket");
		exit(1);
	}
	
	my_addr.sin_family = AF_INET;
	my_addr.sin_port = htons(3333);
	my_addr.sin_addr.s_addr = INADDR_ANY;
	memset(&(my_addr.sin_zero), 0, 8);
	
	if(bind(sockfd, (struct sockaddr*)&my_addr, sizeof(struct sockaddr)) == -1) {
		perror("bind");
		exit(1);
	}
	
	const int BACKLOG = 10;
	if(listen(sockfd, BACKLOG) == -1) {
		perror("listen");
		exit(1);
	}
	
	while(1) {
		sin_size = sizeof(struct sockaddr_in);
		int new_fd;
		new_fd = accept(sockfd, (struct sockaddr*)&their_addr, &sin_size);
		if(new_fd == -1) {
			perror("accept");
			continue;
		}
		cout << "Server got connection from " << inet_ntoa(their_addr.sin_addr) << endl;
		
		string msg = "Hello, World!\n";
		if(send(new_fd, msg.c_str(), msg.length(), 0) == -1)
			perror("send");
		close(new_fd);
	}
	
    return 0;
}
