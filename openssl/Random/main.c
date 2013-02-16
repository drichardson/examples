#include <stdio.h>
#include <unistd.h>
#include <strings.h>
#include <openssl/rand.h>

int main (int argc, const char * argv[])
{
	RAND_seed("1234", 4);
	//printf("Enough seed data? %d\n", RAND_status());
	
	unsigned char buf[20];
	bzero(buf, sizeof(buf));
	RAND_bytes(buf, 20);
	
	write(STDOUT_FILENO, buf, 20);	
	
    return 0;
}
