#include <stdio.h>
#include <CommonCrypto/CommonDigest.h>

static void hexdump(unsigned char* bytes, size_t size);

int main (int argc, const char * argv[])
{
	unsigned char buf[1024*4];
	unsigned char digest[512/8];
	size_t bytesRead;
	CC_SHA512_CTX ctx;
	
	CC_SHA512_Init(&ctx);
	
	while((bytesRead = fread(buf, 1, sizeof(buf), stdin)) > 0)
		CC_SHA512_Update(&ctx, buf, bytesRead);
	
	CC_SHA512_Final(digest, &ctx);
	
	hexdump(digest, sizeof(digest));
	
    return 0;
}

static void hexdump(unsigned char* bytes, size_t size)
{
	FILE *fp = popen("/usr/bin/xxd", "w");
	fwrite(bytes, size, 1, fp);
	pclose(fp);
}