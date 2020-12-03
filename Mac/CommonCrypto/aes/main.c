#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>
#include <strings.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

#include <CommonCrypto/CommonCryptor.h>

static void AES_Encrypt(void);
static void AES_Decrypt(void);

int main (int argc, const char * argv[])
{
	if(argc < 2)
	{
		fprintf(stderr, "Usage: aes <e|d>\n");
		exit(1);
	}
	
	switch(tolower(argv[1][0]))
	{
		case 'e':
			AES_Encrypt();
			break;
			
		case 'd':
			AES_Decrypt();
			break;
			
		default:
			fprintf(stderr, "Invalid mode. Expected e or d\n");
			exit(1);
	}
	
    return 0;
}

static void AES_Operation(CCOperation operation, void* key, size_t keySize,
						  FILE *fpInput, FILE *fpOutput)
{
	CCCryptorRef cryptorRef;
	CCCryptorStatus rc;
	rc = CCCryptorCreate(operation, kCCAlgorithmAES128, 0, key, keySize, NULL, &cryptorRef);
	assert(rc == kCCSuccess);
	
	char rawData[128/8];
	size_t bytesRead;
	while((bytesRead = fread(rawData, 1, sizeof(rawData), fpInput)) > 0)
	{
		char convertedData[128/8];
		size_t dataOutMoved;
		
		if(bytesRead < sizeof(rawData))
			bzero(&rawData[bytesRead], sizeof(rawData) - bytesRead);
		
		rc = CCCryptorUpdate(cryptorRef, rawData, sizeof(rawData), convertedData, sizeof(convertedData), &dataOutMoved);
		assert(rc == kCCSuccess);
		//assert(dataOutMoved == sizeof(convertedData));
		if(dataOutMoved != sizeof(convertedData))
			printf("Data out moved (%d) != converted (%d)\n", dataOutMoved, convertedData);
		
		if(dataOutMoved > 0)
			fwrite(convertedData, dataOutMoved, 1, fpOutput);
	}
	
	CCCryptorRelease(cryptorRef);
}

static void AES_Encrypt(void)
{
	char key[256/8];
	
	int fdRandom = open("/dev/random", O_RDONLY);
	int status = read(fdRandom, key, sizeof(key));
	if(status != sizeof(key))
	{
		fprintf(stderr, "Could not read random key. %s\n", strerror(errno));
		exit(1);
	}
	close(fdRandom);
	
	FILE *fpKeyFile = fopen("aes.key", "wb");
	fwrite(key, sizeof(key), 1, fpKeyFile);
	fclose(fpKeyFile);
	
	FILE *fpEncryptedFile = fopen("aesEncryptedFile", "w");
	
	AES_Operation(kCCEncrypt, key, sizeof(key), stdin, fpEncryptedFile);
	
	fclose(fpEncryptedFile);
}

static void AES_Decrypt(void)
{
	FILE *fpKeyFile = fopen("aes.key", "rb");
	char key[256/8];
	fread(key, sizeof(key), 1, fpKeyFile);
	fclose(fpKeyFile);
	
	FILE *fpEncryptedFile = fopen("aesEncryptedFile", "r");
	AES_Operation(kCCDecrypt, key, sizeof(key), fpEncryptedFile, stdout);
	fclose(fpEncryptedFile);
}

