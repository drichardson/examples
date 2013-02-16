#import <Foundation/Foundation.h>

#include <openssl/pem.h>
#include <openssl/rsa.h>
#include <openssl/bio.h>

static void
checkLicense()
{
	// License key for "Douglas Richardson"
	const char* userLicensedTo = "Douglas Richardson";
	uint8_t md[SHA_DIGEST_LENGTH];
	SHA1((uint8_t*)userLicensedTo, strlen(userLicensedTo), md);
	
	
	// Now decode and unencrypt the message.
	// For PEM, the newlines are important because there must be exactly 64 characters per line.
	char based64EncodedEncryptedLicenseKey[] = "dR/F6Y30l+dyB9XVwgVXTgnxmp9EusEnPkgXuIabTw==\n";
	
	// Read the base64 encoded license.
	BIO *bio, *b64, *bio_out;
	char inbuf[512];
	int inlen;
	BUF_MEM *buf_mem = NULL;
	
	b64 = BIO_new(BIO_f_base64());
	bio = BIO_new_mem_buf(based64EncodedEncryptedLicenseKey, -1);
	BIO_get_mem_ptr(bio, &buf_mem);
	bio_out = BIO_new(BIO_s_mem());
	bio = BIO_push(b64, bio);
	
	while((inlen = BIO_read(bio, inbuf, sizeof(inbuf))) > 0)
		BIO_write(bio_out, inbuf, inlen);
	
	BIO_free_all(bio);
	
	
	void *encryptedLicenseKey = NULL;
	long encryptedLicenseKeySize = BIO_get_mem_data(bio_out, &encryptedLicenseKey);
	
	
	// public key stored in PEM format.
	uint8_t pub_key[] =
		"-----BEGIN PUBLIC KEY-----\n"
		"MDswDQYJKoZIhvcNAQEBBQADKgAwJwIgAMxggot+mBDD1H9bvaMuUMJlG3adDCV5\n"
		"Que2chaytA8CAwEAAQ==\n"
		"-----END PUBLIC KEY-----\n";
	
	// Read the public key from memory
	BIO* bio_pub_key;
	if(bio_pub_key = BIO_new_mem_buf(pub_key, sizeof(pub_key)))
	{
		RSA* rsa_key = 0;
		if(PEM_read_bio_RSA_PUBKEY(bio_pub_key, &rsa_key, NULL, NULL))
		{
			int dst_size = RSA_size(rsa_key);
			uint8_t* dst = malloc(dst_size);
			RSA_public_decrypt(encryptedLicenseKeySize, encryptedLicenseKey,
							   dst, rsa_key, RSA_PKCS1_PADDING);
			
			if(memcmp(dst, md, SHA_DIGEST_LENGTH) == 0)
				printf("The license is valid\n");
			else
				printf("The license sucks\n");
			
			free(dst);
			RSA_free(rsa_key);
		}
		BIO_free(bio_pub_key);
	}
	
	BIO_free(bio_out);
}

int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
	
	checkLicense();
	
#if 0
	// Memory leak checking
	while(1)
		checkLicense();
#endif
	
    [pool release];
    return 0;
}
