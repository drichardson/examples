#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>

#include <png.h>

#include <CommonCrypto/CommonDigest.h>

#include <openssl/pem.h>
#include <openssl/rsa.h>
#include <openssl/bio.h>

static bool sign_png(const char* png_filename, const char* private_key_filename);


typedef struct MyPNGInfo
{
	png_structp png_ptr;
	png_infop info_ptr;
	png_bytep * row_pointers;
} MyPNGInfo;

static bool read_png_file(const char* file_name, MyPNGInfo *pngInfo);
static bool write_png_file(const char* file_name, MyPNGInfo *pngInfo, unsigned char* signature);
static void MyPNGInfo_free(MyPNGInfo *pngInfo);


static void abort_(const char * s, ...);



int main (int argc, const char * argv[])
{
	if(argc < 3)
		abort_("Invalid number of arguments. Needs png and private key filename");
	
	return sign_png(argv[1], argv[2]);
}

static void SHA512RowPointers(MyPNGInfo *pngInfo, unsigned char *digest)
{
	CC_SHA512_CTX ctx;
	png_uint_32 i;
	png_uint_32 height = pngInfo->info_ptr->height;
	png_uint_32 rowbytes = pngInfo->info_ptr->rowbytes;
	
	CC_SHA512_Init(&ctx);
	
	for(i = 0; i < height; i++)
		CC_SHA512_Update(&ctx, pngInfo->row_pointers[i], rowbytes);
	
	CC_SHA512_Final(digest, &ctx);
}

#if 0
static void GetRandomBytes(unsigned char* bp, size_t bytes_to_read)
{
	FILE *fp = fopen("/dev/urandom", "r");
	
	if(fp)
	{
		fread(bp, bytes_to_read, 1, fp);
		fclose(fp);
	}
	else
	{
		abort_("Couldn't get random bytes.");
	}
}
#endif

static void encrypt_signature(const unsigned char* digest, unsigned char* cipher_signature, const char* private_key_filename)
{
	BIO *bio_private_key = NULL;
	RSA *rsa_private_key = NULL;
	
	bio_private_key = BIO_new_file(private_key_filename, "r");
	if(bio_private_key == NULL)
	{
		abort_("Couldn't read private key");
		goto bail;
	}
	
	//PEM_read_bio_RSAPrivateKey(bio_private_key, &rsa_private_key, NULL, <#void * u#>)
	rsa_private_key = PEM_read_bio_RSAPrivateKey(bio_private_key, NULL, NULL, NULL);
	if(rsa_private_key == NULL)
	{
		abort_("Cound't read private key PEM from bio");
		goto bail;
	}
	
	bzero(cipher_signature, 512);
	//int rc = RSA_private_encrypt(64, signature, cipher_signature, rsa_private_key, RSA_PKCS1_PADDING);
	assert(RSA_size(rsa_private_key) == 512);
	unsigned int siglen = 0;
	int rc = RSA_sign(NID_sha512, digest, 64, cipher_signature, &siglen, rsa_private_key);
	if(rc == -1)
	{
		abort_("Couldn't encrypt signature.");
		goto bail;
	}
	
	assert(siglen == 512);
	
bail:
	if(bio_private_key)
		BIO_free(bio_private_key);
	if(rsa_private_key)
		RSA_free(rsa_private_key);
}

static bool sign_png(const char* png_filename, const char* private_key_filename)
{
	bool rc = false;
	
	if(png_filename == NULL)
	{
		abort_("No png filename");
		goto bail;
	}
	
	MyPNGInfo pngInfo;
	
	if(!read_png_file(png_filename, &pngInfo))
	{
		abort_("Couldn't read png file: %s", png_filename);
		goto bail;
	}
	
	unsigned char digest[CC_SHA512_DIGEST_LENGTH];
	SHA512RowPointers(&pngInfo, digest);

#if 0
	uint8_t signature[512]; // 512 is 4096 bits, the size of the private key modulus.
	memcpy(signature, digest, sizeof(digest));
	GetRandomBytes(&signature[sizeof(digest)], sizeof(signature) - sizeof(digest));
#endif
	
	unsigned char cipher_signature[512];
	encrypt_signature(digest, cipher_signature, private_key_filename);
	
	if(!write_png_file(png_filename, &pngInfo, cipher_signature))
	{
		abort_("Couldn't write png file: %s", png_filename);
		goto bail;
	}
	
	MyPNGInfo_free(&pngInfo);
	
bail:
	return rc;
}

#if 0
static int read_chunk_callback(png_structp png_ptr, png_unknown_chunkp chunk)
{
#if 0
	/* The unknown chunk structure contains your
	 chunk data, along with similar data for any other
	 unknown chunks: */
	
	png_byte name[5];
	png_byte *data;
	png_size_t size;
	
	/* Note that libpng has already taken care of
	 the CRC handling */
	
	/* put your code here.  Search for your chunk in the
	 unknown chunk structure, process it, and return one
	 of the following: */
	
	return (-n); /* chunk had an error */
	return (0); /* did not recognize */
	return (n); /* success */
#endif
	
	MyPNGInfo *pngInfo = (MyPNGInfo*)png_ptr->io_ptr;
	
	
	printf("Got chunk:\n\tName:%s\n\tSize: %d\n\tData: %s\n", chunk->name, chunk->size, chunk->data);
	return 1;
}
#endif


static bool read_png_file(const char* file_name, MyPNGInfo *pngInfo)
{
	bool rc = false;
	unsigned char header[8];	// 8 is the maximum size that can be checked
	
	//pngInfo->io_ptr = pngInfo;
	
	/* open file and test for it being a png */
	FILE *fp = fopen(file_name, "rb");
	if (!fp)
		abort_("[read_png_file] File %s could not be opened for reading", file_name);
	fread(header, 1, 8, fp);
	if (png_sig_cmp(header, 0, 8))
		abort_("[read_png_file] File %s is not recognized as a PNG file", file_name);
	
	
	/* initialize stuff */
	pngInfo->png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	
	//png_handle_as_unknown(png_ptr, kMyChunkName);
	//png_set_read_user_chunk_fn(pngInfo->png_ptr, png_get_user_chunk_ptr(pngInfo->png_ptr), read_chunk_callback);
	
	if (!pngInfo->png_ptr)
		abort_("[read_png_file] png_create_read_struct failed");
	
	pngInfo->info_ptr = png_create_info_struct(pngInfo->png_ptr);
	if (!pngInfo->info_ptr)
		abort_("[read_png_file] png_create_info_struct failed");
	
	if (setjmp(png_jmpbuf(pngInfo->png_ptr)))
		abort_("[read_png_file] Error during init_io");
	
	png_init_io(pngInfo->png_ptr, fp);
	png_set_sig_bytes(pngInfo->png_ptr, 8);
	
	png_read_info(pngInfo->png_ptr, pngInfo->info_ptr);
		
	png_set_interlace_handling(pngInfo->png_ptr);
	png_read_update_info(pngInfo->png_ptr, pngInfo->info_ptr);
	
	
	/* read file */
	if (setjmp(png_jmpbuf(pngInfo->png_ptr)))
		abort_("[read_png_file] Error during read_image");
	
	png_uint_32 height = pngInfo->info_ptr->height;
	png_uint_32 rowbytes = pngInfo->info_ptr->rowbytes;
	
	pngInfo->row_pointers = (png_bytep*) malloc(sizeof(png_bytep) * height);
	png_uint_32 y;
	for (y = 0; y < height; y++)
		pngInfo->row_pointers[y] = (png_byte*) malloc(rowbytes);
	
	png_read_image(pngInfo->png_ptr, pngInfo->row_pointers);
	
	fclose(fp);
	
	rc = true;
	
bail:
	return rc;
}


static bool write_png_file(const char* file_name, MyPNGInfo *pngInfo, unsigned char* signature)
{
	/* create file */
	FILE *fp = fopen(file_name, "wb");
	if (!fp)
		abort_("[write_png_file] File %s could not be opened for writing", file_name);
	
	
	/* initialize stuff */
	png_structp png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	
	if (!png_ptr)
		abort_("[write_png_file] png_create_write_struct failed");
	
	png_infop info_ptr = png_create_info_struct(png_ptr);
	if (!info_ptr)
		abort_("[write_png_file] png_create_info_struct failed");
	
	if (setjmp(png_jmpbuf(png_ptr)))
		abort_("[write_png_file] Error during init_io");
	
	png_init_io(png_ptr, fp);
	
	
	/* write header */
	if (setjmp(png_jmpbuf(png_ptr)))
		abort_("[write_png_file] Error during writing header");
	
	png_set_IHDR(png_ptr, info_ptr, pngInfo->info_ptr->width, pngInfo->info_ptr->height,
				 pngInfo->info_ptr->bit_depth, pngInfo->info_ptr->color_type, PNG_INTERLACE_NONE,
				 PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);
	
	
	
	if (setjmp(png_jmpbuf(png_ptr)))
		abort_("[png_write_chunk] Failed writing private chunk.");
	
	png_write_info(png_ptr, info_ptr);
	
	
	/* write bytes */
	if (setjmp(png_jmpbuf(png_ptr)))
		abort_("[write_png_file] Error during writing bytes");
	
	
	// Chunk name:
	// 1st letter lower case = anciliary
	// 2nd letter lower case = private
	// 3rd letter must be uppercase
	// 4th letter lower case means editors can copy the chunk verbatim even if the image changes.
	png_write_chunk(png_ptr, (png_bytep)"keYs", signature, 64);	
	
	
	png_write_image(png_ptr, pngInfo->row_pointers);
	
	
	/* end write */
	if (setjmp(png_jmpbuf(png_ptr)))
		abort_("[write_png_file] Error during end of write");
	
	png_write_end(png_ptr, NULL);
	
	fclose(fp);
	return true;
}



static void MyPNGInfo_free(MyPNGInfo *pngInfo)
{
	png_uint_32 y;
	png_uint_32 height = pngInfo->info_ptr->height;
	
	for(y = 0; y < height; y++)
	{
		if(pngInfo->row_pointers[y])
		{
			free(pngInfo->row_pointers[y]);
			pngInfo->row_pointers[y] = NULL;
		}
	}
	
	if(pngInfo->row_pointers)
	{
		free(pngInfo->row_pointers);
		pngInfo->row_pointers = NULL;
	}
	
#warning Free PNG pointers.
}

static void abort_(const char * s, ...)
{
	va_list args;
	va_start(args, s);
	vfprintf(stderr, s, args);
	fprintf(stderr, "\n");
	va_end(args);
	abort();
}
