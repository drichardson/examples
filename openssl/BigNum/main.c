#include <stdio.h>
#include <openssl/bn.h>

int main (int argc, const char * argv[])
{
	BIGNUM *bn1 = NULL;
	BIGNUM *bn2 = NULL;
	BIGNUM *result = BN_new();
	BN_CTX *bn_ctx = BN_CTX_new();
	
	BN_dec2bn(&bn1, "120");
	BN_dec2bn(&bn2, "21234121234");
	
	BN_add(result, bn2, bn1);
	printf("Result is %s\n", BN_bn2dec(result));
	
	BN_mul(result, bn2, bn1, bn_ctx);
	printf("Result is %s\n", BN_bn2dec(result));
	
	BN_exp(result, bn2, bn1, bn_ctx);
	printf("Result is %s\n", BN_bn2dec(result));
	
	BN_free(bn1);
	BN_free(bn2);
	BN_free(result);
	BN_CTX_free(bn_ctx);
	
	return 0;
}
