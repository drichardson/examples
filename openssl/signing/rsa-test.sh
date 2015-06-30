#!/bin/bash
set -e

# Generate input file
BASEDIR=rsa_test_files
mkdir -p $BASEDIR

INPUT_FILE=$BASEDIR/input
DIGEST_FILE=$INPUT_FILE.digest
SIGNATURE_FILE=$DIGEST_FILE.signature
PRIVATE_KEY=$BASEDIR/private-key.pem
PUBLIC_KEY=$BASEDIR/public-key.pem

cat <<EOF > $INPUT_FILE
This is a test.

Doug
EOF

# Generate private key
# According to https://wiki.openssl.org/index.php/Elliptic_Curve_Cryptography
# 2048 RSA is about equal to 112 bit symmetric key strength.
openssl genrsa -out $PRIVATE_KEY 2048

# Derive public key from private key
openssl rsa -in $PRIVATE_KEY -pubout > $PUBLIC_KEY

# Compute SHA-256 digest of input file
openssl dgst -sha256 -out $DIGEST_FILE -binary $INPUT_FILE

# Sign the digest
openssl rsautl -sign -in $DIGEST_FILE -out $SIGNATURE_FILE -inkey $PRIVATE_KEY

# Verify the signature
DIGEST_FILE_2=$DIGEST_FILE.2
openssl rsautl -verify -in $SIGNATURE_FILE -inkey $PUBLIC_KEY -pubin -out $DIGEST_FILE_2

# Compare the signatures. Is this necessary?
diff $DIGEST_FILE $DIGEST_FILE_2
echo OK
