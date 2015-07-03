#!/bin/bash
set -e

# Generate input file
BASEDIR=hmac_sha256_files
mkdir -p $BASEDIR

INPUT_FILE=$BASEDIR/input
DIGEST_FILE=$INPUT_FILE.digest
SIGNATURE_FILE=$DIGEST_FILE.signature
PRIVATE_KEY_FILE=$BASEDIR/private-key

cat <<EOF > $INPUT_FILE
This is a test.

Doug
EOF

# Randomly generate 128-bit (16 byte) private key
PRIVATE_KEY=$(openssl rand -hex 16|tr -d '\n')
echo $PRIVATE_KEY > $PRIVATE_KEY_FILE

# Compute HMAC
openssl dgst -binary -sha512 -hmac "$PRIVATE_KEY" $INPUT_FILE > $SIGNATURE_FILE

# Verify HMAC
openssl dgst -binary -sha512 -hmac "$PRIVATE_KEY" $INPUT_FILE > $SIGNATURE_FILE.verify
diff $SIGNATURE_FILE $SIGNATURE_FILE.verify

echo OK
