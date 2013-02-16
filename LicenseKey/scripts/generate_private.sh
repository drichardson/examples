#!/bin/bash
# Generate a 248 bit key because we have 11 bytes of padding (this is the default)
# + 160 bits for the digest we are encrypting. The key dictates the minimum
# size of the encrypted data (the license key).
openssl genrsa -out private.pem 248
