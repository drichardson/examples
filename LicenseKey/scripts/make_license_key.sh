#!/bin/bash
# Usage: ./make_license_key.sh <email address>
echo -n "$1" | openssl dgst -sha1 -binary | openssl rsautl -sign -inkey private.pem | openssl enc -base64
