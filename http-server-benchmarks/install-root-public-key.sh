#!/bin/bash
# Install public key for root

set -e
trap "{ echo $RESULT; }" EXIT

HOST=$1
RESULT=FAIL

if [ -z "$HOST" ]; then
    echo "Missing host argument."
    echo "Usage: install-root-public-key.sh <host>"
    exit 1 
fi

PUBKEY_FILE=~/.ssh/id_rsa.pub
echo "Using public key from $PUBKEY_FILE"
PUBKEY=$(cat ~/.ssh/id_rsa.pub)
if [ -z "$PUBKEY" ]; then
    echo "Empty public key"
    exit 1
fi

ssh root@$HOST 'bash -s' <<EOF
set -e
mkdir -p /root/.ssh
chmod 700 /root/.ssh
cat <<INNER_EOF> /root/.ssh/authorized_keys
$PUBKEY
INNER_EOF

EOF

RESULT=OK

