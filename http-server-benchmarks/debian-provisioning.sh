#!/bin/bash
# Provision a Rackspace OnMetal Debian Wheezy instance
# This script should be idempotent.
# To run, do something like this:
# ssh root@104.130.18.76 'bash -s' < debian-provisioning.sh

set -e

# Use same ssh key for non-root user
copy_roots_authorized_keys_to_user() {
    echo "Installing roots authorized_keys for $1"
    mkdir -p ~$1/.ssh
    cp $HOME/.ssh/authorized_keys ~$1/.ssh/authorized_keys
    chown $1:$1 ~$1/.ssh ~$1/.ssh/authorized_keys
    chmod 600 ~$1/.ssh/authorized_keys
    chmod 700 ~d$1/.ssh
}

create_user () {
    # Create non-root user. If user already exists this will fail.
    set +e
    useradd -m $1
    set -e
}

provision() {
    echo Provisioning host

    # Get some standard stuff I need
    apt-get update
    apt-get --yes install vim emacs gcc g++ git subversion sbcl sudo

    local NONROOT_USER=doug
    create_user $NONROOT_USER
    copy_roots_authorized_keys_to_user $NONROOT_USER

    RESULT=OK    
}

RESULT=FAILURE
trap "{ echo $RESULT; }" EXIT
provision
