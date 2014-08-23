#!/bin/bash
# Provision a Rackspace OnMetal Debian Wheezy instance
# This script should be idempotent.
# To run, do something like this:
# ssh root@104.130.18.76 'bash -s' < debian-provisioning.sh

set -e

# Use same ssh key for non-root user
copy_roots_authorized_keys_to_user() {
    echo "Installing root's authorized_keys for $1"
    local UD=/home/$1
    local AUTHKEYS=$UD/.ssh/authorized_keys
    mkdir -p $UD/.ssh
    cp /root/.ssh/authorized_keys $AUTHKEYS
    chown $1:$1 $UD/.ssh $AUTHKEYS
    chmod 600 $AUTHKEYS
    chmod 700 $UD/.ssh
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
    apt-get --yes update
    apt-get --yes install vim emacs gcc g++ git subversion sbcl sudo

    local NONROOT_USER=doug
    create_user $NONROOT_USER
    copy_roots_authorized_keys_to_user $NONROOT_USER
}

RESULT=FAILURE
report_result() {
    echo $RESULT
}
trap "report_result;" EXIT
provision
RESULT=OK
