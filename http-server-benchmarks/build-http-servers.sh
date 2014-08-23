#!/bin/bash
# This script should be idempotent.
# To run, do something like this:
# ssh doug@104.130.18.76 'bash -s' < build-http-servers.sh

set -e

if [ "$UID" == 0 ]; then
    echo "ERROR: you're running as root. Connect as a non-root user."
    exit 1
fi

build_nginx() {
    [ -e nginx-1.7.4.tar.gz ] ||
        curl -O http://nginx.org/download/nginx-1.7.4.tar.gz
    sha256sum --check - <<EOF
935c5a5f35d8691d73d3477db2f936b2bbd3ee73668702af3f61b810587fbf68  nginx-1.7.4.tar.gz
EOF
    rm -rf nginx-1.7.4
    tar xvf nginx-1.7.4.tar.gz
    cd nginx-1.7.4
    env CFLAGS="-march=native -O2" ./configure --without-pcre --without-http_rewrite_module --without-http_gzip_module
    make -j4
}

build_lighttpd() {
    [ -e lighttpd-1.4.35.tar.gz ] ||
        curl -O http://download.lighttpd.net/lighttpd/releases-1.4.x/lighttpd-1.4.35.tar.gz
    sha256sum --check - <<EOF
62c23de053fd82e1bf64f204cb6c6e44ba3c16c01ff1e09da680d982802ef1cc  lighttpd-1.4.35.tar.gz
EOF
    rm -rf lighttpd-1.4.35
    tar xvf lighttpd-1.4.35.tar.gz
    cd lighttpd-1.4.35
    env CFLAGS="-march=native -O2" ./configure --disable-shared --without-pcre --without-zlib --without-bzip2 --disable-ipv6
    make -j4
}

build_lighttpd
build_nginx

echo OK
