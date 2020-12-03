#!/bin/bash

http_client="./http-client/build/Release/http-client"

if [ -z $1 ]; then
    echo "Usage: sum-test.sh <URL>"
    echo "Missing URL!!!"
    exit 1
fi

if [ ! -d http-client ]; then
    echo "Wrong directory. Should be in the same directory as http-client."
    exit 1
fi

if [ ! -e "$http_client" ]; then
    echo "Need to build the http client"
    exit 1
fi

$http_client "$1"
