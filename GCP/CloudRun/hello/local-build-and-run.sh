#!/bin/bash

set -xe

docker build -t helloworld:latest .
PORT=6000
docker run --rm -p $PORT:5555 helloworld:latest -loglevel=debug -port=5555
