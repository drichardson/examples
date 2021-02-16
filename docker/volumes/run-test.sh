#!/bin/bash

set -e

docker build . -t test1
docker run --name test1-updater --mount source=my-volume,destination=/my-volume test1 /update-file.sh
docker cp test1-updater:/my-volume/the-file.txt RESULT.txt
docker rm test1-updater

echo  "RESULT.txt is:"
cat RESULT.txt
