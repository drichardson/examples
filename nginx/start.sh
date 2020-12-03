#!/bin/bash
if [ ! -e ./start.sh ]; then
    echo This script must be run from the directory that contains start.sh
    exit 1
fi

echo Starting nginx...
nginx -c `pwd`/nginx.conf
echo nginx started. To stop, run: nginx -s stop
