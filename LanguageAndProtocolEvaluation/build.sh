#!/bin/bash

if [ ! -d pen-0.18.0 ]; then
    echo "You're not in the correct directory to run this script."
    exit 1
fi

echo "Starting build..."

pushd pen-0.18.0 || exit 1
./configure --prefix=/opt || exit 1
make || exit 1
popd

echo "Build complete"
