#!/bin/bash

x=0;

while true; do
    x=$((x+1));
    echo "Try $x";
    curl --data-binary @Test.plist  http://localhost:1979/test;
done

