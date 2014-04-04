#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
CMD="$DIR/io_test"

NUMPROCESSES=1
NUMTHREADS=4
BUFSIZEBYTES=1024
BYTESPERLOG=173
TESTDURATION=5
MODE=IOSTREAM

for i in $(seq 1 $NUMPROCESSES)
do
    echo Starting process $i
    "$CMD" $NUMTHREADS $BUFSIZEBYTES $BYTESPERLOG $TESTDURATION $MODE &
    pids[$i]=$!
done

for i in $(seq 1 $NUMPROCESSES)
do
    pid=${pids[$i]}
    echo "Waiting for pid $pid"
    wait $pid
    if [ $? != 0 ]; then
        echo "Process $pid return error"
    fi
done

