#!/bin/bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
CMD="$DIR/io_test"

function run_test {
if [ -z "$1" ]; then
    echo "No test name specified"
    exit 1
fi

echo "#### Starting Test $1 #####"

if [ -e "$1" ]; then
    echo "Test directory $1 already exists."
    exit 1
fi

mkdir -p "$1"
pushd "$1" >> /dev/null

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

popd >> /dev/null

}

NUMPROCESSES=1
NUMTHREADS=4
BUFSIZEBYTES=0
BYTESPERLOG=173
TESTDURATION=5
MODE=NONE

run_test unbuffered
MODE=FILEPTR
BUFSIZEBYTES=8000
run_test Fbuffered8k
BUFSIZEBYTES=80000
run_test Fbuffered80k
BUFSIZEBYTES=800000
run_test Fbuffered800k
BUFSIZEBYTES=8000000
run_test Fbuffered8000k
BUFSIZEBYTES=67108864
run_test Fbuffered64M
MODE=IOSTREAM
BUFSIZEBYTES=8000
run_test IObuffered8k
BUFSIZEBYTES=80000
run_test IObuffered80k
BUFSIZEBYTES=800000
run_test IObuffered800k
BUFSIZEBYTES=8000000
run_test IObuffered8000k
BUFSIZEBYTES=67108864
run_test IObuffered64M

