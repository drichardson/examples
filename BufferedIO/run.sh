#!/bin/bash
# Run test_runner.sh and capture the output in a report
# A separate script is necessary because I want to print
# the report as the test is running and also capture
# the same information in a file, using tee.

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TESTDIR="test-$(date +"%Y%m%d-%H%M%S")"
mkdir -p "$TESTDIR"
pushd "$TESTDIR" >> /dev/null
echo "Output Directory: $TESTDIR"
"$DIR/test_runner.sh" | tee report

popd >> /dev/null
