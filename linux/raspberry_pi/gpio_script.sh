#!/bin/bash
set -e

# Note, although I'm sleeping for 0.01 seconds, the value you read
# on the oscilloscope won't agree exactly, because there's also
# overhead to starting a process, which, on the Raspberry Pi
# model B, appears to be about 0.01 seconds for the sleep process.
PIN="25"
HIGH_SECONDS=0.01
LOW_SECONDS=0.01

trap "echo ${PIN} > /sys/class/gpio/unexport" SIGINT SIGTERM

# Export the pin. This creates a sysfs directory for the pin.
echo ${PIN} > /sys/class/gpio/export
PIND="/sys/class/gpio/gpio${PIN}"

# Set the pin for output.
echo "out" > "${PIND}/direction"

echo "Put a scope on pin BMC pin ${PIN} to see signal"
echo "See pin out here http://pi.gadgetoid.com/pinout"

# Switch between high and low.
while true; do
    echo "1" > "${PIND}/value";
    sleep $HIGH_SECONDS;
    echo "0" > "${PIND}/value";
    sleep $LOW_SECONDS;
done
