#!/bin/sh

rm -r /tmp/USBHIDDriver1.kext
cp -R build/Release/USBHIDDriver1.kext /tmp/USBHIDDriver1.kext
chown -R root /tmp/USBHIDDriver1.kext
chgrp -R wheel /tmp/USBHIDDriver1.kext
kextunload /tmp/USBHIDDriver1.kext
kextload /tmp/USBHIDDriver1.kext
