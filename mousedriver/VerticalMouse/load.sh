#!/bin/sh

rm -r /tmp/VerticalMouse.kext
cp -R build/Release/VerticalMouse.kext /tmp/VerticalMouse.kext
chown -R root /tmp/VerticalMouse.kext
chgrp -R wheel /tmp/VerticalMouse.kext
#sudo cp -R /tmp/VerticalMouse.kext /System/Library/Extensions
kextunload /tmp/VerticalMouse.kext
kextload /tmp/VerticalMouse.kext
