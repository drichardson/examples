#!/bin/bash
set -e
ant debug
adb install -r bin/MyFirstApp-debug.apk
adb shell am start com.example1.myfirstapp/com.example1.myfirstapp.MainActivity
