#!/bin/bash
# Use lighttpd to serve the static UI content.
# The config file starts it on port 4000.
/lighttpd-1.4.20/sbin/lighttpd -f lighttpd.conf -D
