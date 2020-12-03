#!/bin/bash
openssl rsa -in private.pem -out public.pem -outform PEM -pubout
