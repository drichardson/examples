#!/bin/bash
openssl enc -base64 -d | openssl rsautl -verify -inkey public.pem -pubin
