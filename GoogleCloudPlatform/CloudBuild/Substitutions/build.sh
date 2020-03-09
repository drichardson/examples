#!/bin/bash

gcloud builds submit \
	--substitutions=_MYVAR1="value1",_MYVAR2="value2" \
	--no-source \
	--timeout=1m

