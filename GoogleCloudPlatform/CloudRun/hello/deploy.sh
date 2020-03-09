#!/bin/bash

set -e

PROJECT_ID=$(gcloud config get-value project)
echo Detected project ID $PROJECT_ID

set -x
gcloud run deploy hello \
	--image=gcr.io/$PROJECT_ID/hello:latest \
	--platform=managed \
	--region=us-central1 \
	--allow-unauthenticated \
	--args=-loglevel=debug


