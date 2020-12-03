#!/bin/bash

gcloud run services delete hello --platform=managed --region=us-central1

PROJECT_ID=$(gcloud config get-value project)
IMAGE="gcr.io/$PROJECT_ID/hello:latest"

echo "Removing hello container image: $IMAGE"
gcloud container images delete $IMAGE

