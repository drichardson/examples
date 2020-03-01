#!/bin/bash

gcloud functions deploy log-test \
           --entry-point LogEndpoint \
           --trigger-http \
           --runtime=go113 \
           --memory=128MB \
           --region=us-central1
