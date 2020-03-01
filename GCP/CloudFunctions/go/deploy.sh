#!/bin/bash

gcloud functions deploy structured-logging-test \
           --entry-point StructuredLoggingEndpoint \
           --trigger-http \
           --runtime=go111 \
           --memory=128MB \
           --region=us-central1
