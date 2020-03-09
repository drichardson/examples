#!/bin/bash

source common.sh

echo "Deleting function"
gcloud functions delete log-test --region=$LOG_TEST_REGION  --quiet
gcloud iam service-accounts delete $LOG_TEST_SERVICE_ACCOUNT --quiet
