#!/bin/bash

set -euo pipefail
shopt -s inherit_errexit

source common.sh

echo Creating service account $LOG_TEST_SERVICE_ACCOUNT
gcloud iam service-accounts create log-test

echo Granting logWriter privileges to $LOG_TEST_SERVICE_ACCOUNT
gcloud projects add-iam-policy-binding \
	$GCP_PROJECT \
	--member=serviceAccount:$LOG_TEST_SERVICE_ACCOUNT \
	--role=roles/logging.logWriter

echo Service account setup complete.
