#!/bin/bash

set -euo pipefail
shopt -s inherit_errexit

GCP_PROJECT=$(gcloud config get-value project)
SERVICE_ACCOUNT=log-test@$GCP_PROJECT.iam.gserviceaccount.com

echo Creating service account $SERVICE_ACCOUNT
gcloud iam service-accounts create log-test

echo Granting logWriter privileges to $SERVICE_ACCOUNT
gcloud projects add-iam-policy-binding \
	$GCP_PROJECT \
	--member=serviceAccount:$SERVICE_ACCOUNT \
	--role=roles/logging.logWriter

echo Service account setup complete.
