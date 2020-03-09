#!/bin/bash

source common.sh

cat <<EOF
- Current GCP project is $GCP_PROJECT
- Using service account $LOG_TEST_SERVICE_ACCOUNT. If it does not exist, run
  setup.sh.
EOF

gcloud functions deploy log-test \
	--quiet \
	--entry-point LogEndpoint \
	--trigger-http \
	--runtime=go113 \
	--memory=128MB \
	--region=$LOG_TEST_REGION \
	--set-env-vars=GCP_PROJECT=$GCP_PROJECT \
	--service-account=$LOG_TEST_SERVICE_ACCOUNT

gcloud functions add-iam-policy-binding log-test \
	--region=$LOG_TEST_REGION \
	--member=allUsers \
	--role=roles/cloudfunctions.invoker 

