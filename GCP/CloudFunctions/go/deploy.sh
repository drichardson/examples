#!/bin/bash

GCP_PROJECT=$(gcloud config get-value project)
if [[ $? != 0 ]]; then
	echo "Failed to lookup current GCP project."
	exit 1
fi

# To create service account, run setup-service-acount.sh.
SERVICE_ACCOUNT=log-test@$GCP_PROJECT.iam.gserviceaccount.com

cat <<EOF
- Current GCP project is $GCP_PROJECT
- Using service account $SERVICE_ACCOUNT. If it does not exist, run
  setup-service-account.sh.
EOF

gcloud functions deploy log-test \
           --entry-point LogEndpoint \
           --trigger-http \
           --runtime=go113 \
           --memory=128MB \
           --region=us-central1 \
		   --set-env-vars=GCP_PROJECT=$GCP_PROJECT \
		   --service-account=$SERVICE_ACCOUNT
