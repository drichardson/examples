
export GCP_PROJECT=$(gcloud config get-value project)
if [[ $? != 0 ]]; then
	echo "Failed to lookup current GCP project."
	exit 1
fi

# To create service account, run setup-service-acount.sh.
export LOG_TEST_SERVICE_ACCOUNT=log-test@$GCP_PROJECT.iam.gserviceaccount.com

export LOG_TEST_REGION=us-central1

