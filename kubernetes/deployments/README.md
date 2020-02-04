# kubernetes example

Commands:

    kubectl apply -f hello.yaml

    kubectl get deployments hello-deployment
    kubectl describe deployments hello-deployment

    kubectl get replicasets
    kubectl describe replicasets

    minikube service hello-service --url

    kubectl expose deployment hello-deployment --type=NodePort --name=hello-service
    minikube service hello-service --url


For Google Container Engine:

    gcloud container clusters create my-cluster --zone us-west1-a
    kubectl apply -f hello-gke.yaml
    kubectl get service hello-service




