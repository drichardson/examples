# kubernetes example

Commands:

    kubectl apply -f hello.yaml

    kubectl get deployments hello-deployment
    kubectl describe deployments hello-deployment

    kubectl get replicasets
    kubectl describe replicasets

    kubectl apply -f service.yaml
    minikube service hello-service --url

    kubectl expose deployment hello-deployment --type=NodePort --name=my-service
    minikube service my-service --url




