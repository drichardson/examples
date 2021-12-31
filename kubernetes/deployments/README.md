# kubernetes example

Commands:

    kubectl apply -f hello.yaml

    kubectl get deployments hello-deployment
    kubectl describe deployments hello-deployment

    kubectl get replicasets
    kubectl describe replicasets

    kubectl get services (note pending IP address for load balancer)
    minikube tunnel
    kubectl get services (note assigned IP address for load balancer)
    curl localhost:80


Port forward to random pod in service:

    Using minikube:
        minikube service hello-service --url

    Using kubectl:
        kubectl port-forward hello-service 8080:80

Port forward to specific pod:
    kubectl get pods
    kubectl port-forward pods/hello-deployment-5d7445f548-dhmkw 8080:80

Tail the log of a specific pod:
    kubectl logs hello-deployment-5d7445f548-cmffd -f

For Google Container Engine:

    gcloud container clusters create my-cluster
    kubectl apply -f hello.yaml
    kubectl get service hello-service (note IP_ADDR)
    curl IP_ADDR
    curl IP_ADDR
    curl IP_ADDR
    gcloud container clusters delete my-cluster




