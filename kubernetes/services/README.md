# Services

> A Pod is the basic execution unit of a Kubernetes application–the smallest
> and simplest unit in the Kubernetes object model that you create or deploy. A
> Pod represents processes running on your Cluster.

Source: [Pods Overview](https://kubernetes.io/docs/concepts/workloads/pods/pod-overview/).

Source: [Service](https://kubernetes.io/docs/concepts/services-networking/service/)

Run the commands below to:

- Create a pod
- Expose application in pod
- curl application endpoint

Commands:

    kubectl create -f service.yaml

    kubectl get pods
    kubectl logs hello-pod 


    kubectl get services
    minikube service hello-service --url

    kubectl delete -f service.yaml


    kubectl create -f service-load-balancer.yaml
    kubectl get services (note pending IP for service)
    minikube tunnel (enter password when prompted)
    kubectl get services (note assigned IP for service)
    curl localhost:80

Alternatives for creation:

    kubectl apply -f pod.yaml
    kubectl apply -f service.yaml

Alternatives for deletion:

    kubectl delete pod hello-pod

