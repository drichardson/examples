# Pods

> A Pod is the basic execution unit of a Kubernetes application–the smallest
> and simplest unit in the Kubernetes object model that you create or deploy. A
> Pod represents processes running on your Cluster.

Source: [Pods Overview](https://kubernetes.io/docs/concepts/workloads/pods/pod-overview/).

Run the commands below to:

- Create a pod
- List pods
- Display pod logs
- Delete the pod

Commands:

    kubectl create -f pod.yaml
    kubectl get pods
    kubectl logs hello-pod 
    kubectl delete -f pod.yaml

Alternatives for creation:

    kubectl apply -f pod.yaml

Alternatives for deletion:

    kubectl delete pod hello-pod

