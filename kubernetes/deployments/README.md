# kubernetes example

## Build and Run Docker Image

bash:

    bash/build.sh
    bash/run.sh

PowerShell:

    powershell\Build-Hello.ps1
    powershell\Run-Hello.ps1


## minikube

If using minikube, you will want to use the version of kubectl that game with
minikube. You can do so like:

    minikube kubectl -- version
    minikube kubectl -- apply -f hello.yaml

Note the double dash, this tells minikube not to process the following
arguments, allowing kubectl to process them.

