# application

This directory contains the example application used by some of the Kubernetes
examples. It is a stateful application that returns the call count each time
it handles an HTTP request.

## Docker Commands

Build image:

    docker build -t hello:1.0 .

List hello images:

    docker images hello

Run container, listening on port 80, and return the containers ID (needed by
stop command):

    docker run --rm --detach --publish 80:80 hello:1.0

Stop container:

    docker stop CONTAINER_ID

Remove image:

    docker rmi hello:1.0

Push to DockerHub:

    docker build -t dougrichardson/hello:1.0 .
    docker push dougrichardson/hello:1.0
