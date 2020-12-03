FROM debian:stable
ENV MY_VAR1="myvalue 1"
WORKDIR /bin
RUN groupadd -r mygroup && useradd -r -g mygroup myuser
USER myuser
COPY ["print_pid/print_pid", "/go/print_pid"]
ENTRYPOINT ["/go/print_pid"]
