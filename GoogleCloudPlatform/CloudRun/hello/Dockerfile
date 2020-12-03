# Build using a multi-stage build
# https://docs.docker.com/develop/develop-images/multistage-build/

FROM golang:1.14 AS builder
COPY src /build
WORKDIR /build
RUN go build

# Just for fun, build final image from scratch. Determined /helloworld binary dependencies by
# running: ldd helloworld
FROM scratch
COPY --from=builder /build/helloworld /
COPY --from=builder /lib/x86_64-linux-gnu/libpthread.so.0 /lib/x86_64-linux-gnu/libpthread.so.0
COPY --from=builder /lib/x86_64-linux-gnu/libc.so.6 /lib/x86_64-linux-gnu/libc.so.6
COPY --from=builder /lib64/ld-linux-x86-64.so.2 /lib64/ld-linux-x86-64.so.2
ENTRYPOINT ["/helloworld"]

