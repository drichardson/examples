FROM golang:1.13.7-buster AS builder
RUN mkdir /build
WORKDIR /build
COPY helloworld .
WORKDIR /build
RUN go build

FROM debian:buster
COPY --from=builder /build/helloworld /hello
ENTRYPOINT ["/hello", "-address", ":80"]]

