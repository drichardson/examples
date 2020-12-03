package main

import (
	"encoding/base64"
	"encoding/hex"
	"flag"
	"fmt"
	"os"
	"time"
)

var algorithm = flag.String("algo", "hex", "Algorithm to use: base64,hex,base64url")
var iterations = flag.Int("count", 10000, "Number of iterations")
var data = flag.String("data", "0123456789", "Data to encode")

type Encoder interface {
	Encode(dst, src []byte)
	EncodeToString(src []byte) string
	EncodedLen(n int) int
}

type hexEncoder int

func (hexEncoder) Encode(dst, src []byte) {
	hex.Encode(dst, src)
}

func (hexEncoder) EncodeToString(src []byte) string {
	return hex.EncodeToString(src)
}

func (hexEncoder) EncodedLen(n int) int {
	return hex.EncodedLen(n)
}

func main() {

	flag.Parse()

	bytes := []byte(*data)

	var encoder Encoder

	switch *algorithm {
	case "hex":
		encoder = hexEncoder(0)
	case "base64":
		encoder = base64.StdEncoding
	case "base64url":
		encoder = base64.URLEncoding
	default:
		fmt.Println("Invalid algorithm", *algorithm)
		os.Exit(1)
	}

	fmt.Println(encoder.EncodeToString(bytes))

	fmt.Println("Starting", *iterations, "iterations...")
	start := time.Now()
	for i := 0; i < *iterations; i++ {
		encoder.Encode(make([]byte, encoder.EncodedLen(len(bytes))), bytes)
	}
	duration := time.Since(start).Seconds()
	fmt.Println("Done in", duration, "seconds or about", 1000000000*duration/float64(*iterations), "nanoseconds per iteration")
}
