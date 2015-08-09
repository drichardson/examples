package main

import (
	"crypto/md5"
	"crypto/sha1"
	"crypto/sha256"
	"crypto/sha512"
	"encoding/hex"
	"flag"
	"fmt"
	"hash"
	"hash/fnv"
	"os"
	"time"
)

var algorithm = flag.String("algo", "sha256", "Algorithm to use: sha256,md5,fnv1,fnv1a")
var iterations = flag.Int("count", 100, "Number of iterations")
var data = flag.String("data", "0123456789", "Data to hash")

func main() {

	flag.Parse()

	var h hash.Hash

	switch *algorithm {
	case "sha1":
		h = sha1.New()
	case "sha256":
		h = sha256.New()
	case "sha512":
		h = sha512.New()
	case "md5":
		h = md5.New()
	case "fnv1":
		h = fnv.New64()
	case "fnv1a":
		h = fnv.New64a()
	default:
		fmt.Println("Invalid hash algorithm", *algorithm)
		os.Exit(1)
	}

	bytes := []byte(*data)

	h.Reset()
	h.Write(bytes)
	digest := h.Sum(make([]byte, 0))
	fmt.Println("digest", hex.EncodeToString(digest))

	fmt.Println("Starting", *iterations, "iterations...")
	start := time.Now()
	for i := 0; i < *iterations; i++ {
		h.Reset()
		h.Write(bytes)
		h.Sum(make([]byte, 0))
	}
	duration := time.Since(start).Seconds()
	fmt.Println("Done in", duration, "seconds or about", 1000000000*duration/float64(*iterations), "nanoseconds per iteration")
}
