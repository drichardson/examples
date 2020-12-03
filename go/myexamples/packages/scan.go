package main

// to test:
// echo "1234, 123 this is a test" | go run scan.go

import (
	"fmt"
	"os"
)

func main() {
	var f float32
	n, err := fmt.Fscanf(os.Stdin, "%f", &f)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return
	}

	fmt.Printf("Read %v values. f = %v\n", n, f)
}
