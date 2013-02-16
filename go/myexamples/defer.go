package main

import "fmt"

func main() {
	defer fmt.Println("Deferred Println")
	fmt.Println("Normal Println")
}
