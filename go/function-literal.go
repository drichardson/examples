package main

import "fmt"

func main() {
	for i := 0; i < 10; i++ {
		g := func(i int) { fmt.Printf("%d", i) }
		g(i)
	}
	fmt.Printf("\n")
}
