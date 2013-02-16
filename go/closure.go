package main

import "fmt"

func main() {
	f := adder()
	fmt.Printf("%d\n", f(1))
	fmt.Printf("%d\n", f(20))
	fmt.Printf("%d\n", f(300))
}

func adder() (func (int) int) {
	var x int
	return func(delta int) int {
		x += delta
		return x
	}
}
