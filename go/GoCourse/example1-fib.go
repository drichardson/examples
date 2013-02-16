package main

import "./fibonacci/fibonacci"
import "fmt"

func main() {

	// Use a function, F, that computes the ith Fibonacci number.
	for i := 0; i < 5; i++ {
		fmt.Printf("F(%d) = %d\n", i, fibonacci.F(i))
	}
	
	// Get a series generator that, when called, returns the next Fibonacci number
	var operation = func (x int64, y int64) (int64) { return x + y }
	var next = fibonacci.Series(operation)
	
	for i := 0; i < 20; i++ {
		fmt.Println(next())
	}
}