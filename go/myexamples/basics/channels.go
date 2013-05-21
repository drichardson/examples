package main

import "fmt"
import "time"

func main() {
	var ci chan int
	ci = make(chan int)

	fmt.Println("Starting")

	go func() {
		fmt.Println("going to sleep")
		time.Sleep(2e9)
		fmt.Println("woke up")
		ci <- 1
	}()

	<-ci
	fmt.Println("Got ci signal")
}
