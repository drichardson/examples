package main

import "fmt"
import "time"

func main() {
	var ci chan int
	ci = make(chan int)

	fmt.Println("Starting")

	go func() {
		fmt.Println("going to sleep")
		time.Sleep(1*time.Second)
		fmt.Println("woke up")
		ci <- 1
	}()

	<-ci
	fmt.Println("Got ci signal")



    go func() {
        // For a channel, range will keep grabbing the next value until the channel is closed.
        for i := range ci {
            fmt.Printf("Got ci value %v\n", i)
        }
        fmt.Printf("Channel closed\n")
    }()

    ci <- 2
    time.Sleep(200*time.Millisecond)
    ci <- 3
    time.Sleep(200*time.Millisecond)
    ci <- 4
    fmt.Printf("Closing channel\n")
    close(ci)
    time.Sleep(100*time.Millisecond)
    fmt.Printf("main exiting")
}
