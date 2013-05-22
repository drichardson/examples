package main

import "fmt"
import "time"
import "runtime"

func main() {

    // As of go 1.0.3, the runtime doesn't run on more than one core at a time by default.
    // The following code enables multiple core usage.
    fmt.Printf("There are %v logical cores.\n", runtime.NumCPU())
    runtime.GOMAXPROCS(runtime.NumCPU())

    ci := make(chan int)

    go foo("testing", ci)
    go func() {
        fmt.Printf("function literal\n");
        ci <- 2
    }()

    // Wait for goroutines to complete. They'll let you know by signalling the channel
    x := <-ci
    fmt.Printf("ci got %v\n", x)
    x = <-ci
    fmt.Printf("ci got %v\n", x)
}

func foo(s string, c chan int) {
    time.Sleep(500 * time.Millisecond)
    fmt.Printf("foo got %v\n", s)
    c <- 1
}
