package main

import "fmt"
import "time"

func main() {

    f := func(x int) {
        defer func() {
            if err := recover(); err != nil {
                fmt.Printf("Recovering in x==%v\n", x)
            }
        }()

        if x == 4 {
            panic("x is 4. That's my panic number")
        }

        fmt.Printf("function literal has x == %v\n", x);
    }

    for i := 0; i < 10; i++ {
        go f(i)
    }

    // Wait a litle while for goroutines to complete
    time.Sleep(1*time.Second)
}

