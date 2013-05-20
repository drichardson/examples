package main

import "fmt"

func main() {
    simple()
    loop()
}

func simple() {
    defer fmt.Println("Deferred Println")
	fmt.Println("Normal Println")
}

func loop() {
    for i := 0; i < 5; i++ {
        defer fmt.Printf("Loop %v\n", i)
    }
}

