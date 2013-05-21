package main

import "fmt"

func main() {

    // Struct definition
    type Message struct {
        Name string;
        Body string;
        Time int64;
    }

    // Struct literal
    fmt.Printf("Struct literal is %v\n", Message{"name2", "body2", 456})

    // Array literal
    fmt.Printf("Array literal: %v\n", []int32{4, 5, 6})

    // Function literal
    fmt.Printf("Func literal is %v\n", func(x int) int { return x });

    // Numeric literals
    fmt.Printf("Numbers: %v, %v, %v, %v\n", 16, 0x10, 2e2, 2+3i)

    // Slice literal
    fmt.Printf("Slice literal is %v\n", []int32{1, 2, 6: 3, 10: 4})

    // Map literal
    fmt.Printf("Map literal: %v\n", map[string]float32{"a": 1.23, "b": 3.14, "c": 5e-10})

}

