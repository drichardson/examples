package main

import "fmt"

func main() {
    variadic(1);
    variadic(1, 2, 3);
    variadic(4, 5, 6);
}

func variadic(nums...int) {
    sum := 0
    fmt.Printf("Summing %d arguments: ", len(nums));
    prefix := ""
    for _, value := range nums {
        sum += value
        fmt.Printf("%s%d", prefix, value)
        prefix = "+"
    }
    fmt.Printf(" = %d\n", sum);
}

