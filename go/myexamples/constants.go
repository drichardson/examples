package main

import "fmt"


func main() {
    const (
        c1 = iota
        c2
        c3
    )

    const (
        c4 = 1 << iota
        c5
        c6
    )

    const (
        c71, c72 = iota * 10, iota
        c81, c82
        c91, c92
    )


    fmt.Println(c1, c2, c3)
    fmt.Println(c4, c5, c6)
    fmt.Println(c71, c72, c81, c82, c91, c92)
}

