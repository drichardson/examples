package main

import "fmt"

func main() {
    tryForString("testing")
    tryForString(123)
}

func tryForString(x interface{}) {
    str, ok := x.(string)
    if ok {
        fmt.Printf("Turned x into string %s\n", str);
    } else {
        fmt.Printf("Couldn't turn x (%T) into string\n", x)
    }
}

