package main

import "fmt"

type MyStruct struct {
    i int
    s string
}

func main() {
    m := MyStruct{100, "example"}
    m.DoSomething()
}

func (ms MyStruct) DoSomething() {
    fmt.Printf("%v\n%+v\n%#v", ms, ms, ms);
}

