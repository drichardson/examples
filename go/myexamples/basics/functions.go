package main

import "fmt"

func main() {

    foo()

    fmt.Printf("bar returned: %v\n", bar());

    x, y := foobie()
    fmt.Printf("foobie returned %v, %v\n", x, y)

    a, b := barbie()
    fmt.Printf("barbie returned %v, %v\n", a, b)

    e,f,j := func() (i int, a []string, f float32) {
        i = 543
        a = []string{"one", "two", "three", "four"}
        f = 3.14
        return
    }()
    fmt.Printf("function literal returned %v, %v, %v\n", e, f, j)
}

func foo() {
    fmt.Println("foo called")
}

func bar() string {
    return "one two three"
}

func foobie() (string, int) {
    return "5+4", 9
}

func barbie() (return1 string, return2 int) {
    return1 = "howdy"
    return2 = 554433
    return
}
