package main

import (
	"fmt"
	"http"
)

func main() {
	fmt.Printf("Hi there %v\n", 88)
	fmt.Println("String ", string(int64(88)), "lala")
	fmt.Println(http.URLEscape("http://localhost:8080/?key1=val1&key2=val2"))
}
