package main

import (
	"fmt"
	"net/url"
)

func main() {
	fmt.Printf("Hi there %v\n", 88)
	fmt.Println("String ", string(int64(88)), "lala")
	fmt.Println(url.QueryEscape("http://localhost:8080/?key1=val1&key2=val2"))
}
