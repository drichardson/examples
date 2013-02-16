package main

import (
	"http"
	"io"
	"fmt"
)

func main() {
	http.HandleFunc("/hello", func(w http.ResponseWriter, req *http.Request) {
		io.WriteString(w, "hello, world!\n")
	})
	
	err := http.ListenAndServe(":12345", nil)
	
	if err != nil {
		fmt.Println("ListenAndServe error: ", err)
	}
}
