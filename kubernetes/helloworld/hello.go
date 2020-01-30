package main

import (
	"flag"
	"fmt"
	"log"
	"net/http"
)

var address = flag.String("address", ":8080", "Listen address.")
var counter int

func main() {
	flag.Parse()
	log.Print("Starting server")
	http.HandleFunc("/", index)
	log.Fatal(http.ListenAndServe(*address, nil))
}

func index(w http.ResponseWriter, r *http.Request) {
	log.Print("index method")
	w.Write([]byte(fmt.Sprintf("Hello, World!\nCounter: %d\n", counter)))
	counter++
}
