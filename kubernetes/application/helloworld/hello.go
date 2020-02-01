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
	log.Print("Starting hello application")
	http.HandleFunc("/", index)
	log.Fatal(http.ListenAndServe(*address, nil))
}

func index(w http.ResponseWriter, r *http.Request) {
	log.Printf("index handler, counter: %d", counter)
	w.Write([]byte(fmt.Sprintf("Hello, World!\nCounter: %d\n", counter)))
	counter++
}
