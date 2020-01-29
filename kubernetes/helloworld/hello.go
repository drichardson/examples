package main

import (
	"flag"
	"log"
	"net/http"
)

var address = flag.String("address", ":8080", "Listen address.")

func main() {
	flag.Parse()
	log.Print("Starting server")
	http.HandleFunc("/", index)
	log.Fatal(http.ListenAndServe(*address, nil))
}

func index(w http.ResponseWriter, r *http.Request) {
	log.Print("index method")
	w.Write([]byte("Hello, World!"))
}
