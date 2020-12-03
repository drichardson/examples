package main

import (
	"flag"
	"fmt"
	"log"
	"net/http"

	"github.com/google/uuid"
)

var address = flag.String("address", ":8080", "Listen address.")
var counter int

var serverUUID uuid.UUID

func main() {
	flag.Parse()
	log.Print("Starting hello application")
	var err error
	serverUUID, err = uuid.NewRandom()
	if err != nil {
		panic(err)
	}
	log.Print("Server UUID:", serverUUID)
	http.HandleFunc("/", index)
	log.Fatal(http.ListenAndServe(*address, nil))
}

func index(w http.ResponseWriter, r *http.Request) {
	log.Printf("serverUUID: %v, counter: %d", serverUUID, counter)
	w.Write([]byte(fmt.Sprintf("Hello, World!\nServer UUID: %v\nCounter: %d\n", serverUUID, counter)))
	counter++
}
