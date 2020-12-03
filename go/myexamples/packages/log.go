package main

import (
    "log"
)

func main() {
    log.SetFlags(log.Lshortfile | log.Ldate | log.Lmicroseconds)
    log.SetPrefix("MYPREFIX: ")
    log.Print("log message")
    //log.Fatal("fatal message")
    //log.Panic("panic message")
    log.Print("final message")
}

