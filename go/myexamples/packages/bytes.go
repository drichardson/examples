package main

import (
    "fmt"
    "bytes"
    "os"
)

func main() {

    b1 := []byte("this is a test")
    b2 := []byte("this is a tesT")

    fmt.Printf("Compare: %d\n", bytes.Compare(b1, b1))
    fmt.Printf("Compare: %d\n", bytes.Compare(b1, b2))

    b3 := []byte{1, 2, '\n', 3, 4, 5, ' ', 6, '\t', 7}
    b4 := []byte{3, 4 }
    fmt.Printf("Contains: %v has %v? %v\n", b3, b4, bytes.Contains(b3, b4))

    fmt.Printf("Fields: %v -> %v\n", b3, bytes.Fields(b3))

    var buffer bytes.Buffer
    buffer.Write([]byte("Hello"))
    fmt.Fprintf(&buffer, " world!\n")
    buffer.WriteTo(os.Stdout)
}
