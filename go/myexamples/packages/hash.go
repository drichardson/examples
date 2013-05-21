package main
import "fmt"

import "hash"
import "hash/adler32"
import "hash/crc32"
import "hash/crc64"
import "hash/fnv"

func main() {
    test := []byte{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18}
    fmt.Println("Test data: ", test)

    hashFunctions := []hash.Hash{adler32.New(), crc32.NewIEEE(), fnv.New32(), crc64.New(crc64.MakeTable(0))}

    for _,hashFunction := range hashFunctions {
        hashFunction.Write(test)
        switch t := hashFunction.(type) {
        case hash.Hash32:
           fmt.Printf("Using %T to hash test results in %d\n", hashFunction, t.Sum32())
        case hash.Hash64:
           fmt.Printf("Using %T to hash test results in %d\n", hashFunction, t.Sum64())
       }
    }
}

