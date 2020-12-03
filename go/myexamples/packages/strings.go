package main

import (
    "fmt"
    "strings"
)

func main() {
    fmt.Printf("Contains? %v\n", strings.Contains("testing 1 2 3", "ing"))
    fmt.Printf("Contains? %v\n", strings.Contains("testing 1 2 3", "sing"))

    fmt.Printf("Count: %v\n", strings.Count("this is a test of something that is like is", "is"))

    s := "This is a  test "
    split := strings.Split(s, " ")
    fmt.Printf("Split: %v -> %v\n", s, split)
    fmt.Printf("Join: %v -> %v\n", split, strings.Join(split, "X"))

    fmt.Printf("Index: %d\n", strings.Index(s, "test"))
    fmt.Printf("Map: %v -> %v\n", s, strings.Map(func(r rune) rune {
        switch r {
        case 'i':
            return '1'
        case 'e':
            return '3'
        case 'a':
            return '@'
        }
        return r
    }, s));

    fmt.Printf("Repeat: %v * 3 -> %v\n", s, strings.Repeat(s, 3))

    fmt.Printf("Title: %v -> %v\n", s, strings.Title(s))
    fmt.Printf("ToLower: %v -> %v\n", s, strings.ToLower(s))
    fmt.Printf("ToUpper: %v -> %v\n", s, strings.ToUpper(s))

    s = "another test with space at the end   "
    fmt.Printf("Trim before len %v after len %v\n", len(s), len(strings.Trim(s, " ")))

    stringReader := strings.NewReader(s)
    fmt.Printf("Reader: %v\n", stringReader)

    replacer := strings.NewReplacer("1", "one", "2", "two", "3", "three")
    s = "testing 1 2 3"
    fmt.Printf("Replacer: %v -> %v\n", s, replacer.Replace(s))

    s = "pid1\tuser1\t  time1   "
    fmt.Printf("Fields: %v -> %v\n", s, strings.Fields(s))
}
