package main

import (
    "fmt"
    "reflect"
)

type MyStruct struct {
    i int
    s string `myTag1:"value1" myTag2:"value 2" myTag3`
}

func main() {
    m := MyStruct{100, "example"}
    m.InspectFields()
}

func (ms MyStruct) InspectFields() {
    t := reflect.TypeOf(ms)
    structField, ok := t.FieldByName("s")
    if !ok {
        panic("Error getting struct field 's'")
    }
    fmt.Printf("Struct field 's' has tag %v\n", structField.Tag)
    fmt.Printf("Struct field 's' has tag name 1 = %v, 2 = %v, 3 = %v\n", structField.Tag.Get("myTag1"), structField.Tag.Get("myTag2"), structField.Tag.Get("myTag3"))
}

