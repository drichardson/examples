package main

import (
	"fmt"
	"reflect"
)

type thing struct {
	i1 int "this is i1"
	// use the convention as given by
	i2 int `key1:"value1" key2:"value2" testkey:"value3"`
}

func main() {
	var t thing
	dumpStructInfo(t)
}

func dumpStructInfo(i interface{}) {
	t := reflect.TypeOf(i)
	fmt.Println("Struct", t.Name())
	for i := 0; i < t.NumField(); i++ {
		f := t.Field(i)
		fmt.Println(i, f.Name, f.Tag, ", has testkey? ", f.Tag.Get("testkey"))

	}
}
