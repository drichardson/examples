package main

import (
	"fmt"
)

func main() {
	var p1 *int = new(int)
	p2 := new(int)

	fmt.Println("p1", p1, *p1)
	*p1 = 123
	fmt.Println("p1", p1, *p1)

	fmt.Println("p2", p2, *p2)

	var m1 map[string]int = map[string]int{}
	fmt.Println("m1", m1["howdy"])
	m1 = map[string]int{ "howdy": 123 }
	fmt.Println("m1", m1["howdy"])
	m1["howdy2"] = 111
	fmt.Println("m1", m1["howdy2"])

	m1 = make(map[string]int)
	fmt.Println("m1 make", m1)
	m1["howdy"] = 314
	fmt.Println("m1 make 2", m1)
}

