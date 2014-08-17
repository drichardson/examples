package main

import (
	"flag"
	"fmt"
	"strings"
)

func main() {
	var i1 *int = flag.Int("i1", 111, "Use this some way")
	var i2 int64
	flag.Int64Var(&i2, "i2", 222, "Some doc")
	s1 := flag.String("s1", "", "A string")
	var s2 string
	flag.StringVar(&s2, "s2", "something not nil", "More string documentation")
	b1 := flag.Bool("b1", false, "some bool")

	flag.PrintDefaults()

	fmt.Println("PRE-PARSE")
	fmt.Println("non flag Args:", strings.Join(flag.Args(), ","))
	fmt.Println("i1:", i1, *i1)
	fmt.Println("i2:", i2)
	fmt.Println("s1:", s1, *s1)
	fmt.Println("s2:", s2)
	fmt.Println("b1:", b1, *b1)

	fmt.Println("POST-PARSE")
	flag.Parse()
	fmt.Println("non flag Args:", strings.Join(flag.Args(), ","))
	fmt.Println("i1: ", i1, *i1)
	fmt.Println("i2: ", i2)
	fmt.Println("s1:", s1, *s1)
	fmt.Println("s2:", s2)
	fmt.Println("b1:", b1, *b1)
}
