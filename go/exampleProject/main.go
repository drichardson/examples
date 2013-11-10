package main

import (
	"./pkg1"
	"./pkg2"
	"fmt"
)

func main() {
	fmt.Print("Testing\n")
	pkg1.Testing()
	pkg2.Testing()
	pkg1.Lala()
}
