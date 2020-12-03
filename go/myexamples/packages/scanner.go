package main

import (
	"fmt"
	"os"
	"text/scanner"
)

func main() {
	var s scanner.Scanner
	s.Init(os.Stdin)
	//s.Mode = scanner.ScanInts
	for tok := s.Scan(); tok != scanner.EOF; tok = s.Scan() {
		if s.Position.IsValid() {
			fmt.Printf("TOKEN: %v\n", s.TokenText())
		} else {
			fmt.Printf("INVALID TOKEN AT: %v\n", s.Position)
		}
	}
}
