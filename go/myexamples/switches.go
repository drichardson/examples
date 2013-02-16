package main

import (
	"fmt"
)

func main() {
	switchWithExpression("doug")
	switchWithExpression("rebecca")
	switchWithExpression("something else")
	
	switchWithExpression2(-123)
	switchWithExpression2(4122)
	switchWithExpression2(0)
	
	switchWithoutExpression("doug")
	switchWithoutExpression("rebecca")
	switchWithoutExpression("something else")
	
	switchOnType("a string")
	switchOnType(123)
	switchOnType(false)
	switchOnType(123.32)
}

func switchWithExpression(s string) {
	switch s {
	case "doug":
		fmt.Println("doug case")
	case "rebecca":
		fmt.Println("rebecca case")
	default:
		fmt.Println("default case")
	}
}

func switchWithExpression2(i int) {
	switch {
	case i < 0:
		fmt.Println("negative", i)
	case i > 0:
		fmt.Println("positive", i)
	case true:
		fmt.Println("zero", i)
	}
}


func switchWithoutExpression(s string) {
	switch {
	default:
		fmt.Println("s is something else")
	case s == "doug" || s == "rebecca":
		fmt.Println("s is doug or rebecca")
	}
}

func switchOnType(interfaceValue interface{}) {
	switch t := interfaceValue.(type) {
	default:
		fmt.Printf("Unexpected type %T: %v\n", t, t)
	case int:
		fmt.Printf("int value of %v\n", t)
	case bool:
		fmt.Printf("bool value of %v\n", t)
	case string:
		fmt.Printf("string value of %v\n", t)
	}
}
