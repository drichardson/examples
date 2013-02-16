package main

import "fmt"

func main() {
	buf := [3]int{0, 1, 2}
	fmt.Println("buf:",buf)
	
	var slice []int
	
	slice = buf[:]
	fmt.Println("entire:", slice)
	
	slice = buf[0:]
	fmt.Println("0:", slice)
	
	slice = buf[1:]
	fmt.Println("1:", slice)
	
	slice = buf[1:1]
	fmt.Println("1:1", slice)
	
	slice = buf[1:2]
	fmt.Println("1:2", slice)
	
	slice = buf[1:3]
	fmt.Println("1:3", slice)
	
	slice = buf[0:3]
	fmt.Println("0:3", slice)
}
