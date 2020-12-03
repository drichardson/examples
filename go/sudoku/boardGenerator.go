package main

import (
    "fmt"
    "./sudoku"
)

func main() {
    b := sudoku.NewStandardBoard()
    fmt.Printf("NewStandardBoard is\n%v\n", b)

    b = sudoku.NewRandomBoard()
    fmt.Printf("NewRandomBoard is:\n%v\n", b)

    b = sudoku.NewRandomBoardByPartialSolutionAlgorithm()
    fmt.Printf("NewRandomBoardByPartialSolutionAlgorithm is:\n%v\n", b)
}

