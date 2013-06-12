package main

import (
    "fmt"
)

func main() {
    b := NewBoard()
    fmt.Printf("Board is:\n%v\n", b)
}


type Board struct {
    grid []uint
} 


func NewBoard() (b *Board) {
    b = new(Board)
    b.grid = make([]uint, 9*9)
    for rowIndex := uint(0); rowIndex < 9; rowIndex++ {
        row := b.Row(rowIndex)
        rowCounter := rowIndex + 1
        for pos := 0; pos < 9; pos++  {
            row[pos] = rowCounter;
            rowCounter++
            if ( rowCounter > 9 ) {
                rowCounter = 0
            }
        }
    }
    return
}

func (b* Board) Row(index uint) ([]uint) {
    return b.grid[index * 9 : index * 9 + 9]
}

func (b* Board) String() (string) {
    return fmt.Sprintf("%v\n%v\n%v\n%v\n%v\n%v\n%v\n%v\n%v", b.Row(0), b.Row(1), b.Row(2), b.Row(3), b.Row(4), b.Row(5), b.Row(6), b.Row(7), b.Row(8))
}

