package main

import (
    "fmt"
    "sort"
)

func main() {
    b := NewRandomBoard()
    fmt.Printf("Board is:\n%v\n", b)
}


type Board struct {
    grid []uint
} 


func NewBoard() (b *Board) {
    b = new(Board)
    b.grid = make([]uint, 9*9)
    order := [9]uint{0, 3, 6, 1, 4, 7, 2, 5, 8}
    for i, rowIndex := range order {
        row := b.Row(rowIndex)
        rowCounter := i + 1
        for pos := 0; pos < 9; pos++  {
            row[pos] = uint(rowCounter);
            rowCounter++
            if ( rowCounter > 9 ) {
                rowCounter = 1
            }
        }
    }
    return
}

func squareIndexForRowAndColumn(rowIndex uint, columnIndex uint) (uint) {
    return rowIndex * 9 + columnIndex
}

func (b* Board) validValuesAtRowColumn(row uint, col uint) ([]int) {

    // Assume all values are valid
    validValues := make(map[int]bool, 9);
    for i := 1; i <= 9; i++ {
        validValues[i] = true
    }


    // Remove any values that already appear in the row
    for i := uint(0); i < 9; i++ {
        delete(validValues, int(b.grid[row * 9 + i]))
    }

    // Remove any values that already appear in the column
    for i := uint(0); i < 9; i++ {
        delete(validValues, int(b.grid[i * 9 + col]))
    }

    // Remove any values that already appear in the 3x3 group this square is part of
    topLeftSquareInGroup := (row / 3) * 3 + (col / 3) * 3
    fmt.Printf("Top left square is %v, %v, %v\n", topLeftSquareInGroup, (row / 3) * 3, (col / 3) * 3)
    for iRow := uint(0); iRow < 3; iRow++ {
        for iCol := uint(0); iCol < 3; iCol++ {
            fmt.Printf("Removing square value %v because (%v, %v) = (%v, %v)\n", b.grid[iRow * 9 + iCol + topLeftSquareInGroup], iRow, iCol, iRow * 9, iCol + topLeftSquareInGroup)
            delete(validValues, int(b.grid[iRow * 9 + iCol + topLeftSquareInGroup]))
        }
    }

    // Turn the values map into an array of valid values.
    values := make([]int, len(validValues))
    counter := 0
    for key, value := range validValues {
        if value {
            values[counter] = key
            counter++
        }
    }

    sort.Ints(values)
    return values
}

func NewRandomBoard() (b* Board) {

    b = new(Board)
    b.grid = make([]uint, 9*9)

    // For each row in the board
    for rowIndex := uint(0); rowIndex < 9; rowIndex++ {

        row := b.Row(rowIndex)

        // For each column in the row
        for col := uint(0); col < 9; col++ {

            // Get an array of valid values
            validValues := b.validValuesAtRowColumn(rowIndex, col)
            fmt.Printf("Valid values for %v, %v = %v\n", rowIndex, col, validValues)

            // Randomly select one
            if len(validValues) == 0 {
                fmt.Printf("Can't find value to select from for %v, %v for board\n%v\n", rowIndex, col, b)
                panic("No valid values to select from")
            }

            row[col] = uint(validValues[0])
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

