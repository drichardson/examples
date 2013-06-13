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
    AssertBoardValid(b)
    return
}

func AssertBoardValid(b* Board) (bool) {
    // Check rows
    for row := 0; row < 9; row++ {
        m := make(map[int]bool)
        for col := 0; col < 9; col++ {
            val := b.grid[row * 9 + col]
            if _, present := m[int(val)]; present {
                panic(fmt.Sprintf("Given value %d already occurs in row %d", row, val))
            }
            if val < 1 || val > 9 {
                panic(fmt.Sprintf("Invalid value %d at row %d col %d", val, row, col))
            }
            m[int(val)] = true
        }
    }

    // check columns
    for col := 0; col < 9; col++ {
        m := make(map[int]bool)
        for row := 0; row < 9; row++ {
            val := b.grid[row * 9 + col]
            if _, present := m[int(val)]; present {
                panic(fmt.Sprintf("Given value %d already occurs in col %d", row, val))
            }
            if val < 1 || val > 9 {
                panic(fmt.Sprintf("Invalid value %d at row %d col %d", val, row, col))
            }
            m[int(val)] = true
        }
    }

    // check 3x3 squares
    for groupRow := 0; groupRow < 3; groupRow++ {
        for groupCol := 0; groupCol < 3; groupCol++ {
            m := make(map[int]bool)
            groupRowStart := groupRow * 3
            groupColStart := groupCol * 3
            for row := 0; row < 3; row++ {
                for col := 0; col < 3; col++ {
                    val := b.grid[(groupRowStart + row) * 9 + (groupColStart + col)]
                    if _, present := m[int(val)]; present {
                        panic(fmt.Sprintf("Value %d already occurs in group %d, %d", val, groupRow, groupCol))
                    }
                    m[int(val)] = true
                }
            }
        }
    }


    return true
}

func (b* Board) Row(index uint) ([]uint) {
    return b.grid[index * 9 : index * 9 + 9]
}

func (b* Board) String() (string) {
    return fmt.Sprintf("%v\n%v\n%v\n%v\n%v\n%v\n%v\n%v\n%v", b.Row(0), b.Row(1), b.Row(2), b.Row(3), b.Row(4), b.Row(5), b.Row(6), b.Row(7), b.Row(8))
}

