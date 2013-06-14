package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    b := NewStandardBoard()
    fmt.Printf("NewStandardBoard is\n%v\n", b)

    b = NewRandomBoard()
    fmt.Printf("NewRandomBoard is:\n%v\n", b)

    b = NewRandomBoardByPartialSolutionAlgorithm()
    fmt.Printf("NewRandomBoardByPartialSolutionAlgorithm is:\n%v\n", b)
}


type Board struct {
    grid []uint
}

func NewZeroBoard() (b *Board) {
    b = new(Board)
    b.grid = make([]uint, 9*9)
    return
}

func NewStandardBoard() (b *Board) {
    b = NewZeroBoard()
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

func randomRowsOrColumnsInOneGroup(r *rand.Rand) (int, int) {
    row1 := r.Intn(9)
    group1 := row1 / 3
    var row2 int
    for row2 = row1; row2 == row1; row2 = (group1*3) + r.Intn(3) {
    }
    return row1, row2
}

func NewRandomBoard() (b *Board) {
    b = NewStandardBoard()

    r := rand.New(rand.NewSource(time.Now().UnixNano()))

    for count := 0; count < 100; count++ {
        b.SwapRows(randomRowsOrColumnsInOneGroup(r))
        b.SwapColumns(randomRowsOrColumnsInOneGroup(r))
    }

    return
}

func NewRandomBoardByPartialSolutionAlgorithm() (b *Board) {
    b = NewZeroBoard()

    r := rand.New(rand.NewSource(time.Now().UnixNano()))

    // Randomly fill in groups (0,0), (1,1), and (2,2). They don't depend on each other at this point so we can
    // fill them with any values.
    b.RandomlyFillGroup(r,0,0)
    b.RandomlyFillGroup(r,1,1)
    b.RandomlyFillGroup(r,2,2)


    return
}

// TODO: Figure out how to use an array[]interface{}, since you don't care about the type of the elements in the array.
func shuffleArray(array []uint, r *rand.Rand) {
    for i, l := 0, len(array); i < l; i++ {
        swapIndex := r.Intn(l)
        swap := array[swapIndex]
        array[swapIndex] = array[i]
        array[i] = swap
    }
}

func (b *Board) RandomlyFillGroup(r *rand.Rand, groupRow int, groupCol int) {
    if !(groupRow >= 0 && groupRow <= 2 && groupCol >= 0 && groupCol <= 2) {
        panic(fmt.Sprintf("Invalid group row %v or column %v. Should be between 0, 1, or 2.", groupRow, groupCol))
    }

    group := []uint{1, 2, 3, 4, 5, 6, 7, 8, 9}
    shuffleArray(group, r)
    startRow := groupRow * 3
    startCol := groupCol * 3
    start := startRow * 9 + startCol
    end := start + 3
    copy(b.grid[start:end], group[0:3])
    copy(b.grid[start+9:end+9], group[3:6])
    copy(b.grid[start+18:end+18], group[6:9])
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


func AssertInSameRowOrColumnGroup(row1 int, row2 int) {
    if row1 >= 0 && row1 <= 2 {
        if row2 < 0 || row2 > 2 {
            panic("Rows must be in same group row")
        }
    } else if row1 >= 3 && row1 <= 5 {
        if row2 < 3 || row2 > 5 {
            panic("Rows must be in same group row 2")
        }
    } else if row1 >= 6 && row1 <= 8 {
        if row2 < 6 || row2 > 8 {
            panic("Rows must be in same group row 3")
        }
    } else {
        panic("Invalid rows")
    }
}

func (b* Board) SwapColumns(column1 int, column2 int) {
    AssertInSameRowOrColumnGroup(column1, column2)

    for row := 0; row < 9; row++ {
        col1Index := row * 9 + column1
        col2Index := row * 9 + column2
        tmp := b.grid[col1Index]
        b.grid[col1Index] = b.grid[col2Index]
        b.grid[col2Index] = tmp
    }
}

func (b* Board) SwapRows(row1 int, row2 int) {
    AssertInSameRowOrColumnGroup(row1, row2)

    tmpRow := make([]uint, 9)
    r1 := b.grid[row1 * 9 : row1 * 9 + 9]
    r2 := b.grid[row2 * 9 : row2 * 9 + 9]
    copy(tmpRow, r1)
    copy(r1, r2)
    copy(r2, tmpRow)
}

func (b* Board) String() (string) {
    return fmt.Sprintf("%v\n%v\n%v\n%v\n%v\n%v\n%v\n%v\n%v", b.Row(0), b.Row(1), b.Row(2), b.Row(3), b.Row(4), b.Row(5), b.Row(6), b.Row(7), b.Row(8))
}

