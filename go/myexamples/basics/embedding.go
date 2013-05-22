package main

import "fmt"

type Point struct {
    x, y float32
}

type Size struct {
    width, height float32
}

type Rect struct {
    Point
    Size
}

func main() {
    p := &Point{x:400, y:105}
    fmt.Printf("Point %v\n", p)

    s := &Size{width:10, height: 40}
    fmt.Printf("Size: %v\n", s)

    r := &Rect{Point:*p, Size: *s}
    fmt.Printf("Rect: %v\n", r)
}

func (p* Point) String() string {
    return fmt.Sprintf("%f,%f", p.x, p.y)
}

func (s* Size) String() string {
    return fmt.Sprintf("%f,%f", s.width, s.height)
}

func (r* Rect) String() string {
    return fmt.Sprintf("point={%v},size={%v}", r.(Point), r.Size)
}
