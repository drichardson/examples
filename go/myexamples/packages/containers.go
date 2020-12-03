package main

import (
    "fmt"
    "container/heap"
    "container/list"
    "container/ring"
    "math"
)

func main() {

    testHeap()
    testList()
    testRing()
}

type IntHeap []int;
func (h IntHeap) Len() int { return len(h) }
func (h IntHeap) Less(i, j int) bool { return h[i] < h[j] }
func (h IntHeap) Swap(i, j int) { h[i], h[j] = h[j], h[i] }
func (h *IntHeap) Push(x interface{}) {
    *h = append(*h, x.(int))
}
func (h *IntHeap) Pop() interface{} {
    old := *h
    n := len(old)
    x := old[n-1]
    *h = old[0 : n-1]
    return x
}

func testHeap() {
    h := &IntHeap{2, 1, 5}
    heap.Init(h)
    heap.Push(h, 3)
    for h.Len() > 0 {
        fmt.Printf("%d ", heap.Pop(h))
    }
    fmt.Printf("\n")
}

func testList() {
    l := list.New()
    l.PushBack(1)
    l.PushBack("some string")
    l.PushBack(math.Pi)
    l.PushBack(complex(1.2, 3.4))
    l.PushFront("first!")

    fmt.Println("Front to Back\n----------------")
    for e := l.Front(); e != nil; e = e.Next() {
        fmt.Printf("e is %v\n", e.Value)
    }

    fmt.Println("Back to Front\n----------------")
    for e := l.Back(); e != nil; e = e.Prev() {
        fmt.Printf("e is %v\n", e.Value)
    }

}

func testRing() {
    // Setup route 1
    busRoute1 := ring.New(4)
    busRoute1.Value = "Mary"
    busRoute1 = busRoute1.Move(1)
    busRoute1.Value = "El Camino"
    busRoute1 = busRoute1.Move(1)
    busRoute1.Value = "De Anza"
    busRoute1 = busRoute1.Move(1)
    busRoute1.Value = "Fair Oaks"
    busRoute1 = busRoute1.Move(1)

    fmt.Printf("Bus Route 1 has %v stops\n", busRoute1.Len())
    visitor := func(v interface{}) {
        s := v.(string)
        fmt.Printf("Visiting bus stop %v\n", s)
    }
    busRoute1.Do(visitor)

    // Setup route 2
    busRoute2 := ring.New(2)
    busRoute2.Value = "Freemont"
    busRoute2 = busRoute2.Move(1)
    busRoute2.Value = "Union City"
    busRoute2 = busRoute2.Move(1)
    fmt.Printf("Bus Route 2 has %v stops\n", busRoute2.Len())
    busRoute2.Do(visitor)


    // Connect the routes
    busRoute1.Link(busRoute2)
    fmt.Printf("Linked Routes len is %v\n", busRoute1.Len())
    busRoute1.Do(visitor)
    fmt.Printf("Linked Routes from busRoute2's perspective is len %v\n", busRoute2.Len())
    busRoute2.Do(visitor)


}

