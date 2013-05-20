package main
import "fmt"

func main() {
    var mt1 MyType = 123
    var mt2 MyType2 = 345

    DoIt(mt1);
    DoIt(&mt1);
    DoIt(mt2);
    DoIt(&mt2);
}

func DoIt(mt interface{DoSomething() string}) {
    fmt.Printf("Type %T: %v", mt, mt.DoSomething())
}

type MyType int;
type MyType2 int;

// Note: normally you device methods that take a pointer to the type. The rule about pointers 
// vs. values for receivers is that value methods can be invoked on pointers and values, but
// pointer methods can only be invoked on pointers. This is because pointer methods can modify
// the receiver; invoking them on a copy of the value would cause those modifications to be discarded.
//
// I defined these to take values to demonstrate DoIt using pointer and value arguments.

func (d MyType) DoSomething() string {
    return fmt.Sprintf("Doing Something on MyType %v\n", d)
}

func (d MyType) String() string {
    return fmt.Sprintf("MyTypeString%d", d)
}

func (d MyType2) DoSomething() string {
    return fmt.Sprintf("Doing Something on MyType2 %v\n", d)
}

func (d MyType2) String() string {
    return fmt.Sprintf("MyType2Sting%v", d)
}


