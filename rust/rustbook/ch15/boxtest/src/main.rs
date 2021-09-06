use crate::List::{Cons, Nil};

fn main() {
    let b = Box::new(5);
    println!("b = {}", b);

    // Use Box to get a recursive type with a known size, otherwise this would fail compilation
    // with "recusirve types has infinite size".
    let list = Cons(1, Box::new(Cons(2, Box::new(Cons(3, Box::new(Nil))))));
    print_values_recursive(&list);
    print_values_iterative(&list);
}

enum List {
    Cons(i32, Box<List>),
    Nil,
}

fn print_values_recursive(list: &List) {
    match list {
        Cons(x, next) => {
            println!("x={}", x);
            print_values_recursive(next);
        }
        Nil => {}
    }
}

fn print_values_iterative(list: &List) {
    //let l: &mut List = list;
    let mut list = list;

    while let Cons(x, next) = list {
        println!("x={}", x);
        list = next;
    }
}
