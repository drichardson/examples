use crate::List::{Cons, Nil};
use std::rc::Rc;

fn main() {
    let a = Rc::new(Cons(5, Rc::new(Cons(10, Rc::new(Nil)))));
    println!("count after creating a = {}", Rc::strong_count(&a));
    let b = Cons(3, Rc::clone(&a));
    println!("count after creating b = {}", Rc::strong_count(&a));
    {
        let c = Cons(4, Rc::clone(&a));
        println!("count after creating c = {}", Rc::strong_count(&a));
        print_values_recursive(&a);
        print_values_iterative(&a);
        print_values_recursive(&b);
        print_values_iterative(&b);
        print_values_recursive(&c);
        print_values_iterative(&c);
    }
    println!("count after c goes out of scope = {}", Rc::strong_count(&a));
}

enum List {
    Cons(i32, Rc<List>),
    Nil,
}

fn print_values_recursive(list: &List) {
    match list {
        Cons(x, next) => {
            print!("{} ", x);
            print_values_recursive(next);
        }
        Nil => {
            println!("");
        }
    }
}

fn print_values_iterative(list: &List) {
    //let l: &mut List = list;
    let mut list = list;

    while let Cons(x, next) = list {
        print!("{} ", x);
        list = next;
    }
    println!("");
}
