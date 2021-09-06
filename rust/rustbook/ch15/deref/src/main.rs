fn main() {
    let x = 5;
    let y = &x;

    assert_eq!(5, x);
    assert_eq!(5, *y); // dereference y, otherwise compile error can't compare {integer} with &{integer}

    let y = Box::new(x);
    assert_eq!(5, *y); // Using dereference operator on a Box<i32>.

    println!("done");

    let y = MyBox::new(x);
    assert_eq!(5, *y); // compiler substitutes *(y.deref()) for *y here

    //
    // Implicit Deref Coercions with Functions and Methods
    //
    let m = MyBox::new(String::from("Rust"));
    // hello(m); // error: expected &str but found struct MyBox
    hello(&m); // implicit deref conversion to &str: MyBox<String> => &String (via MyBox<T>::deref()) => &str (via String::deref).

    // same thing without deref coercion
    hello(&((*m)[..]));
}

// MyBox demonstrates how to implement Deref. Note it is very different than Box, since this does
// not allocate on the heap.
struct MyBox<T>(T);

impl<T> MyBox<T> {
    fn new(x: T) -> MyBox<T> {
        MyBox(x)
    }
}

impl<T> std::ops::Deref for MyBox<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

fn hello(name: &str) {
    println!("Hello, {}!", name);
}
