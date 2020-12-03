fn main() {
    let mut s = String::from("hello");

    s.push_str(", world!"); // append a literal to a String

    println!("{}", s);

    let s1 = String::from("hello");
    let s2 = s1;
    // println!("s1={}", s1); // compile error: borrowed value after move
    println!("s2={}", s2);

    let s1 = String::from("hello");
    let s2 = s1.clone();
    println!("s1={}, s2={}", s1, s2);

    // ownership and functions
    let s = String::from("hello");
    takes_ownership(s);
    // println!("after takes_ownership={}", s); // compile error: borrow of moved value s

    let x = 5;
    makes_copy(x);
    println!("after makes_copy={}", x);

    // Return values
    let s1 = gives_ownership();
    let s2 = String::from("hello");
    let s3 = takes_and_gives_back(s2);
    println!("after takes_and_gives_back: s1={}", s1);
    // println!("after takes_and_gives_back: s2={}", s2); // compile error: borrow of moved value
    println!("after takes_and_gives_back: s3={}", s3);

    let s1 = String::from("hello");
    let (s2, len) = calculate_length(s1);
    println!("The length of '{}' is {}.", s2, len);
}

fn takes_ownership(some_string: String) {
    println!("some_string={}", some_string);
}

fn makes_copy(some_integer: i32) {
    println!("some_integer={}", some_integer);
}

fn gives_ownership() -> String {
    let some_string = String::from("hello");
    some_string
}

fn takes_and_gives_back(a_string: String) -> String {
    a_string
}

fn calculate_length(s: String) -> (String, usize) {
    let length = s.len();
    (s, length)
}
