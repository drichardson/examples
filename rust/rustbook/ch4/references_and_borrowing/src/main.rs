fn main() {
    let s1 = String::from("hello");

    let len = calculate_length(&s1);

    println!("The length of '{}' is {}.", s1, len);

    let mut s1 = String::from("hello");
    change(&mut s1);
    println!("s1 is now {}", s1);

    let r1 = &mut s1;
    // let r2 = &mut s1; // compiler error: cannot borrow s1 as mutable more than once at a time

    println!("{}", r1);

    // Can have multiple mutable references at different times by using scopes.
    let mut s1 = String::from("hello");
    {
        let r1 = &mut s1;
        println!("{}", r1);
    }

    // this is okay, r2 not used after s1 assigned to r3
    let r2 = &mut s1;
    println!("{}", r2);
    let r3 = &mut s1;
    println!("{}", r3);

    // this is not okay, r2 used after s1 assigned to r3.
    let r2 = &mut s1;
    // let r3 = &mut s1; // cannot borrow s1 as mutable more than once at a time
    println!("{}", r2);
    // println!("{}", r3);

    let mut s = String::from("hello"); // warning: variable does not need to be mutable... just for this example.
    let r1 = &s; // immutable reference
    let r2 = &s; // immutable reference, okay
                 // let r3 = &mut s; // compiler error: cannot borrow s as mutable because it is also borrowed as immutable
    println!("{} and {}", r1, r2);

    // Dangling References
    // let reference_to_nothing = dangle();
    let _reference_to_something = no_dangle();

    //
    // Question: is there anyway to get a 2 references (mutable or non mutable) to a mutable?
    //

    // try to use a mutable after taking a reference
    let mut i1 = 0;
    i1 += 2;
    let r1 = &i1;
    // i1 += 1; // error: cannot assign to i1 because it is borrowed
    // r1 += 1; // binary assignment += cannot be applied to type &{interger}.
    // *r1 += 1; // cannot assign to *1 which is behind a & reference.
    println!("r1={}", r1);

    // try to use a mutable after taking a mutable ference
    let mut i1 = 0;
    i1 += 2;
    let r1 = &mut i1;
    // i1 += 2; // cannot use i1 because it was mutable borrowed
    *r1 += 2;
    println!("r1={}", r1);

    // Try to use immutable after borrowing mutable.
    let i1 = 0;
    // i1 += 2; // error: cannot assign twice to immutable variable.
    // let r1 = &mut i1; // cannot borrow i1 as mutable, as it is not declared as mutable
}

fn calculate_length(s: &String) -> usize {
    s.len()
}

fn _change_broken(_some_string: &String) {
    // some_string.push_str(", world"); // compiler error: cannot borrow some_string as mutable, it is behind a & reference
}

fn change(some_string: &mut String) {
    some_string.push_str(", world");
}

/*
fn dangle() -> &String {
    // compiler error: this function's return type contains a borrowed value, but there is no value for it to be borrowed from.
    let s = String::from("hello");

    &s
}
*/

fn no_dangle() -> String {
    let s = String::from("hello");
    s
}
