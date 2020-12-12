fn main() {
    let v: Vec<i32> = Vec::new();
    println!("v={:?}", v);

    // using the vec! macro. Type inferred.
    let v = vec![1, 2, 3];
    println!("v={:?}", v);

    let mut v = Vec::new();
    v.push(5);
    v.push(6);
    v.push(7);
    v.push(8);
    println!("v={:?}", v);

    // Dropping a vector drops it's elements
    {
        let v = vec![1, 2, 3, 4];
        println!("v={:?}", v);
    } // <- v goes out of scope and is freed here

    let v = vec![1, 2, 3, 4, 5];
    let third: &i32 = &v[2];
    println!("The third element is {}", third);

    match v.get(2) {
        Some(third) => println!("The third element is {}", third),
        None => println!("There is no third elements."),
    }

    // two ways to get items from a vector: &[] and .get
    // let does_not_exist = &v[100]; // will crash with 'main panicked at index is out of bounds'
    let does_not_exist = v.get(100); // returns None.
    print!("does_not_exist={:?}", does_not_exist);

    let mut v = vec![1, 2, 3, 4, 5];
    let first = &v[0];
    // v.push(6); // error: cannot borrow 'v' as mutable because it is also borrowed as immutable
    println!("The first elemetn is: {}", first);
}
