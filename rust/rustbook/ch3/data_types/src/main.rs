fn main() {
    //
    // Type Annotations
    //
    // compiler error without type annotation
    let _guess: u8 = "200".parse().expect("not a number");

    //
    // Non-wrapping arithmetic
    //

    // integer overflow panics in debug builds unless you use Wrapping.
    let _i: u8 = 100;
    // let _j: u8 = _i + 200; // compiler error: this arithmetic operation will overflow
    // let _j2: u8 = _i + _guess; // compiles, but runtime arithmetic overflow panic
    let j3 = _i.wrapping_add(_guess); // allows you to do C style overflow arithmetic

    println!("j3={}", j3);

    //
    // Tuples
    //

    let tup = (500, 6.4, 1);
    let (x, y, z) = tup;
    println!("tup.1={}, y={}", tup.1, y);

    let x: (i32, f64, u8) = (500, 6.4, 1);
    println!("tup={},{},{}", x.0, x.1, x.2);

    //
    // Arrays
    //
    let a = [1, 2, 3, 4, 5];
    print!("a>");
    for x in a.iter() {
        print!("{} ", x);
    }
    println!("");

    let a: [i32; 5] = [1, 2, 3, 4, 5];
    print!("a>");
    for x in a.iter() {
        print!("{} ", x);
    }
    println!("");

    let a = [8; 3]; // [8, 8, 8]
    print!("a>");
    for x in a.iter() {
        print!("{} ", x);
    }
    println!("");

    // println!("a={}", a[4]); // compiler error: out of bounds
}
