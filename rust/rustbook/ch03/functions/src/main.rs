fn main() {
    println!("Hello, world!");

    another_function(5, 2);

    // Expressions

    let y = {
        let x = 3;
        x + 1 // expressions do not include semi-colons
    };

    println!("The value of y is: {}", y);

    println!("five={}", five());
    println!("six={}", six());

    println!("the value of plus_one(100) is {}", plus_one(100));
}

fn another_function(x: i32, y: i32) {
    println!("The value of x is {}, y is {}", x, y);
}

fn five() -> i32 {
    5
}

fn six() -> i32 {
    return 6;
}

fn plus_one(x: i32) -> i32 {
    x + 1
}
