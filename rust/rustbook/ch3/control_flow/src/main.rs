fn main() {
    //
    // if expressions
    //

    // Condition must be a bool.
    let number = 17 * 5 * 3;
    if number % 4 == 0 {
        println!("divisible by 4");
    } else if number % 3 == 0 {
        println!("divisible by 3");
    } else if number % 2 == 0 {
        println!("divisible by 2");
    } else {
        println!("not divisble by 4, 3, or 2");
    }

    let condition = true;
    let number = if condition { 5 } else { 6 };
    println!("The value of number is: {}", number);

    // let number = if condition { 5 } else { "six" }; // compile error: incompatible types

    //
    // loops
    //

    // infinite loop
    /*
    loop {
        println!("again!");
    }
    */

    // Returning values from loops
    let mut counter = 0;
    let result = loop {
        counter += 1;

        if counter == 10 {
            break counter * 2;
        }
    };
    println!("The result is {}", result);

    // Conditional loop with while
    let mut number = 3;
    while number != 0 {
        println!("{}!", number);
        number -= 1;
    }

    println!("LIFTOFF!!!");

    let a = [10, 20, 30, 40, 50];

    // Looping through each element of a collection using a while loop.
    let mut index = 0;
    while index < 5 {
        println!("the value is: {}", a[index]);
        index += 1;
    }

    // Looping through each element of a collection using a for loop.
    for element in a.iter() {
        println!("the value is: {}", element);
    }

    // Countdown using Range
    for number in (1..4).rev() {
        println!("{}!", number);
    }
    println!("LIFTOFF!!!");
}
