enum Coin {
    Penny,
    Nickel,
    Dime,
    Quarter(UsState),
}

#[derive(Debug)]
enum UsState {
    Alaska,
}

fn value_in_cents(coin: Coin) -> u8 {
    match coin {
        Coin::Penny => {
            println!("Lucky penny");
            1
        }
        Coin::Nickel => 5,
        Coin::Dime => 10,
        Coin::Quarter(state) => {
            println!("State quarter from {:?}!", state);
            25
        }
    }
}

fn plus_one(x: Option<i32>) -> Option<i32> {
    match x {
        None => None,
        Some(i) => Some(i + 1),
    }
}

fn main() {
    println!("Penny: {}", value_in_cents(Coin::Penny));
    println!("Nickel: {}", value_in_cents(Coin::Nickel));
    println!("Dime: {}", value_in_cents(Coin::Dime));
    println!(
        "Quarter: {}",
        value_in_cents(Coin::Quarter(UsState::Alaska))
    );

    let five = Some(5);
    let six = plus_one(five);
    let none = plus_one(None);
    println!("five={:?}, six={:?}, none={:?}", five, six, none);

    // the _ placeholder
    let some_u8_value = 0u8;
    match some_u8_value {
        1 => println!("one"),
        3 => println!("three"),
        5 => println!("five"),
        7 => println!("seven"),
        _ => (), // return the unit value, just like println!. All arms must return compatible types.
    };
}
