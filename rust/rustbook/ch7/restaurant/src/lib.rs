#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

mod front_of_house {
    pub mod hosting {
        pub fn add_to_waitlist() {}
        fn seat_at_table() {}
    }

    mod serving {
        fn take_order() {}
        fn serve_order() {}
        fn take_payment() {}
    }
}

mod back_of_house {
    pub struct Breakfast {
        pub toast: String,
        seasonal_fruit: String,
    }

    impl Breakfast {
        pub fn summer(toast: &str) -> Breakfast {
            Breakfast {
                toast: String::from(toast),
                seasonal_fruit: String::from("peaches"),
            }
        }
    }

    #[derive(Debug)]
    pub enum Appetizer {
        Soup,
        Salad,
    }
}

//
// bring hosting module into this scope.
//

// absolute use path
// use crate::front_of_house::hosting;

// relative use path. Commented out because you can only bring hosting in once.
// use self::front_of_house::hosting;

// re-expost. Make name available for any code to use
pub use crate::front_of_house::hosting;

pub fn eat_at_restaurant() {
    // absolute path
    crate::front_of_house::hosting::add_to_waitlist();

    // relative path
    front_of_house::hosting::add_to_waitlist();

    // hosting is valid name in this scope due to 'use' above.
    hosting::add_to_waitlist();

    let mut meal = back_of_house::Breakfast::summer("Rye");
    meal.toast = String::from("Wheat");
    println!("I'd like {} toast please", meal.toast);

    // meal.seasonal_fruit = String::from("blueberries"); // error: seasonal_fruit is private

    // error: cannot construct Breafast with struct literal syntax due to inaccessible fields
    /*
    let meal = back_of_house::Breakfast {
        toast: String::from("White"),
    };
    println!("meal={}", meal.toast);
    */

    let order1 = back_of_house::Appetizer::Soup;
    let order2 = back_of_house::Appetizer::Salad;
    println!("order1={:?}, order2={:?}", order1, order2);
}

// Using nested paths to clean up large use lists
use std::{cmp::Ordering, fs};

// two use statements where one is a subpath of the otehr
// use std::io;
// use std::io::Write;
use std::io::{self, Write};

// the glob operator
use std::collections::*;
