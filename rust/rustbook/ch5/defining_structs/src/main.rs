struct User {
    username: String,
    email: String,
    sign_in_count: u64,
    active: bool,
}

// Tuple Structs
struct Color(i32, i32, i32);
struct Point(i32, i32, i32);

// Unit-like struct without any fields.
struct MyUnitLike {};

fn main() {
    let user1 = User {
        email: String::from("someone@example.com"),
        sign_in_count: 0,
        active: false,
        username: String::from("example"),
    };

    print_user(&user1);
    // user1.email = String::from("anotheremail@example.com"); // error: user1 is not declared mutable

    let mut user1 = User {
        email: String::from("someone@example.com"),
        sign_in_count: 0,
        active: false,
        username: String::from("example"),
    };

    print_user(&user1);
    user1.email = String::from("anotheremail@example.com"); // error: user1 is not declared mutable
    print_user(&user1);

    let user2 = build_user(String::from("fred@example.com"), String::from("freddy"));
    print_user(&user2);

    let user3 = build_user_shorthand(String::from("philomena@example.com"), String::from("phil"));
    print_user(&user3);

    // LONGHAND: Creating a new user from an existing one, changing a single field.
    let user4 = User {
        email: user1.email,
        username: String::from("anotherusername567"),
        active: user1.active,
        sign_in_count: user1.sign_in_count,
    };
    print_user(&user4);

    // SHORTHAND: Creating a new user from an existing one, changing a single field.
    let user5 = User {
        username: String::from("yetanotherusername567"),
        ..user4
    };
    print_user(&user5);

    /*
    Trying to use user4 again in the same way does not work sincei the value of user4.email has
    already been moved.

    let user6 = User {
        username: String::from("george"),
        ..user4 // error: use of moved value: user4.email
    };
    print_user(&user6);
    */

    let black = Color(0, 0, 0);
    let origin = Point(0, 0, 0);

    print_color(&black);
    // print_color(&origin); // error: mismatched types, even though their fields are the same.
    print_point(&origin);
    // print_point(&black); // error: mismatched types

    // Unit-like struct without any fields.
    struct MyUnitLike {};
    let x = MyUnitLike {};
    print_myunitlike(&x);
}

fn print_myunitlike(myunitlike: &MyUnitLike) {
    println!("myunitlike");
}

fn print_color(c: &Color) {
    println!("color={},{},{}", c.0, c.1, c.2);
}

fn print_point(p: &Point) {
    println!("point={},{},{}", p.0, p.1, p.2);
}

fn print_user(u: &User) {
    println!(
        "user: username={}, sign_in_count={}, active={}, email={}",
        u.username, u.sign_in_count, u.active, u.email
    );
}

fn build_user(email: String, username: String) -> User {
    User {
        email: email,
        username: username,
        active: true,
        sign_in_count: 1,
    }
}

fn build_user_shorthand(email: String, username: String) -> User {
    // Field init shorthand when variables and fields have the same name.
    User {
        email,
        username,
        active: true,
        sign_in_count: 1,
    }
}
