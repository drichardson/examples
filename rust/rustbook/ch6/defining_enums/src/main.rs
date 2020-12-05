#[derive(Debug)]
enum IpAddrKind {
    V4(u8, u8, u8, u8),
    V6(String),
}

#[derive(Debug)]
enum Message {
    Quit,                       // no data associated
    Move { x: i32, y: i32 },    // anonymouse struct
    Write(String),              // single String
    ChangeColor(i32, i32, i32), // 3 i32 values
}

// Define methods on enums
impl Message {
    fn call(&self) {
        println!("call called on {:?}", self);
    }
}

fn main() {
    let loopback_four = IpAddrKind::V4(127, 0, 0, 1);
    let loopback_six = IpAddrKind::V6(String::from("::1"));

    route(loopback_four);
    route(loopback_six);

    let m = Message::Write(String::from("hello"));
    m.call();
    let m = Message::Move { y: 32, x: 0 };
    m.call();
    let m = Message::ChangeColor(1, 2, 3);
    m.call();
    let m = Message::Quit;
    m.call();

    // Using Option from the standard librar. It's included in the prelude so you don't have to
    // explicitly include it.
    let some_number = Some(5);
    let some_string = Some("a string");
    let absent_number: Option<i32> = None;

    /*
    println!("some_number: {}", some_number);
    println!("some_string: {}", some_string);
    println!("absent_number: {}", absent_number);
    */

    let x: i8 = 5;
    let y: Option<i8> = Some(5);
    // let sum = x + y; // error: cannot add Option<i8> to i8.
}

fn route(ip_kind: IpAddrKind) {
    println!("route called on {:?}", ip_kind);
}
