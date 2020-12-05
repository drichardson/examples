fn main() {
    // With separate fields.
    let width = 30;
    let height = 50;

    println!(
        "The area of the rectangle is {} square pixels.",
        area(width, height)
    );

    // With tuples
    let rect = (30, 50);

    println!(
        "The area of the rectangle is {} square pixels.",
        area_tuple(rect)
    );

    let rect = Rectangle {
        width: 30,
        height: 50,
    };
    println!(
        "The area of the rectangle is {} square pixels.",
        area_rectangle(&rect)
    );

    println!("rect is {:?}", rect); // debug print
    println!("rect is {:#?}", rect); // pretty print

    // calling struct methods
    println!("The area of rectangle is {} square pixels.", rect.area());

    // rect.set_width(10); // error: cannot borrow rect as mutable, as it is not declared as
    // mutable

    rect.take_ownership(); // take ownership of immutable rect
                           // println!("The area of rectangle is {} square pixels.", rect.area()); // error: borrow of moved value rect.

    let mut rect = Rectangle {
        width: 20,
        height: 2,
    };
    println!("The area of rectangle is {} square pixels.", rect.area());
    rect.set_width(10);
    println!("The area of rectangle is {} square pixels.", rect.area());

    rect.take_ownership(); // take ownership of mut rect

    // println!("The area of rectangle is {} square pixels.", rect.area()); // error: borrow of
    // moved value rect.

    let rect = Rectangle {
        width: 10,
        height: 5,
    };
    rect.print();

    let smaller = Rectangle {
        width: rect.width / 2,
        height: rect.height,
    };
    println!("can rect hold smaller? {}", rect.can_hold(&smaller));
    println!("can smaller hold rect? {}", smaller.can_hold(&rect));
    println!("can rect hold rect? {}", rect.can_hold(&rect));

    println!("square is {:?}.", Rectangle::square(25));
    println!("square area is {}.", Rectangle::square(100).area());
}

fn area(width: u32, height: u32) -> u32 {
    width * height
}

fn area_tuple(dimensions: (u32, u32)) -> u32 {
    dimensions.0 * dimensions.1
}

#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

// Defining struct methods
impl Rectangle {
    // &self immutable reference
    fn area(&self) -> u32 {
        self.width * self.height
    }

    /* error: duplicate definition with name area
    fn area(&self, u: u32) -> u32 {
        42
    }
    */

    // &mut self mutable reference
    fn set_width(&mut self, new_width: u32) {
        self.width = new_width;
    }

    // take ownership. This is rare, and usually used when the method transorms self into something
    // else and wants to prevent the caller from using the original instance after the
    // transformation. Here we just consume it for demonstration.
    fn take_ownership(self) {
        println!("took ownership of rect {:?}", self);
    }

    fn can_hold(&self, other: &Rectangle) -> bool {
        self.width >= other.width && self.height >= other.height
    }

    // Associated functions. Commonly used for constructors like String::from.
    fn square(size: u32) -> Rectangle {
        Rectangle {
            width: size,
            height: size,
        }
    }
}

// multiple impl blocks are okay
impl Rectangle {
    fn print(&self) {
        println!("print called");
    }
}

fn area_rectangle(rectangle: &Rectangle) -> u32 {
    rectangle.width * rectangle.height
}
