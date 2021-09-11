use std::thread;
use std::time::Duration;

fn main() {
    let handle = thread::spawn(|| {
        for i in 1..15 {
            println!("hi number {} from the spawned thread!", i);
            thread::sleep(Duration::from_millis(1));
        }
    });

    // use move keyword to force closure to take ownership of captured variables.
    let v = vec![1, 2, 3];

    let handle2 = thread::spawn(move || {
        println!("Here's a vector: {:?}", v);
    });

    for i in 1..5 {
        println!("hi number {} from the main thread!", i);
        thread::sleep(Duration::from_millis(1));
    }

    handle.join().unwrap();
    handle2.join().unwrap();

    println!("complete");
}
