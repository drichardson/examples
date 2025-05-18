// idiomatic to specify full path for structs and enums.
use std::collections::HashMap;

fn main() {
    let mut map = HashMap::new();
    map.insert(1, 2);

    println!("map={:?}", map);
}
