use std::collections::HashMap;

fn main() {
    println!("Hello, world!");

    let mut scores = HashMap::new();

    //scores.insert("Blue", 10);
    //scores.insert("Hi", 20);
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    let teams = vec![String::from("Blue"), String::from("Yello")];
    let initial_scores = vec![10, 50];

    // Need to specify HashMap for collect, but key and value types can be inferred.
    let mut _scores: HashMap<_, _> = teams.into_iter().zip(initial_scores.into_iter()).collect();

    let mut scores: HashMap<i32, i32> = HashMap::new();
    scores.insert(100, 10);
    scores.insert(200, 20);
    // println!("scores={}", scores);

    for (key, value) in &scores {
        println!("{}: {}", key, value);
    }

    println!("");

    scores.entry(100).or_insert(50);
    scores.entry(300).or_insert(60);
    for (key, value) in &scores {
        println!("{}: {}", key, value);
    }

    println!("scores={:?}", scores);

    let text = "hello world wonderful world";

    let mut map = HashMap::new();

    for word in text.split_whitespace() {
        let count = map.entry(word).or_insert(0);
        *count += 1;
    }

    println!("map={:?}", map);
}
