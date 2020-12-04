fn main() {
    // Find first word, without using slices.
    let first = first_word_noslice(&String::from("one two three"));
    println!("first={}", first);

    // slice examples
    let s = String::from("hello");
    let slice = &s[0..2];
    println!("slice={}", slice);
    let slice = &s[..2];
    println!("slice={}", slice);
    let slice = &s[3..s.len()];
    println!("slice={}", slice);
    let slice = &s[3..];
    println!("slice={}", slice);

    // Find first word again, this time using slices.
    let s = String::from("one two three");
    let first = first_word_slice(&s);
    println!("first={}", first);

    // string literals are slices
    let s = "Hello, world!"; // s is type &str
    println!("first={}", first_word_slice2(s));
    let s = String::from("one two three");
    println!("first={}", first_word_slice2(&s[..])); // can also pass string as slice

    // other (non-String) slices
    let a = [1, 2, 3, 4, 5];
    let slice = &a[1..3];
    println!("a[0]={}, slice[0]={}", a[0], slice[0]);
    let slice: &[i32] = &a[1..3];
    println!("a[0]={}, slice[0]={}", a[0], slice[0]);
}

fn first_word_noslice(s: &String) -> usize {
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return i;
        }
    }

    s.len()
}

fn first_word_slice(s: &String) -> &str {
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[0..i];
        }
    }

    &s[..]
}

fn first_word_slice2(s: &str) -> &str {
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[0..i];
        }
    }

    &s[..]
}
