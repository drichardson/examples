fn main() {
    let s = String::new();
    println!("s={}", s);

    let data = "initial contents";
    let s = data.to_string();
    println!("s={}", s);

    let s = "initial contents".to_string();
    println!("s={}", s);

    let s = String::from("initial contents");
    println!("s={}", s);

    for hello in vec![
        String::from("السلام عليكم"),
        String::from("Dobrý den"),
        String::from("Hello"),
        String::from("שָׁלוֹם"),
        String::from("नमस्ते"),
        String::from("こんにちは"),
        String::from("안녕하세요"),
        String::from("你好"),
        String::from("Olá"),
        String::from("Здравствуйте"),
        String::from("Hola"),
    ] {
        println!("hello={}", hello);
    }

    let mut s = String::from("foo");
    s.push_str("bar");
    println!("s={}", s);
    s.push('l');
    println!("s={}", s);

    let s1 = String::from("Hello, ");
    let s2 = String::from("world!");
    let s3 = s1 + &s2;
    // println!("s1={}", s1); // error s1 value borrowed here after move
    println!("s2={}", s2);
    println!("s3={}", s3);

    let hello = "Здравствуйте";
    // let answer = &hello[0]; // won't compile, need to use different syntax to specify byte,
    // scalar value, or grapheme index.

    let s = &hello[0..4]; // first 4 bytes of string
    println!("s={}", s); // will panic if s doesn't end on a char boundary.

    let s = String::from("नमस्ते");
    println!("s={}, len={}", s, s.len());

    for c in s.chars() {
        println!("c={}", c);
    }

    for b in s.bytes() {
        println!("b={}", b);
    }
}
