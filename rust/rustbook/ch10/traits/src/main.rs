fn main() {
    println!("Hello, world!");

    // use traits::{NewsArticle, Summary, Tweet};
    use traits::*;

    let tweet = Tweet {
        username: String::from("horse_ebooks"),
        content: String::from("of course, as you probably already know, people"),
        reply: false,
        retweet: false,
    };

    println!("1 new tweet: {}", tweet.summarize());

    let news_article = NewsArticle {
        headline: String::from("Penguins win the Stanley Cup"),
        location: String::from("Pittsburgh"),
        author: String::from("Iceburgh"),
        content: String::from("The Penguins once again win."),
    };

    println!("New article available! {}", news_article.summarize());

    notify(&tweet);
    notify(&news_article);

    notify2(&tweet);
    notify2(&news_article);
}

// using impl shorthand
fn notify(item: &impl traits::Summary) {
    println!("notify! {}", item.summarize());
}

// equivalent to notify
fn notify2<T>(item: &T) -> ()
where
    T: traits::Summary,
{
    println!("notify2! {}", item.summarize());
}
