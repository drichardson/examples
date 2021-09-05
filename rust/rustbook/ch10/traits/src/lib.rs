pub trait Summary {
    fn summarize(&self) -> String {
        return String::from("default summary implementation");
    }
}

pub struct NewsArticle {
    pub headline: String,
    pub location: String,
    pub author: String,
    pub content: String,
}

// impl Summary for NewsArticle {} // get the default implementation

impl Summary for NewsArticle {
    fn summarize(&self) -> String {
        format!(
            "{}, by {} ({}) len={}",
            self.headline,
            self.author,
            self.location,
            self.content.len()
        )
    }
}

pub struct Tweet {
    pub username: String,
    pub content: String,
    pub reply: bool,
    pub retweet: bool,
}

impl Summary for Tweet {
    fn summarize(&self) -> String {
        format!(
            "{}: {} retweet={}, reply={}",
            self.username, self.content, self.retweet, self.reply
        )
    }
}
