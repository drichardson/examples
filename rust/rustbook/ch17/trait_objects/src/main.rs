fn main() {
    let screen = Screen {
        components: vec![
            Box::new(Button {
                width: 100,
                height: 40,
                label: String::from("Submit"),
            }),
            Box::new(SelectBox {
                width: 200,
                height: 40,
                options: vec![
                    String::from("Canada"),
                    String::from("Mexico"),
                    String::from("United States"),
                ],
            }),
        ],
    };

    screen.run();
}

trait Draw {
    fn draw(&self);
}

struct Screen {
    components: Vec<Box<dyn Draw>>,
}

impl Screen {
    fn run(&self) {
        for component in self.components.iter() {
            component.draw();
        }
    }
}

struct Button {
    width: u32,
    height: u32,
    label: String,
}

impl Draw for Button {
    fn draw(&self) {
        println!(
            "draw button: {}x{} label={}",
            self.width, self.height, self.label
        );
    }
}

struct SelectBox {
    width: u32,
    height: u32,
    options: Vec<String>,
}

impl Draw for SelectBox {
    fn draw(&self) {
        println!(
            "draw select box: {}x{} options={:?}",
            self.width, self.height, self.options
        );
    }
}
