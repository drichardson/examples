fn main() {
    test_drop_and_display();
    test_memory_allocations();
}

macro_rules! print_test_header {
    () => {
        let function_name = function!();
        let border = "=".repeat(function_name.chars().count());
        println!("{}\n{}\n{}", border, function_name, border);
    };
}

// function! macro from: https://stackoverflow.com/a/40234666/196964
macro_rules! function {
    () => {{
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);
        &name[..name.len() - 3]
    }};
}

struct MyStruct {
    val: String,
}

impl Drop for MyStruct {
    fn drop(&mut self) {
        println!("Drop for MyStruct");
    }
}

impl std::fmt::Display for MyStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("MyStruct.val={}", self.val))
    }
}

fn test_drop_and_display() {
    print_test_header!();

    println!("making vec");
    let v = vec![1, 2, 3];
    println!("dropping vec");
    drop(v); // explicitly drop
             // drop(v); // error: use of moved value

    {
        let m = MyStruct {
            val: String::from("s1"),
        };
        println!("m={}", m);
        println!("leaving scope 1");
    }
    println!("left scope 1");

    {
        let m = MyStruct {
            val: String::from("s2"),
        };
        println!("m={}", m);
        drop(m);
        // drop(m); // error: m used after move
        // println!("m={}", m); // error: m borrowed after move
        println!("leaving scope 2");
    }
    println!("left scope 2");
}

fn test_memory_allocations() {
    print_test_header!();

    unsafe {
        let layout = std::alloc::Layout::new::<u16>();
        let ptr = std::alloc::alloc(layout);
        assert!(!ptr.is_null());
        *ptr = 42;
        println!("ptr={}", *ptr);
        std::alloc::dealloc(ptr, layout);
        // println!("ptr={}", *ptr); // segmentation fault
        //std::alloc::dealloc(ptr, layout);
    }

    let layout = std::alloc::Layout::from_size_align(4000, 8).unwrap();

    unsafe {
        let m = std::alloc::alloc(layout);
        assert!(!m.is_null());
    }
}
