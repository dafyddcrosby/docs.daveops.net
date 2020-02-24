---
title: Rust
---

## Writing tests

If the function passes, the test passes

```rust
#[test]
fn this_tests_code(){
  println!("");
  if 1 == 0 {
    fail!("This should never happen");
  }
}
```

To compile the tests and replace main with test runner:

``rustc --test test.rs``

<https://doc.rust-lang.org/stable/book/testing.html>

## Syntax cheatsheet

```rust
// Add debugging info to struct
// Trait is std::fmt::Debug
#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

// use impl (implementation) block to create methods
impl Rectangle {
  fn area(&self) -> u32 {
    self.width * self.height
  }
  fn is_square(&self) -> bool {
    self.width == self.height
  }
  // associated functions
  #[allow(dead_code)]
  fn square(size: u32) -> Rectangle {
    Rectangle { width: size, height: size }
  }
}

fn main() {
  let mut a = 1; // mut makes the value mutable
  println!("{}", a);
  a = 2;
  println!("{}", a);

  // shadowing
  let x = 2;
  let x = x + 2;

  loop {
    println!("simple loop");
    break;
  }

  println!("easy as");
  for number in 1..4 {
    println!("{}", number);
  }

  let rect = Rectangle { width: 10, height: 20 };
  // get struct details for debugging
  println!("rect is {:?}", rect);
  println!("pretty print rect is {:#?}", rect);

  // using match
  let thing = 5;
  match thing {
     5 => println!("five!"),
     6 => {
         println!("six?");
         println!("why six?");
     },
     _ => (), // wildcard, don't do anything
  }
  // or using if let
  if let 5 = thing {
    println!("still five");
  } else {
    println!("not five");
  }

  // make a vector
  let mut v = vec![1,2,3];
  for i in 4..11 {
    v.push(i);
  }
  for (j, l) in v.iter().enumerate() {
    println!("index {}: {}", j, l);
  }
}
```

## Cargo

```bash
# New project
cargo new project_name --bin
# Build a release
cargo build --release
# Test a build
cargo check
# Create documentation and open it in a browser
cargo doc --open
# Build the executable, run it
cargo run
```

## rustc

```bash
# Build a library
rustc --crate-type=lib thing.rs
```

## Resources

* [Rust Programming Language Book](https://doc.rust-lang.org/stable/book/)
  * [Stack-Only Data: Copy](https://doc.rust-lang.org/stable/book/ch04-01-what-is-ownership.html#stack-only-data-copy)
* <https://doc.rust-lang.org/core/>
* <https://areweasyncyet.rs/>
* <https://www.rust-lang.org/documentation.html>
* <https://doc.rust-lang.org/stable/rust-by-example/>
* [vim plugin](https://github.com/rust-lang/rust.vim)

<!-- TODO
* Using rand crate 0.7+
* rust fmt
* enums are a little fuzzy - especially using impl on them
* revisit how Option works (ch 6.1)
* match keyword seems conceptually similar to switch in other langs
* crate paths/files (ch 7.5)
* The ? operator (ch 9.2)
* Traits still kind of hand-wavy (10.2)
* Lifetimes (10.3)
-->
