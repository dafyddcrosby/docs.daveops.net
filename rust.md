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
```

## rustc

```bash
# Build a library
rustc --crate-type=lib thing.rs
```

## Resources

* <https://doc.rust-lang.org/core/>
* <https://areweasyncyet.rs/>
* <https://www.rust-lang.org/documentation.html>
* <https://rust-lang.github.io/book/> / <https://doc.rust-lang.org/book/>
* <https://doc.rust-lang.org/stable/rust-by-example/>
* [vim plugin](https://github.com/rust-lang/rust.vim)

<!-- TODO
* Using rand crate 0.7+
* rust fmt
-->
