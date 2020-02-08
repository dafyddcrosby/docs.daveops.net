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
	a = 2;
	println!("{}", a);
}
```

## Resources

* <https://doc.rust-lang.org/core/>
* <https://areweasyncyet.rs/>
* <https://www.rust-lang.org/documentation.html>
* <https://rust-lang.github.io/book/> / <https://doc.rust-lang.org/book/>
* <https://doc.rust-lang.org/stable/rust-by-example/>
* [vim plugin](https://github.com/rust-lang/rust.vim)
