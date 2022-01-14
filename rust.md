# Rust

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
//! You can create a description for your crate using two slashes and an exclamation mark

// Add debugging info to struct
// Trait is std::fmt::Debug
/// Documentation comments use three slashes and Markdown
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

  eprinln!("Use this to print to stderr");

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

## Cross-compilation

* https://blog.rust-lang.org/2016/05/13/rustup.html
* https://sigmaris.info/blog/2019/02/cross-compiling-rust-on-mac-os-for-an-arm-linux-router/

```bash
rustup target list
rustup target add x86_64-unknown-linux-musl
cargo build --target x86_64-unknown-linux-musl
```

If you're working on Mac and compiling for Linux, you'll also want:

```bash
brew install x86_64-elf-binutils
```

And using that specific linker:

```toml
[target.x86_64-unknown-linux-musl]
linker = "x86_64-elf-ld"
```

## Nightly rust

```bash
# Install nightly toolchain
rustup toolchain install nightly
# Use nightly toolchain by default
rustup default nightly
```

## Resources

* [Rust Programming Language Book](https://doc.rust-lang.org/stable/book/)
  * [Stack-Only Data: Copy](https://doc.rust-lang.org/stable/book/ch04-01-what-is-ownership.html#stack-only-data-copy)
* <https://doc.rust-lang.org/core/>
* <https://areweasyncyet.rs/>
* <https://www.rust-lang.org/documentation.html>
* <https://doc.rust-lang.org/stable/rust-by-example/>
* [vim plugin](https://github.com/rust-lang/rust.vim)
* https://rustacean.net/
* [Rust on Windows](https://docs.microsoft.com/en-us/windows/dev-environment/rust/)

<!-- TODO
* Using rand crate 0.7+
* crate paths/files (ch 7.5)
* The ? operator (ch 9.2)
* Traits still kind of hand-wavy (10.2)
* Lifetimes (10.3)
* Closures (13)
-->

# Cargo

## Cargo watch

```bash
cargo watch -x check -x test
```


# Rust for iOS
```bash
# Be able to create iOS executables
cargo install cargo-lipo
# Install targets
rustup target add aarch64-apple-ios x86_64-apple-ios
```


# Rust Webassembly

## Using --target web

```bash
wasm-pack build --target web
```

```html
<html>
  <head>
    <meta content="text/html;charset=utf-8" http-equiv="Content-Type"/>
  </head>
  <body>
    <!-- Note the usage of `type=module` here as this is an ES6 module -->
    <script type="module">
      // Use ES module import syntax to import functionality from the module
      // that we have compiled.
      //
      // Note that the `default` import is an initialization function which
      // will "boot" the module and make it ready to use. Currently browsers
      // don't support natively imported WebAssembly as an ES module, but
      // eventually the manual initialization won't be required!
      import init, { add } from './pkg/without_a_bundler.js';

      async function run() {
        // First up we need to actually load the wasm file, so we use the
        // default export to inform it where the wasm file is located on the
        // server, and then we wait on the returned promise to wait for the
        // wasm to be loaded.
        //
        // It may look like this: `await init('./pkg/without_a_bundler_bg.wasm');`,
        // but there is also a handy default inside `init` function, which uses
        // `import.meta` to locate the wasm file relatively to js file.
        //
        // Note that instead of a string you can also pass in any of the
        // following things:
        //
        // * `WebAssembly.Module`
        //
        // * `ArrayBuffer`
        //
        // * `Response`
        //
        // * `Promise` which returns any of the above, e.g. `fetch("./path/to/wasm")`
        //
        // This gives you complete control over how the module is loaded
        // and compiled.
        //
        // Also note that the promise, when resolved, yields the wasm module's
        // exports which is the same as importing the `*_bg` module in other
        // modes
        await init();

        // And afterwards we can use all the functionality defined in wasm.
        const result = add(1, 2);
        console.log(`1 + 2 = ${result}`);
        if (result !== 3)
          throw new Error("wasm addition doesn't work!");
      }

      run();
    </script>
  </body>
</html>
```
