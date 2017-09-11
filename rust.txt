Rust
====
:date: 2016-06-06

https://doc.rust-lang.org/core/

Writing tests
-------------
If the function passes, the test passes

.. code-block:: rust

  #[test]
  fn this_tests_code(){
      println!("");
      if 1 == 0 {
          fail!("This should never happen");
      }
  }

To compile the tests and replace main with test runner:

::

  rustc --test test.rs

https://doc.rust-lang.org/stable/book/testing.html

Syntax cheatsheet
-----------------
.. code-block:: rust

  fn main() {
      let mut a = 1; // mut makes the value mutable
      a = 2;
      println!("{}", a);
  }

Resources
---------

- https://www.rust-lang.org/documentation.html
- https://rust-lang.github.io/book/ / https://doc.rust-lang.org/book/
- http://rustbyexample.com/
- `vim plugin <https://github.com/rust-lang/rust.vim>`_
