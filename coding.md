# Coding

The focus of this page is the actual implementation of logic

# Binary Hacks

## Determine if integer is odd
```c
if ((x & 1) == 0) {
  //x is even
} else {
  //x is odd
}
```

## Determine if nth bit is set

```c
if (x & (1<<n)) {
  // n-th bit is set
} else {
  //  n-th bit is not set
}
```

## Set the nth bit

```c
y = x | (1<<n)
```


## Unset the nth bit

```c
y = x & ~(1<<n)
```


## Toggle the nth bit

```c
y = x ^ (1<<n)
```


# Demoscene tricks

## Fixed point math

Take a 16-bit integer, and break it into two 8 bit parts, an integer part and a fractional part.
Addition and subtraction are the same, but for multiplication and division you will need to do bit shifting

	A = ( C * D ) >> 8
	A = ( C << 8 ) / D


## Interpolation

k is within 0..1

	C = A + (B-A) * k

# comby

https://comby.dev

flag           | desc
---            | ---
-f SUFFIX      | search for files with suffix
-i             | replace files in-place
-d DIR         | only files in DIR
-diff          | get patches as unified diffs
-templates DIR | use a template directory (see below)

## matching syntax

pattern    | desc
---        | ---
:[hole]    | match zero or more characters (including whitespace, and across newlines) in a lazy fashion. When :[hole] is inside delimiters, as in {:[h1], :[h2]} or (:[h]), matching stops inside them. Holes can be used outside of delimiters as well.
:[[hole]\] | match one or more alphanumeric characters and _
:[hole.]   | (with a period at the end) matches one or more alphanumeric characters and punctuation (like ., ;, and -)
:[hole\n]  | (with a \n at the end) matches one or more characters up to a newline, including the newline.
:[ ]	(with a space) matches only whitespace characters, excluding newlines. To assign the matched whitespace to variable, put the variable name after the space, like :[ hole].
:[?hole]   | (with a ? before the variable name) optionally matches syntax. Optional holes work like ordinary holes, except that if they fail to match syntax, the variable is assigned the empty string "". Optional hole support is currently an experimental feature.

## templates

Create a directory with `match` and `rewrite` files

```bash
comby .go -templates /path/to/dir
```
# Style Guides

[Google Style Guides](<https://google.github.io/styleguide/>)
# Refactoring by Martin Fowler


* "Any fool can write code that a computer can understand. Good programmers write code that humans can understand."
* Switch statements should only be on your own data
* "Three strikes and you refactor"



# Design Patterns

Knowledge of design patterns make it easier to hold software designs in your head (a la memory chunking)

## Singleton

```python
class Singleton:
    __single = None
    def __init__( self ):
        if Singleton.__single:
            raise Singleton.__single
        Singleton.__single = self
```
# Trace Assertion Method

## Papers

- Using Traces to Write Abstract Specifications for Software Modules (Wolfram Bartussek and David Parnas)
