
# Programming Language Theory and Design

# Compilers

A compiler transforms one language into another (eg High-Level Language -> Machine Code)

A [lexer](#org023366b) tokenizes a file

A [parser](#org286a75e) takes the tokens and creates an abstract syntax tree


## Self-hosting

A self-hosted compiler is one where the language is used for its own compiler. In order to accomplish this, the initial compiler is bootstrapped from either another language or assembled


## Compiler-compilers

A compiler-compiler takes a grammar file and creates a source files for an implementation to use

See lex, yacc, bison


## Just-in-time compilation

JIT compilation is when the interpreter takes the loaded program and compiles it to native code for the architecture the program is running on.

Sophisticated JITs use profiling hooks in generated code to see which regions are performance critical, recompiling with more advanced optimizations as needed (eg HotSpot JVM)


## Single-pass compilers

You can create a simple compiler that parses the source, analyzes the code, and generates native language code in a single pass. There's no IR, and code isn't revisited. Early C and Pascal compilers did this since memory was at a premium, which is why type and function declarations have to be defined before usage.


# Assembler

An assembler translates a program into machine code. There is no fancy changing of code like you would get with a compiler - this is an easier way to read machine code.


<a id="org023366b"></a>

# Lexers

AKA Scanners

Convert strings of characters into groups of [tokens](#org7865770)

After lexical analysis, something like

```
foo = 2 + 2
```

Would become something like

```
[:identifier, +'=', :number, :'+', :number]
```

A lexer will use multiple pointers in the file to know

- the line number
- the starting location of a [lexeme](#orgee1d433)
- where in the lexeme it is as it is scanning

Not all tokens can be determined by the first character (take for example `!=` - if you're too agressive, instead of 'not equal' you could get 'not' and 'assign' - different semantics!)


<a id="orgee1d433"></a>

## Lexemes

A sequence of characters that matches a [token](#org7865770) pattern


<a id="org7865770"></a>

## Tokens

A token is a data structure that conveys the meaning of a [lexeme](#orgee1d433)


<a id="org23cdbc9"></a>

## Expressions

An expression, in contrast to [statements](#orgf0cf9a5), will return a value. `2 + 2` is an example of an expression.

In languages like Ruby/Lisp/etc where "everything is an expression", you have to decide what a control flow (if, while) evaluates as, a variable declation evaluates to, a block. In exchange for making a few critical choices up front, you cut out redundancy and get implicit returns (ie last expression is the return value) cheaply.


<a id="orgf0cf9a5"></a>

## Statements

A statement, in contrast to an [expressions](#org23cdbc9), does not produce a value. This is often something like a conditional.


# Static analysis

Binding/resolution is where an identifier (eg a variable name) is defined and the scope is determined.

In statically-typed languages, this is where the type is checked.

The information from an identified node can be stored as an **attribute** within the AST tree node. It can also be put in a **symbol table** (a key-value lookup). The tree can also be transformed into an **[Intermediate Representation](#orga77ede2)**.


<a id="orga77ede2"></a>

## Intermediate Representation

An intermediate representation is tied neither to the source language nor to a particular target platform - this makes it easy to have a 'general' compiler that can handle multiple languages and platforms (eg [LLVM](compilers.md), [GCC](compilers.md))


# Class-based vs Prototype-based languages

In class-based, instances store state for an object and have a reference to a class. Classes contain the methods and inheritance chain

To call a method on an instance requires a lookup of the instances' classes' methods.

In a prototype-based language there are no classes, only objects, and each object may contain state and methods. Objects can directly inherit form each other.

There's no hard-line difference between the two - JS constructor functions create class-like objects. Class-based Ruby lets you attach methods to individual instances. What often happens with prototype-based languages is that the user creates classes instead of the language (what Larry Wall calls the "Waterbed Theory" - there's a basic complexity that's either handled by the user or the language, but it's going to be there nevertheless).


# Static and dynamic dispatch

Static dispatch is when method lookup happens at compile time based on the static type of the instance.

Dynamic dispatch looks up the class of the actual instance object at runtime. Virtual methods are a form of dynamic dispatch


<a id="org286a75e"></a>

# Parsers

A parser works through a list of [tokens](#org7865770) to create an [abstract syntax tree](#org4c2f5ea). It uses multiple pointers to track the state of the input (eg. the tokens inside of matching brackets (which are also tokens))

A parser will determine if something is a [statement](#orgf0cf9a5) or an [expression](#org23cdbc9)

When parsing, you will need to decide how to handle unrecognized tokens (generally best to throw an error)

- [Syntactical sugar](#orgb7d600e)
- [Abstract Syntax Trees](#org4c2f5ea)


<a id="org4c2f5ea"></a>

## Abstract Syntax Trees

An AST is only concerned with the semantic meaning of a program, not the syntactical expression.

You can compare two programs if each node in the AST implements an equivalence function (`==`) comparing the children values


# Interpreters

An interpreter takes the code that it is given and executes it immediately.

The interpreter will need to track the output and the environment (eg variable assignments, etc) as it walks the tree.


## Tree-walking interpreter

A tree-walking interpreter walks each leaf of an [AST](#org4c2f5ea) to execute - this can be kind of slow.

Early Ruby versions using the Matz Ruby Interpreter did this, before being replaced by Yet Another Ruby VM (YARV)


<a id="orgb7d600e"></a>

# Syntactical Sugar

Syntax sugar is when, through a shorthand, you are able to express the same semantics in less code. You can spot sugar when removing it causes no irreparable harm to a program (ie could transform the code to the more verbose version, often mechanically).

```ruby
if some_condition
  # ...
else
  if another_condition
    # ...
  end
end

# Semantically identical syntax sugar
if some_condition
  # ...
elsif another_condition
  # ...
end
```


# Code generation

Code generation is when you create code for the underlying platform/processor.

One way of addressing portability is generating p-code/bytecode for a hypothetical machine, which isn't tied too tightly to a particular computer architecture. Then you write a language virtual machine that runs on the physical platform, which should be easier than writing native machine code.


# Code Optimization


## Constant folding

Constant folding is when the compiler determines that an expressions will always evaluate the same way. `a = 2 + 5` can be converted at compile time to `a = 7`.


# Closures

A closure is when a function has an inner function that is allowed to use references within the outer function. This means you can't assume variable scope is like a stack (where variables disappear on function return).


# Syntax

An argument is a value you pass to a function when you call it.

A parameter is a variable that holds the value of the argument inside the body of the function. A function declaration has a *parameter list*. AKA formal parameters


# Control Flow

Control flow is a way of managing the logic of a program with if/then, goto, etc.

- if statements
- loop constructs
    - while
    - for
    - foreach
        - requires dynamic dispatch to an iterator protocol
- goto


# Memory Management

3 kinds of MM

1. Hardware MM (eg MMU, RAM,)
2. OS MM (virtual memory, process memory protection)
3. Application MM (garbage collection)

Resources:

- <https://www.memorymanagement.org>


## tracing garbage collection

Trickier to debug than [reference counting](#org39c861e)

> In practice, ref counting and tracing are more ends of a continuum than opposing sides. Most ref counting systems end up doing some tracing to handle cycles, and the write barriers of a generational collector look a bit like retain calls if you squint.


<a id="org39c861e"></a>

## Reference counting

Reference counting is easy to implement, but has limitations. Perl, PHP, and Python started with it, but eventually moved to tracing garbage collection


# Papers/books


## A Nanopass Framework for Compiler Education

Dipanwita Sarkar, Oscar Waddell, R. Kent Dybvig

<https://legacy.cs.indiana.edu/~dyb/pubs/nano-jfp.pdf>

Each pass of a micropass compiler performs a single specific task:

- simplify (eg relaxing the AST)
- verify (eg check compiler invariants that are hard to express in grammar)
- convert (eg makes abstractions explicit into lower-level target language)
- analyze (eg collect program information and annotate the output program)
- improve (eg decrease run time/resource utilization)

A nanopass compiler differs from a micropass compiler in three ways:

1. the intermediate-language grammars are formally specified and enforced
2. each pass needs to contain traversal code only for forms that undergo meaningful transformation
3. the intermediate code is represented more efficiently as records, although all interaction with the programmer is still via the s-expression syntax


## "Go To Statement Considered Harmful" by Edsgar Dijkstra

Dijkstra argues in "Go To Statement Consider Harmful" that beyond being logically superfluous (Bohm-Jacopini), goto has the problem of making programs hard to reason about - we aren't just reasoning about the static program, we're reasoning about the dynamic process behaviour as well (which is more difficult).


## Program Development by Stepwise Refinement by Niklaus Wirth

It's better pedagogically to show (and do) the development process than read old programs

8 Queen's problem: 8x8 chessboard and 8 queens that are hostile. What configuration must they be in to be on the board

Brute force would take ~8 hours if each config took 100 microseconds. (That would be ~4-5 minutes at one microsecond - Moore's Law could well make this kind of a example a moot point if seen in terms of developer productivity time...)
