---
title: Ruby
---

## CLI

```bash
# Print warnings
ruby -w ...

# get machine instructions
ruby --dump insns script.rb

# See how commands are parsed
ruby --dump parsetree_with_comment script.rb
```

## Abort on thread errors

```ruby
Thread.abort_on_exception = true

Thread.new do
  fail 'Cannot continue'
end

loop do
  sleep
end
```

## Syntax cheatsheet

```ruby
class Thing
  attr_accessor :foo
  def initialize(foo = 0)
    @foo = foo
  end

  # use `self.` to define a class method
  def self.foobar
    puts "foobar"
  end

  private

  def bar
    puts "whoa"
  end
end

if val = 42
  #do stuff
elsif val = 33
  #do stuff
else
  #do stuff
end

# Ternary operator
exp ? true : false

# Begin block
begin
  # try to do this
rescue Exception
  # oh crap
else
  # whatever else
ensure
  # do this no matter what
end

# Case statement
case thing
when 3
  puts 'fizz'
when 5
  puts 'buzz'
else
  puts thing
end

# Safe navigation operator (introduced in Ruby 2.3)
u && u.profile && u.profile.thumbnails && u.profiles.thumbnails.large
# versus
u&.profile&.thumbnails&.large
```

## Running processes

- [system()](http://ruby-doc.org/core/Kernel.html#method-i-system) - return value of true (zero exit), false (non-zero), and nil (failed execution)
- [backticks](http://ruby-doc.org/core/Kernel.html#method-i-60) - returns STDOUT, sets $? to the process status
- [exec()](http://ruby-doc.org/core/Kernel.html#method-i-exec) - replace current process by running command

## % Notation

Modifier | Meaning
---      | ---
%i[ ]    | Non-interpolated symbol array (Ruby 2+)
%I[ ]    | Interpolated symbol array (Ruby 2+)
%q[ ]    | Non-interpolated String (except for \\ \[ and \])
%Q[ ]    | Interpolated String (default)
%r[ ]    | Interpolated Regexp (flags can appear after the closing delimiter)
%s[ ]    | Non-interpolated Symbol
%w[ ]    | Non-interpolated Array of words, separated by whitespace
%W[ ]    | Interpolated Array of words, separated by whitespace
%x[ ]    | Interpolated shell command

## Links

- [List of end-of-life Ruby versions](https://endoflife.date/ruby)

<!--- Todo

safelevel -T
set_trace_func proc
caller method
Marshal.dump
distributed Ruby (drb)
attr_reader attr_writer
Ruby Tk toolkit
ruby ncurses
loop retry redo
yield
Module for namespacing
require 'testunit'
bmbm - two-pass if garbage collection

* <https://www.exceptionalcreatures.com/>
* GC.stat
* Object::AllocationTracer (gem allocation_tracer)

-->


## Get list of undocumented code

```
rdoc -C1 > documentation_coverage.txt
```

 ### Links

- <http://documenting-ruby.org/>


## Extending and Embedding
 ### C

Take a look at doc/extension.rdoc in [MRI](https://github.com/ruby/ruby/blob/master/doc/extension.rdoc)

- [Running Ruby in C](https://silverhammermba.github.io/emberb/embed/)
- [Embedding a Ruby interpreter](https://www.linuxtopia.org/online_books/programming_books/ruby_tutorial/Extending_Ruby_Embedding_a_Ruby_Interpreter.html)

 ### C++

[Rice](https://github.com/jasonroelofs/rice)

 ### Rust

Can use C bindings...

 ### Java

- <https://www.jruby.org>


## Neat Ruby source files to look at

file          | desc
---           | ---
parse.y       | The lexing/parsing of Ruby source code
defs/keywords | The reserved keywords of Ruby

## parsing
 ### parsetree (CLI)

```bash
ruby --dump parsetree
```

This gives the names for the node objects used in ast.c/compile.c/node.c/vm.c in
CRuby source (output not usable in other implementations)

 ### Ripper

Built-in since Ruby 1.9

Dumps S-expressions

```ruby
require 'ripper'
require 'pp'

f = File.read('foo.rb')
pp Ripper.sexp(f)
```

http://www.rubyinside.com/using-ripper-to-see-how-ruby-is-parsing-your-code-5270.html

 ### ast and parser gems
- https://github.com/whitequark/ast
- https://github.com/whitequark/parser

 ### code reconstruction from AST
- [unparser](https://github.com/mbj/unparser)
- [sorcerer](https://github.com/jimweirich/sorcerer)

## Ractors
Ractors are Ruby's take on the Actor Model. The main Ruby thread is a Ractor.


```ruby
ract = Ractor.new do
  puts "hello"
end

ract.take
```

- [Ruby Ractor Reference](https://ruby-doc.org/core-3.0.2/Ractor.html)
- [Good intro to Ractors [ScoutAPM]](https://scoutapm.com/blog/ruby-ractor)
- [Koichi's 'guild' slides that Ractor was based off](https://www.atdot.net/~ko1/activities/2016_rubykaigi.pdf)
