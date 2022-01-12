---
title: Ruby
tags:
  - Ruby
---

# Ruby

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

## JSON

```ruby
require 'json'
obj = JSON.parse(File.read('./thing.json'))
obj.to_json
JSON.pretty_generate(obj)
```


## Naming idioms

```ruby
local_variable
@instance_variable
@@class_variable
CONSTANT_VARIABLE
ClassName
ModuleName
$global
```

## Minitest
- <https://ruby-doc.org/core-3.0.2/_bundle/gems/minitest-5_14_2/README_rdoc.html>


## CSV
```ruby
require 'csv'
data = CSV.read('path/to/file', headers: true)
```



## Write to a file

```ruby
fp = File.open(filename, mode)
fp.write('bloop')
fp.close
```

## Read individual chars

```ruby
fp = File.open(filename, mode)
fp.each_char do |char|
      puts char
end
fp.close
```


# Gems

## Build a gem

```bash
gem build name.gemspec
```

## Uploading

```bash
# push gem to rubygems.org or other host
gem push name-0.0.1.gem [--host HOST]
```

## Sources

```bash
# add a source
gem source -a SOURCE
# remove a source
gem source -r SOURCE
# update source cache
gem source -u
```

## Ownership

```bash
gem owner GEM --add EMAIL
gem owner GEM --remove EMAIL
```

## Environment variables

var      | description
---      | ---
GEM_PATH | where gems can be loaded
GEM_HOME | where gems are installed

## Links

* [Gem packaging best practices](http://weblog.rubyonrails.org/2009/9/1/gem-packaging-best-practices/)


## heredoc

	message1 = <<EOM
	This string starts at line start
	EOM # Needs to be at 0 position
	
	# TODO <<-EOM
	
	message3 = <<~EOM
	  This one will remove space before first printable character
	  Available in Ruby 2.3+
	EOM




# ObjectSpace

This module is useful for understanding the memory size of what you're working with, as well as seeing what objects are still alive at a given time

```ruby
ObjectSpace.count_objects
```

- <https://ruby-doc.org/core-3.0.2/ObjectSpace.html>


# Versions

This is a list of interesting bits from Ruby versions

## 3.1

### YJIT

YJIT looks to get some excellent speed improvements in

Moreover, what *really* impresses me is how quickly Maxime was welcomed into
the core community and YJIT merged into the upstream.

- [Redmine: Proposal to merge YJIT](https://bugs.ruby-lang.org/issues/18229)
- [Github: Merge YJIT: an in-process JIT compiler](https://github.com/ruby/ruby/pull/4992)
- [Maxime's blog post](https://pointersgonewild.com/2021/06/02/yjit-building-a-new-jit-compiler-inside-cruby/)

### New Debugger

- [Sneak preview of Ruby's new debugger](https://dev.to/st0012/a-sneak-peek-of-ruby-s-new-debugger-5caa)
- <https://github.com/ruby/debug>


## 2.4
- binding.irb
## 2.1
- generational garbage collector
## 2.3
- frozen string literal pragma
## 2.2
- incremental garbage collector
## 2.0
- copy-on-write friendly memory management

## 1.9
- added the Ruby Virtual Machine

# Rdoc

```text
*word*
    displays word in a bold font
_word_
    displays word in an emphasized font
+word+
    displays word in a code font
```

Comments:

```ruby
# This is the foo function
#--
# Lines between -- and ++ won't be parsed
#++
# It will return true
def foo
  true
end
```

Don't document a thing:

```ruby
module MyModule # :nodoc:
end
```

Links:

```text
https://github.com/example/example
mailto:user@example.com
{RDoc Documentation}[http://rdoc.rubyforge.org]
{RDoc Markup}[rdoc-ref:RDoc::Markup]
```

## Using `ri` on Fedora

To get the system ruby library documentation, you'll need to install `ruby-doc`

```bash
sudo dnf install rubygem-rdoc ruby-doc
```

## Links

- <https://docs.ruby-lang.org/en/2.1.0/RDoc/Markup.html>
- <https://www.mikeperham.com/wp-content/uploads/2010/12/rdoc.html>
- <https://jan.varwig.org/wp-content/uploads/2006/09/Rdoc%20Cheat%20Sheet.pdf>


# Erb
<http://ruby-doc.org/stdlib-2.4.0/libdoc/erb/rdoc/ERB.html>

## Tags

```erb
<% Ruby code -- inline with output %>
<%= Ruby expression -- replace with result %>
<%# comment -- ignored -- useful in testing %>
% a line of Ruby code -- treated as <% line %> (optional -- see ERB.new)
%% replaced with % if first thing on a line and % processing is used
<%% or %%> -- replace with <% or %> respectively
```

## Trim mode

code | desc
---  | ---
%    | enables Ruby code processing for lines beginning with %
<>   | omit newline for lines starting with <% and ending in %>
>    | omit newline for lines ending in %>
-    | omit blank lines ending in -%>


# Fibers

A fiber is an independent execution context that can be paused and resumed
programmatically. There's always a currently active fiber, which is created by
the runtime for each thread. Fibers are managed in the userspace program, using
cooperative multitasking (the fiber must voluntarily give up control) instead
of the OS' pre-emptive multitasking.

A fiber always starts in a suspended state, it will not run until you switch to it with #transfer.

States: `:running`, `:waiting`, `:runnable`, `:dead`

- [Fiber class](https://ruby-doc.org/core-3.0.2/Fiber.html)
- [Fiber::SchedulerInterface](https://ruby-doc.org/core-3.0.2/Fiber/SchedulerInterface.html)
- <https://noteflakes.com/articles/2021-10-20-explaining-ruby-fibers>


# Debugging

## -r debug

```bash
ruby -r debug example.rb
```

<!---
~/.irbrc options
-->

## Irb

As of Ruby 2.4+, you can use `binding.irb` for an experience similar to Pry

## Pry

<http://pryrepl.org/>

```ruby
require 'pry'

# start REPL
binding.pry
```

```text
# get list of commands
repl> help

# quit program
repl> !!!
```


# Performance

As with any system performance advice, don't forget to [benchmark](ruby.md)

## Frozen string literals

When 'freeze' is called on a string you are marking that string as immutable.
The performance benefit is that the object can be re-used without new object
allocation.

```ruby
def unfrozen
  a = 'hello'
  a.object_id
end

p unfrozen
p unfrozen

def frozen
  a = 'world'.freeze
  a.object_id
end

p frozen
p frozen
```

```bash
$ ruby freeze.rb
60
80
100
100
```

### File pragma

In Ruby 2.3+, you can add this pragma to opt-in to all string literals being
made immutable in the source file. Note - this isn't a guaranteed performance
win!

To debug, use `--debug=frozen-string-literal`

```ruby
# frozen_string_literal: true
```

## Performance pitfalls

### Inefficient string concatenation

```ruby
x = 'a'
x += 'b'
# the above is equivalent to
x = 'a'
y = x + 'b'
x = y
```

instead, use this:

```ruby
x = 'a'
x << 'b'
```

Worst case, it'll be a `realloc` but won't trigger GC.

### Memory-heavy iterators

When going through a file, it's better to read the file line-by-line like the
following to avoid GC hits:
<!--- TODO put inefficient example here-->

```ruby
file = File.open('foobar', 'r')
while line = file.gets
  line.split('.')
end
```

## Use in-place operations whenever possible

The difference between `sort` and `sort!` can be pretty significant, especially when you're dealing with loops and big objects, since you're copying the object with `sort`. If you don't need to re-use the value, use a mutating (`!`) function.

## Use a time format instead of Date#parse

If you know in advance what your time format is, don't use Date#parse, since there's a bunch of overhead just figuring out what kind of time it is.

## Other significant wins

- Use a hash over an OpenStruct
- Use a single array over a splat operator
- Use `Array#bsearch` over `Array#find` (with sorted lists only!)
- Use `Array#sample` over `Array#shuffle.first` (saves an extra allocation)
- Use `Array#insert` over `Array#unshift`
- Use `Hash#key?` over `Hash#keys.include?` ( O(1) vs O(N) and allocation )
- Use `Hash#value?` over `Hash#values.include?` ( O(N) vs O(N) and allocation )
- Use `String#start_with?` over regex when you only care about the beginning of the string
- Use `String#match?` over `String#match` (if you don't need the resulting match)
- Use `String#tr` over `String#gsub`
- Use `String#casecmp` over `String#downcase + ==`
- Use `Enumerable#detect` over `Enumerable#select.first`

## See also 

- [ObjectSpace](ruby.md)

## Books / Links

- Ruby Performance Optimization by Alexander Dymo
- https://github.com/JuanitoFatas/fast-ruby
- https://github.com/DamirSvrtan/fasterer


# Benchmarking

## Running the profiler

```bash
ruby -r profile script.rb
```

## Benchmarking

```ruby
require 'benchmark'

n = 50000

# this gives you a Benchmark::Tms object
tms = Benchmark.measure { for i in 1..n; a = "1"; end }

# Returns [@label, @utime, @stime, @cutime, @cstime, @real]
tms.to_a
```

- https://github.com/evanphx/benchmark-ips
- https://github.com/davy/benchmark-bigo


# Contributing

- [Github source mirror](https://github.com/ruby/ruby)
- [Bug tracker](https://redmine.ruby-lang.org/)

## Template for feature proposals

From `contributing.rdoc`:

> [Abstract]
>   Summary of your feature
> [Background]
>   Describe current behavior and why it is problem. Related work, such as
>   solutions in other language helps us to understand the problem.
> [Proposal]
>   Describe your proposal in details
> [Details]
>   If it has complicated feature, describe it
> [Usecase]
>   How would your feature be used? Who will benefit from it?
> [Discussion]
>   Discuss about this proposal. A list of pros and cons will help start
>   discussion.
> [Limitation]
>   Limitation of your proposal
> [Another alternative proposal]
>   If there are alternative proposals, show them.
> [See also]
>   Links to the other related resources



# RubyVM

<https://ruby-doc.org/core-2.2.0/RubyVM/InstructionSequence.html>

