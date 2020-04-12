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

## Running proceses

* [system()](http://ruby-doc.org/core/Kernel.html#method-i-system) - return value of true (zero exit), false (non-zero), and nil (failed execution)
* [backticks](http://ruby-doc.org/core/Kernel.html#method-i-60) - returns STDOUT, sets $? to the process status
* [exec()](http://ruby-doc.org/core/Kernel.html#method-i-exec) - replace current process by running command


## % Notation

| Modifier | Meaning                                                            |
|----------|--------------------------------------------------------------------|
| %i[ ]    | Non-interpolated symbol array (Ruby 2+)                            |
| %I[ ]    | Interpolated symbol array (Ruby 2+)                                |
| %q[ ]    | Non-interpolated String (except for \\ \[ and \])                  |
| %Q[ ]    | Interpolated String (default)                                      |
| %r[ ]    | Interpolated Regexp (flags can appear after the closing delimiter) |
| %s[ ]    | Non-interpolated Symbol                                            |
| %w[ ]    | Non-interpolated Array of words, separated by whitespace           |
| %W[ ]    | Interpolated Array of words, separated by whitespace               |
| %x[ ]    | Interpolated shell command                                         |

## Freezing strings

The `frozen_string_literal` pragma will have all strings in the code be immutable, improving performance. To debug, use `--debug=frozen-string-literal`
```ruby
# frozen_string_literal: true
```

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
