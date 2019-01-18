---
title: Ruby
---

Running the profiler
--------------------

```bash
ruby -rprofile script.rb
```

get machine instructions
------------------------

```bash
ruby --dump insns script.rb
```

See how commands are parsed
---------------------------

```bash
ruby --dump parsetree_with_comment script.rb
```

Abort on thread errors
----------------------

```ruby
Thread.abort_on_exception = true

Thread.new do
  fail 'Cannot continue'
end

loop do
  sleep
end
```


Syntax cheatsheet
-----------------

```ruby
class Thing
  attr_accessort :foo
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

begin
  # try to do this
rescue Exception
  # oh crap
else
  # whatever else
ensure
  # do this no matter what
end

case thing
when 3
  puts 'fizz'
when 5
  puts 'buzz'
else
  puts thing
end
```

Running proceses
----------------

* [system()](http://ruby-doc.org/core/Kernel.html#method-i-system) - return value of true (zero exit), false (non-zero), and nil (failed execution)
* [backticks](http://ruby-doc.org/core/Kernel.html#method-i-60) - returns STDOUT, sets $? to the process status
* [exec()](http://ruby-doc.org/core/Kernel.html#method-i-exec) - replace current process by running command


% Notation
----------

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

Todo
----

* GC.stat
* Object::AllocationTracer (gem allocation_tracer) 
