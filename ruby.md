
# Ruby

# Resources


## [official website](https://www.ruby-lang.org/en/)


## [docs website](https://docs.ruby-lang.org/en/)


## specification

[Ruby Spec Suite](https://github.com/ruby/spec)

ISO/IEC 30170:2012 <https://www.iso.org/standard/59579.html> (not free, though)


## [Github source mirror](https://github.com/ruby/ruby)


## community


### MINASWAN

"Matz is Nice And So We Are Nice"


## Academic papers on Ruby

[The Ruby Bibliography](https://rubybib.org/)


## Contributing upstream

- [Bug tracker](https://redmine.ruby-lang.org/)


### Template for feature proposals

From `contributing.rdoc`:

<p class="verse">
[Abstract]<br />
&#xa0;&#xa0;Summary of your feature<br />
[Background]<br />
&#xa0;&#xa0;Describe current behavior and why it is problem. Related work, such as<br />
&#xa0;&#xa0;solutions in other language helps us to understand the problem.<br />
[Proposal]<br />
&#xa0;&#xa0;Describe your proposal in details<br />
[Details]<br />
&#xa0;&#xa0;If it has complicated feature, describe it<br />
[Usecase]<br />
&#xa0;&#xa0;How would your feature be used? Who will benefit from it?<br />
[Discussion]<br />
&#xa0;&#xa0;Discuss about this proposal. A list of pros and cons will help start<br />
&#xa0;&#xa0;discussion.<br />
[Limitation]<br />
&#xa0;&#xa0;Limitation of your proposal<br />
[Another alternative proposal]<br />
&#xa0;&#xa0;If there are alternative proposals, show them.<br />
[See also]<br />
&#xa0;&#xa0;Links to the other related resources<br />
</p>


## Conferences

- [Ruby Kaigi](https://rubykaigi.org/)
- [Rails Conf](https://rubyconf.org/)


# Syntax cheatsheet

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


## % Notation

| Modifier | Meaning                                                            |
|-------- |------------------------------------------------------------------ |
| %i[ ]    | Non-interpolated symbol array (Ruby 2+)                            |
| %I[ ]    | Interpolated symbol array (Ruby 2+)                                |
| %q[ ]    | Non-interpolated String (except for \\ [ and ])                    |
| %Q[ ]    | Interpolated String (default)                                      |
| %r[ ]    | Interpolated Regexp (flags can appear after the closing delimiter) |
| %s[ ]    | Non-interpolated Symbol                                            |
| %w[ ]    | Non-interpolated Array of words, separated by whitespace           |
| %W[ ]    | Interpolated Array of words, separated by whitespace               |
| %x[ ]    | Interpolated shell command                                         |


## Read individual chars

```ruby
fp = File.open(filename, mode)
fp.each_char do |char|
  puts char
end
fp.close
```


## heredoc

[heredoc documentation](https://docs.ruby-lang.org/en/master/doc/syntax/literals_rdoc.html#label-Here+Document+Literals)

```ruby
  message1 = <<EOM
  This string starts at line start
EOM # Needs to be at 0 position

  message2 = <<-EOM
While the terminator is indented, text is left flush
  EOM

  message3 = <<~EOM
    This one removes space before first printable character 
      of the least indented line. Empty lines ignored

    Available in Ruby 2.3+
  EOM
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


# concepts, design, terminology


## Nakayoshi fork

"Friendly fork"

Runs the GC several times before forking to make a clean heap for copy-on-write benefit, reduced memory usage.

```ruby
# looks a bit like this
4.times { GC.start }
GC.compact
```


# CLI

```shell
# Print warnings
ruby -w ...

# get machine instructions
ruby --dump insns script.rb

# See how commands are parsed
ruby --dump parsetree_with_comment script.rb
```


# Releases

This is a list of interesting bits from Ruby versions

[Ruby Changes](https://rubyreferences.github.io/rubychanges/)


## [3.1](https://www.ruby-lang.org/en/news/2021/12/25/ruby-3-1-0-released/)

- [YJIT](https://github.com/ruby/ruby/pull/4992) (looks to get some excellent speed improvements in)
    - Moreover, what *really* impressed me is how quickly Maxime was welcomed into the core community and YJIT merged into the upstream.
    - [Redmine: Proposal to merge YJIT](https://bugs.ruby-lang.org/issues/18229)
    - [Maxime's blog post](https://pointersgonewild.com/2021/06/02/yjit-building-a-new-jit-compiler-inside-cruby/)
- [New debugger](https://github.com/ruby/debug)
    - [Sneak preview of Ruby's new debugger](https://dev.to/st0012/a-sneak-peek-of-ruby-s-new-debugger-5caa)
- [Error highlighting](https://github.com/ruby/error_highlight)
- Irb autocompletion with tab, documentation with alt-d
- pin operator in pattern matching can take an expression, parentheses optional in one-line matching
- values in hash literals can be omitted
- Psych.load uses safe<sub>load</sub> by default
- [multiple assignment evaluation order changes](https://bugs.ruby-lang.org/issues/4443)


## [3.0](https://www.ruby-lang.org/en/news/2020/12/25/ruby-3-0-0-released/)


## [2.7](https://www.ruby-lang.org/en/news/2019/12/25/ruby-2-7-0-released/)


## [2.6](https://www.ruby-lang.org/en/news/2018/12/25/ruby-2-6-0-released/)


## [2.5](https://www.ruby-lang.org/en/news/2017/12/25/ruby-2-5-0-released/)


## [2.4](https://www.ruby-lang.org/en/news/2016/12/25/ruby-2-4-0-released/)

- binding.irb


## [2.3](https://www.ruby-lang.org/en/news/2015/12/25/ruby-2-3-0-released/)

- frozen string literal pragma


## [2.2](https://www.ruby-lang.org/en/news/2014/12/25/ruby-2-2-0-released/)

- incremental garbage collector


## [2.1](https://www.ruby-lang.org/en/news/2013/12/25/ruby-2-1-0-is-released/)

- First release with [semantic versioning](https://www.ruby-lang.org/en/news/2013/12/21/ruby-version-policy-changes-with-2-1-0/)
- generational garbage collector


## [2.0](https://www.ruby-lang.org/en/news/2013/02/24/ruby-2-0-0-p0-is-released/)

- copy-on-write friendly memory management


## [1.9.3](https://www.ruby-lang.org/en/news/2011/10/31/ruby-1-9-3-p0-is-released/)


## [1.9.2](https://www.ruby-lang.org/en/news/2010/08/18/ruby-1-9-2-released/)


## [1.9.1](https://www.ruby-lang.org/en/news/2009/01/30/ruby-1-9-1-released/)


## [1.9.0](https://www.ruby-lang.org/en/news/2007/12/25/ruby-1-9-0-released/)

- added the Ruby Virtual Machine


## [1.8.7](https://www.ruby-lang.org/en/news/2008/05/31/ruby-1-8-7-has-been-released/)

- securerandom library
- Array handles recursive data properly
- Lots of new base class methods


## [1.8.6](https://www.ruby-lang.org/en/news/2007/03/12/ruby-1-8-6-released/)

- First release with NEWS file


## [List of end-of-life Ruby versions](https://endoflife.date/ruby)


# Execution


## Threads

```ruby
# Abort on thread errors
Thread.abort_on_exception = true

Thread.new do
  fail 'Cannot continue'
end

loop do
  sleep
end
```


## Fibers

A fiber is an independent execution context that can be paused and resumed programmatically. There's always a currently active fiber, which is created by the runtime for each thread. Fibers are managed in the userspace program, using cooperative multitasking (the fiber must voluntarily give up control) instead of the OS' pre-emptive multitasking.

A fiber always starts in a suspended state, it will not run until you switch to it with #transfer.

States: `:running`, `:waiting`, `:runnable`, `:dead`

- [Core fiber readme](https://docs.ruby-lang.org/en/master/doc/fiber_md.html)
- [Fiber class](https://docs.ruby-lang.org/en/master/Fiber.html)
- [Fiber::SchedulerInterface](https://docs.ruby-lang.org/en/master/Fiber/SchedulerInterface.html)
- <https://noteflakes.com/articles/2021-10-20-explaining-ruby-fibers>


## Running processes

- [system()](https://docs.ruby-lang.org/en/master/Kernel.html#method-i-system) - return value of true (zero exit), false (non-zero), and nil (failed execution)
- [backticks](https://docs.ruby-lang.org/en/master/Kernel.html#method-i-60) - returns STDOUT, sets $? to the process status
- [exec()](https://docs.ruby-lang.org/en/master/Kernel.html#method-i-exec) - replace current process by running command


## Ractors

Ruby's take on the Actor Model. The main Ruby thread is a Ractor.

```ruby
ract = Ractor.new do
  puts "hello"
end

ract.take
```

- [Ruby Ractor Reference](https://docs.ruby-lang.org/en/master/Ractor.html)
- [Good intro to Ractors [ScoutAPM](https://scoutapm.com/blog/ruby-ractor)]
- [Koichi's 'guild' slides that Ractor was based off](https://www.atdot.net/~ko1/activities/2016_rubykaigi.pdf)


## Async

- <https://brunosutic.com/blog/async-ruby>
- [async gem](https://github.com/socketry/async)


# Extending and Embedding


## C

Take a look at doc/extension.rdoc in [MRI](https://github.com/ruby/ruby/blob/master/doc/extension.rdoc)

- [Running Ruby in C](https://silverhammermba.github.io/emberb/embed/)
- [Embedding a Ruby interpreter](https://www.linuxtopia.org/online_books/programming_books/ruby_tutorial/Extending_Ruby_Embedding_a_Ruby_Interpreter.html)


## C++

[Rice](https://github.com/jasonroelofs/rice)


## Rust

Can use C bindings...


## Java

Use [JRuby](https://www.jruby.org)


# Implementations


## [CRuby](https://github.com/ruby/ruby)

AKA Matz Ruby Implementation


### Neat Ruby source files to look at

| file          | desc                                   |
|------------- |-------------------------------------- |
| parse.y       | The lexing/parsing of Ruby source code |
| defs/keywords | The reserved keywords of Ruby          |


## [JRuby](https://www.jruby.org)


## [mruby](https://github.com/mruby/mruby/)

compile with mrbc


## [PicoRuby](https://github.com/picoruby/picoruby)


## [TruffleRuby](https://github.com/oracle/truffleruby)


## [Artichoke](https://github.com/artichoke/artichoke)

A Ruby made with Rust


# Standard Library and Extensions


## Bundler

- <https://bundler.io>


### Writing a Gemfile

```ruby
source 'https://example.org' do
  # If you have private gems, put them here so that someone doesn't spoof them on rubygems.org !
end

source 'https://rubygems.org' do
  # Gems here
end

# Using a git repository
gem 'rack', git: 'https://github.com/rack/rack'

# Make a gem group optional
# use `bundle config set --local with GROUP` to install
group :development, optional: true do
  gem 'guard'
end
```

<https://bundler.io/v2.2/guides/git.html>


### Install gems to dir

```shell
bundle config set --local path 'vendor'

# Deprecated way:
bundle install --path dir
```


### Using an http proxy

```shell
http_proxy=http://proxy bundle install
```


## CSV

- [CSV documentation](https://docs.ruby-lang.org/en/master/CSV.html)
- [gem source](https://github.com/ruby/csv)

```ruby
require 'csv'
data = CSV.read('path/to/file', headers: true)
```


## debug


### -r debug

```shell
ruby -r debug example.rb
```


## ERB

[Core ERB documentation](https://docs.ruby-lang.org/en/master/ERB.html)

Tags:

```
<% Ruby code -- inline with output %>
<%= Ruby expression -- replace with result %>
<%# comment -- ignored -- useful in testing %>
% a line of Ruby code -- treated as <% line %> (optional -- see ERB.new)
%% replaced with % if first thing on a line and % processing is used
<%% or %%> -- replace with <% or %> respectively
```

Trim mode:

| code | desc                                                     |
|---- |-------------------------------------------------------- |
| %    | enables Ruby code processing for lines beginning with %  |
| <>   | omit newline for lines starting with <% and ending in %> |
| >    | omit newline for lines ending in %>                      |
| -    | omit blank lines ending in -%>                           |


## Gem

```shell
# Build a gem
gem build name.gemspec

# upload gem to rubygems.org or other host
gem push name-0.0.1.gem [--host HOST]

# add a gem source
gem source -a SOURCE
# remove a source
gem source -r SOURCE
# update source cache
gem source -u

# add an owner
gem owner GEM --add EMAIL
# remove an owner
gem owner GEM --remove EMAIL
```

Environment variables:

| var        | description              |
|---------- |------------------------ |
| `GEM_PATH` | where gems can be loaded |
| `GEM_HOME` | where gems are installed |

- [Gem packaging best practices](http://weblog.rubyonrails.org/2009/9/1/gem-packaging-best-practices/)
- <http://rakeroutes.com/blog/lets-write-a-gem-part-one/>


## Irb

As of Ruby 2.4+, you can use `binding.irb` for an experience similar to Pry


## JSON

```ruby
require 'json'
obj = JSON.parse(File.read('./thing.json'))
obj.to_json
JSON.pretty_generate(obj)
```


## Minitest

- <http://docs.seattlerb.org/minitest/>


### Mocking methods

```ruby
# method mocking is done with a block
ClassName.stub :method_name, method_value do
  ClassName.method_to_run_against
end
```


## Net::HTTP

```shell
require 'net/http'

uri = URI('http://example.com')
```


## Net::SMTP

[gem source](https://github.com/ruby/net-smtp)

```ruby
# Gemfile
gem 'net-smtp'
```

```ruby
# Using starttls
smtp = Net::SMTP.new smtp_options[:address], smtp_options[:port]
smtp.enable_starttls_auto
smtp.start(
  smtp_options[:helo_domain],
  smtp_options[:user_name],
  smtp_options[:password],
  smtp_options[:authentication]
) do |smtp|
  smtp.send_message msgstr, "from@example.org", [ "to@example.org" ]
end
```


## ObjectSpace

This module is useful for understanding the memory size of what you're working with, as well as seeing what objects are still alive at a given time

```ruby
ObjectSpace.count_objects
```

[ObjectSpace docs](https://docs.ruby-lang.org/en/master/ObjectSpace.html)


## Rdoc

- [Core documentation](https://docs.ruby-lang.org/en/master/RDoc.html)
- [Core markup documentation](https://docs.ruby-lang.org/en/master/RDoc/Markup.html)
- <https://www.mikeperham.com/wp-content/uploads/2010/12/rdoc.html>
- <https://jan.varwig.org/wp-content/uploads/2006/09/Rdoc%20Cheat%20Sheet.pdf>

```
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

```
https://github.com/example/example
mailto:user@example.com
{RDoc Documentation}[http://rdoc.rubyforge.org]
{RDoc Markup}[rdoc-ref:RDoc::Markup]
```


### Using `ri` on Fedora

To get the system ruby library documentation, you'll need to install `ruby-doc`

```shell
sudo dnf install rubygem-rdoc ruby-doc
```


### Get list of undocumented code

```
rdoc -C1 > documentation_coverage.txt
```


#### Links

- <http://documenting-ruby.org/>


## racc

- [ruby/racc](https://github.com/ruby/racc)

Built-in to Ruby

Need a scanner/tokenizer, use rexical or oedipus<sub>rex</sub> (or yyparse and an iterator to extract tokens from your string)


## RubyVM

Only exists in MRI <https://docs.ruby-lang.org/en/master/RubyVM.html>


## strscan

[ruby/strscan](https://github.com/ruby/strscan)


## Set

- [ruby/set](https://github.com/ruby/set)


## Ripper

Built-in since Ruby 1.9

Dumps S-expressions

```ruby
require 'ripper'
require 'pp'

f = File.read('foo.rb')
pp Ripper.sexp(f)
```

<http://www.rubyinside.com/using-ripper-to-see-how-ruby-is-parsing-your-code-5270.html>


<a id="orgafa25a8"></a>

## Benchmark

[ruby/benchmark](https://github.com/ruby/benchmark)

```ruby
require 'benchmark'

n = 50000

# this gives you a Benchmark::Tms object
tms = Benchmark.measure { for i in 1..n; a = "1"; end }

# Returns [@label, @utime, @stime, @cutime, @cstime, @real]
tms.to_a
```


# Write to a file

```ruby
fp = File.open(filename, mode)
fp.write('bloop')
fp.close
```


# Performance

As with any system performance advice, don't forget to [benchmark](#orgafa25a8)


## Frozen string literals

When 'freeze' is called on a string you are marking that string as immutable. The performance benefit is that the object can be re-used without new object allocation.

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

```shell
$ ruby freeze.rb
60
80
100
100
```


### File pragma

In Ruby 2.3+, you can add this pragma to opt-in to all string literals being made immutable in the source file. Note - this isn't a guaranteed performance win!

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

When going through a file, it's better to read the file line-by-line like the following to avoid GC hits:

```ruby
file = File.open('foobar', 'r')
while line = file.gets
  line.split('.')
end
```


### Execution context copying

When Ruby converts something into a Proc object, it stores references to all objects in the block's execution context. This can leak memory, since the GC can't sweep those references.


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
- Use `String#unpack1` over `String#unpack().first`


## Books / Links

- Ruby Performance Optimization by Alexander Dymo
- <https://github.com/JuanitoFatas/fast-ruby>
- <https://github.com/DamirSvrtan/fasterer>


# Benchmarking


## Running the profiler

[ruby/profile](https://github.com/ruby/profile)

The profile gem was removed from the standard library in Ruby 2.7

```shell
ruby -r profile script.rb
```


## Third-party benchmarking

- <https://github.com/evanphx/benchmark-ips>
- <https://github.com/davy/benchmark-bigo>


# Binary manipulation with ::Array#pack / ::String#unpack

Integer:

| Directive  | Meaning                                                                                  |
|---------- |---------------------------------------------------------------------------------------- |
| C          | 8-bit unsigned (unsigned char)                                                           |
| S          | 16-bit unsigned, native endian (uint16<sub>t</sub>)                                      |
| L          | 32-bit unsigned, native endian (uint32<sub>t</sub>)                                      |
| Q          | 64-bit unsigned, native endian (uint64<sub>t</sub>)                                      |
| c          | 8-bit signed (signed char)                                                               |
| s          | 16-bit signed, native endian (int16<sub>t</sub>)                                         |
| l          | 32-bit signed, native endian (int32<sub>t</sub>)                                         |
| q          | 64-bit signed, native endian (int64<sub>t</sub>)                                         |
| S\_, S!    | unsigned short, native endian                                                            |
| I, I\_, I! | unsigned int, native endian                                                              |
| L\_, L!    | unsigned long, native endian                                                             |
| n          | 16-bit unsigned, network (big-endian) byte order                                         |
| N          | 32-bit unsigned, network (big-endian) byte order                                         |
| v          | 16-bit unsigned, VAX (little-endian) byte order                                          |
| V          | 32-bit unsigned, VAX (little-endian) byte order                                          |
| U          | UTF-8 character                                                                          |
| w          | BER-compressed integer (see Array.pack)                                                  |
| Q\_, Q!    | unsigned long long, native endian (ArgumentError if the platform has no long long type.) |
| s\_, s!    | signed short, native endian                                                              |
| i, i\_, i! | signed int, native endian                                                                |
| l\_, l!    | signed long, native endian                                                               |
| q\_, q!    | signed long long, native endian (ArgumentError if the platform has no long long type.)   |

| desc          | suffix |
|------------- |------ |
| native endian | !      |
| native endian | \_     |
| big-endian    | >      |
| little-endian | <      |

- J, J! j, and j! are available since Ruby 2.3.
- Q\_, Q!, q\_, and q! are available since Ruby 2.1.
- I!<, i!<, I!>, and i!> are available since Ruby 1.9.3.

Float:

| Directive | Meaning                                           |
|--------- |------------------------------------------------- |
| D, d      | double-precision, native format                   |
| F, f      | single-precision, native format                   |
| E         | double-precision, little-endian byte order        |
| e         | single-precision, little-endian byte order        |
| G         | double-precision, network (big-endian) byte order |
| g         | single-precision, network (big-endian) byte order |

String:

| Directive | Meaning                                                                                            |
|--------- |-------------------------------------------------------------------------------------------------- |
| A         | arbitrary binary string (remove trailing nulls and ASCII spaces)                                   |
| a         | arbitrary binary string                                                                            |
| Z         | null-terminated string                                                                             |
| B         | bit string (MSB first)                                                                             |
| b         | bit string (LSB first)                                                                             |
| H         | hex string (high nibble first)                                                                     |
| h         | hex string (low nibble first)                                                                      |
| u         | UU-encoded string                                                                                  |
| M         | quoted-printable, MIME encoding (:RFC:=2045=)                                                      |
| m         | base64 encoded string (:RFC:=2045=) (default) base64 encoded string (:RFC:=4648=) if followed by 0 |
| P         | pointer to a structure (fixed-length string)                                                       |
| p         | pointer to a null-terminated string                                                                |

Misc:

| Directive | Meaning                                         |
|--------- |----------------------------------------------- |
| @         | skip to the offset given by the length argument |
| X         | skip backward one byte                          |
| x         | skip forward one byte                           |


# Ruby Gems


## Rack

[Live reloading](https://github.com/jaredmdobson/rack-livereload)

```ruby
# config.ru
require 'rack-livereload'
use Rack::LiveReload
```

(pair it with [guard-livereload](https://github.com/guard/guard-livereload))


## Sorbet

[Home page](https://sorbet.org)

```shell
# First run
bundle exec srb init
# Run typechecker
bundle exec src tc
```

Gemfile:

```ruby
gem 'sorbet', :group => :development
```


## Sinatra

- [Home page](http://www.sinatrarb.com)
- [Dockerizing Sinatra](https://www.codewithjason.com/dockerize-sinatra-application/)


### Cheatsheet

```shell
# To run a Sinatra server
ruby app.rb
```

```ruby
require 'sinatra'
get '/helloworld' do
  'Hello, world!'
end

get '/haml' do
  # Render template views/thing, insert some variables
  haml :thing, locals: {foo: 'bar', biz: 'baz'}
end
```


### Using Rack

Classic-style:

```ruby
require './app'
run Sinatra::Application
```


### Using haml

- Add haml to Gemfile
- To add partials use: `= haml :footer`


### Reloader

<http://sinatrarb.com/contrib/reloader>

```ruby
# Gemfile
gem install sinatra-contrib
```

```ruby
# Classic
require "sinatra/reloader" if development?

# Specify additional load paths
also_reload '/path/to/some/file' # path can use globbing
dont_reload '/path/to/other/file'
after_reload do
  puts 'reloaded'
end

# Modular approach
require "sinatra/base"
require "sinatra/reloader"

class MyApp < Sinatra::Base
  configure :development do
    register Sinatra::Reloader
    also_reload '/path/to/some/file'
    dont_reload '/path/to/other/file'
    after_reload do
      puts 'reloaded'
    end
  end

  # ...
end
```


### Mustache handler

- <https://github.com/mustache/mustache>
- <https://github.com/mustache/mustache-sinatra>
- <https://github.com/mustache/vim-mustache-handlebars>


## Capistrano

```shell
# Create new stub
cap install
```


## Camping

- <http://www.ruby-camping.com/>


## Guard

```shell
# Create bin/guard
bundle binstub guard

# Write a Guardfile
guard init

# List available guards
guard list
```

- <https://github.com/guard/guard>

Integrations:

| gem                                                       | initializing          |
|--------------------------------------------------------- |--------------------- |
| [guard-minitest](https://github.com/guard/guard-minitest) | `guard init minitest` |
| [guard-rubocop](https://github.com/rubocop/guard-rubocop) | `guard init rubocop`  |


## Rubocop


### NodePattern

Note: table uses int node, but can be for any type of node.

Variable length patterns can only be used in a sequence once

| syntax                   | desc                                                              |
|------------------------ |----------------------------------------------------------------- |
| `int`                    | match `int` exactly                                               |
| `(int 1)`                | match a node with more precision (eg, int node that represents 1) |
| `_`                      | match any single node                                             |
| ...                      | several subsequent nodes                                          |
| `int*`                   | Match zero or more targets of int type                            |
| `int?`                   | Match zero or one of the int type                                 |
| `int+`                   | Match at least one of the int type                                |
| `<int float>`            | Match integer and float in either order                           |
| `{int float}`            | "OR" union function (ie either int or float)                      |
| `(int [odd? positive?])` | "AND", useful when using predicate methods against type           |
| `(int $_)`               | Capture the node/element (variable length captured as arrays)     |
| `(^array int+)`          | TODO Check the parent of a node                                   |
| ``(`int int*)``          | Check for descendents of a node, here an array of ints            |
| `(int ALLOWED_INTS)`     | Use a constant (here ALLOWED<sub>INTS</sub>) in a pattern         |

Predicate methods can be used as well:

```
# Patterns can have a comment, with `# ` to EOL

int_type?  # equivalent to (int _)
(int odd?) # Only match odd numbers

# `#` can be used to call an outside function

(int #prime?) # if a prime? method has been created
(int #multiple_of?(12)) # You can also use arguments in your functions (the signature here being `multiple_of?(value, multiple)`)

# An argument in the matcher can be passed to a pattern
def_node_matcher :int_node_magic_number?, '(int #magic_number?(%1))'
```

Developing a pattern to match (consider using `irb` for interactivity):

```ruby
require 'rubocop'
code = '2 + 2'
source = RuboCop::ProcessedSource.new(code, RUBY_VERSION.to_f)
node = source.ast
RuboCop::NodePattern.new('(int ...)').match(node)
```

```shell
# Generating AST on the command line
ruby-parse -e '2 + 2'
```

Racc compiles the NodePattern. See [lib/rubocop/ast/nodepattern/parser.y](https://github.com/rubocop/rubocop-ast/blob/master/lib/rubocop/ast/node_pattern/parser.y)

- [RuboCop development guide](https://docs.rubocop.org/rubocop/development.html)
- [NodePattern doc](https://github.com/rubocop/rubocop-ast/blob/master/docs/modules/ROOT/pages/node_pattern.adoc)


## adsf

```shell
gem install 'adsf'
gem install 'adsf-live' # Live reload functionality
adsf --live-reload
```

- <https://github.com/ddfreyne/adsf/>


## Ruby on Rails


### Installing and setting up Rails 4

```shell
gem install rails
rails new app_name
cd app_name
rake db:create
rails server # (on standalone machine)
rails generate controller home index
rm public/index.html
# Uncomment the ``root :to => "home#index"`` line
$EDITOR config/routes.rb
```


### Scaffolding

```shell
rails generate scaffold Post user:references title:string{50} content:text
```


### Add indexes to migration

```shell
rails g resource user name:index email:uniq
```


### Using RSpec with Rails

- <http://rspec.info/>
- <http://rubydoc.info/gems/rspec-rails/frames>

Add to `Gemfile`:

```ruby
gem 'rspec-rails'
gem 'guard-rspec'
```

```shell
rails generate rspec:install
```


### ActiveRecord

Supported database column types:

- binary
- boolean
- date
- datetime
- decimal
- float
- integer
- primary<sub>key</sub>
- string
- text
- time
- timestamp

Use "AddColumnToTable" style migration names to have the work done for you automatically in the migration


### Validating Active Records

```ruby
class Post < ActiveRecord::Base
  # ...

  validates :name,  :presence => true
  validates :title, :presence => true,
                     :length => { :minimum => 5 }
end
```


#### Ensure uniqueness at the db level

1. `rails generate migration add_index_to_users_email`
2. add to migration file under def change: `add_index :users, :email, unique: true`
3. `bundle exec rake db:migrate`


### Database

```shell
# Migrate to new model
rake db:migrate

# Return to previous model
rake db:rollback
```


### Automated testing with guard and spork

- Add to Gemfile:

```ruby
group :test, :development do
  gem 'guard-spork'
  gem 'spork'
  gem 'guard-rspec'
end
group :test do
  gem 'rb-inotify'
  gem 'libnotify'
end
```

- run the following

```shell
bundle exec guard init rspec
bundle exec spork --bootstrap
bundle exec guard init spork
```

- Add `:cli => '--drb`' to guard 'rspec'
- `bundle exec guard`


### Creating tables

```ruby
create_table "contacts" do |t|
  t.integer  "user_id", :null => false
  t.string   "name", :null => false
  t.string   "phone", :limit => 40
  t.string   "email"
end
```


### Reset test database

```shell
bundle exec rake db:test:prepare
```


### Rake default niceties

```shell
# Find TODO, FIXME, and OPTIMIZE comment tags
rake notes

# Get versions
rake about
```


### Simple wins

- Use `find_each` instead of `each` when searching through large sets of iterables
- Use `content_tag` to avoid XSS hacks


### Links

- <https://robertheaton.com/2013/07/22/how-to-hack-a-rails-app-using-its-secret-token/>


### Omniauth

These notes are assuming you're also allowing regular email/password logins. It's greatly simplified if you don't...

```shell
rails generate model Authorization provider:string uid:string user_id:integer
```

`Gemfile`:

```ruby
gem 'omniauth'
gem 'omniauth-twitter'
gem 'omniauth-facebook'
gem 'omniauth-linkedin'
```

`config/initializers/omniauth.rb`:

```ruby
Rails.application.config.middleware.use OmniAuth::Builder do
  provider :twitter, 'CONSUMER_KEY', 'CONSUMER_SECRET'
  provider :facebook, 'APP_ID', 'APP_SECRET'
  provider :linked_in, 'CONSUMER_KEY', 'CONSUMER_SECRET'
end
```

`app/models/user.rb`:

```ruby
has_many :authorizations

def add_provider(auth_hash)
  unless authorizations.find_by_provider_and_uid(auth_hash["provider"], auth_hash["uid"])
        Authorization.create :user => self, :provider => auth_hash["provider"], :uid => auth_hash["uid"]
  end
end
```

`app/controllers/sessions_controller.rb`:

```ruby
def omniauth_create
  auth_hash = request.env['omniauth.auth']
  if session[:user_id]
        # Means user is signed in. Add the authorization to the user
        user = User.find(session[:user_id])
        user.add_provider(auth_hash)
  else
        auth = Authorization.find_or_create(auth_hash)
        # Create the session
        session[:user_id] = auth.user_id
        user = User.find(session[:user_id])
  end
  sign_in user
  redirect_back_or user
end
```

`app/models/authorization.rb`:

```ruby
belongs_to :user
validates :provider, :uid, :presence => true

def self.find_or_create(auth_hash)
  unless auth = find_by_provider_and_uid(auth_hash["provider"], auth_hash["uid"])
        user = User.find_by_email(auth_hash["info"]["email"])
        if not user
          # If it's a new user, we want to give them a solid password
          random_string = SecureRandom.base64(30)
          user = User.create :name => auth_hash["info"]["name"],
                           :email => auth_hash["info"]["email"],
                           :password => random_string,
                           :password_confirmation => random_string
        end
        auth = create :user_id => user, :provider => auth_hash["provider"], :uid => auth_hash["uid"]
  end
  auth
end
```

`config/routes.rb`:

```ruby
match '/auth/:provider/callback', to: 'sessions#create'
match '/auth/failure', to: 'sessions#failure'
```


### Gravatar

Helper to return Gravatar image:

```ruby
# Returns the Gravatar (http://gravatar.com/) for the given user.
def gravatar_for(user, options = { size: 50 })
  gravatar_id = Digest::MD5::hexdigest(user.email.downcase)
  size = options[:size]
  gravatar_url = "https://secure.gravatar.com/avatar/#{gravatar_id}?s=#{size}"
  image_tag(gravatar_url, alt: user.name, class: "gravatar")
end
```


## Pry

<http://pryrepl.org/>

```ruby
require 'pry'

# start REPL
binding.pry
```

```
# get list of commands
repl> help

# quit program
repl> !!!
```


## [dalli](https://github.com/petergoldstein/dalli)

Memcached client - fast!


## [FrozenRecord](https://github.com/byroot/frozen_record)

Read-only ActiveRecord interface over YAML (and JSON) files


## [icalendar](https://github.com/icalendar/icalendar)


## Compilers


### scanning


#### rexical

[tenderlove/rexical](https://github.com/tenderlove/rexical)


#### oedipus<sub>lex</sub>

[seattlerb/oedipus<sub>lex</sub>](https://github.com/seattlerb/oedipus_lex)


### parsing


#### parsetree (CLI)

```shell
ruby --dump parsetree
```

This gives the names for the node objects used in ast.c/compile.c/node.c/vm.c in CRuby source (output not usable in other implementations)


#### parser gem

- [whitequark/parser](https://github.com/whitequark/parser)

Despite the generic sounding name, it's not generic. Parser handles Ruby (and Ruby-ish) code.

A lot of the tree-rewriter code is in here (though a bit of a shame that's not abstracted out, in the same way that NodePattern should be)


### AST


#### ast gem

- [whitequark/ast](https://github.com/whitequark/ast)

- [Node class](https://github.com/whitequark/ast/blob/master/lib/ast/node.rb)
- [Processing mixin](https://github.com/whitequark/ast/blob/master/lib/ast/processor/mixin.rb)


#### code reconstruction from AST

- [unparser](https://github.com/mbj/unparser)
- [sorcerer](https://github.com/jimweirich/sorcerer)


## Solargraph

```
gem install solargraph
# get core
solargraph download-core
```


# Ruby-Next

- [Ruby-next Github repo](https://github.com/ruby-next/ruby-next)
- [Writing Go in Ruby](https://evilmartians.com/chronicles/a-no-go-fantasy-writing-go-in-ruby-with-ruby-next)


# RVM

```shell
curl -L https://get.rvm.io | bash -s stable --ruby

# Installing openssl and readline for dependencies
curl -L https://get.rvm.io | bash -s stable
rvm pkg install openssl
rvm pkg install readline
rvm install 1.9.3 --with-openssl-dir=$HOME/.rvm/ --with-readline-dir=$HOME/.rvm/usr

# Create a per-project rvmrc
rvm --rvmrc --create 1.9.3@projectname
# set default ruby
rvm --default use 2.2.0
# Use a gemset
rvm gemset [create|use|delete] gemsetname
# See gem directory
gem env gemdir
```

For automatic gemset initialization, add gems to `~/.rvm/gemsets/global.gems`
