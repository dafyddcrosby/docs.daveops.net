---
title: Debugging
---

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
