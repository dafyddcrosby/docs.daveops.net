---
title: Debugging
---

## -r debug

```bash
ruby -r debug example.rb
```

## Irb

As of Ruby 2.4+, you can use `binding.irb` for an experience similar to Pry

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
```
