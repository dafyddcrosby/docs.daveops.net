---
title: Ruby - Performance
---

## Frozen string literals

## Frozen string literals

When 'freeze' is called on a string you are marking that string as immutable. The   performance benefit is that the object can be re-used without new object allocation.

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
made immutable in the source file.

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

### Memory-heavy iterators

When going through a file, it's better to read the file line-by-line like the
following to avoid GC hits:

```ruby
file = File.open('foobar', 'r')
while line = file.gets
  line.split('.')
end
```

## See also 

- [ObjectSpace](./objectspace.md)
