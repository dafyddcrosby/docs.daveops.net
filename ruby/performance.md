---
title: Ruby - Performance
---

As with any system performance advice, don't forget to [benchmark](./benchmarking.md)

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

- [ObjectSpace](./objectspace.md)

## Books / Links

- Ruby Performance Optimization by Alexander Dymo
- https://github.com/JuanitoFatas/fast-ruby
- https://github.com/DamirSvrtan/fasterer
