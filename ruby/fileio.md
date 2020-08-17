---
title: Ruby - File IO
---

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
