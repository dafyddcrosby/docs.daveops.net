---
title: benchmarking
tags: ["Ruby"]
---

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
