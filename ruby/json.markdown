---
title: json
---

```ruby
require 'json'
obj = JSON.parse(File.read('./thing.json'))
obj.to_json
JSON.pretty_generate(obj)
```
