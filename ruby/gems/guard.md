---
title: Ruby/Guard
---

```bash
# Create bin/guard
bundle binstub guard

# Write a Guardfile
guard init

# List available guards
guard list
```

- <https://github.com/guard/guard>

## Minitest

In bundler:

```ruby
gem "guard-minitest"
```

```bash
# Add minitest to Guardfile
guard init minitest
```

- <https://github.com/guard/guard-minitest>

## Rubocop

In bundler:

```ruby
gem "guard-rubocop"
```

```bash
# Add rubocop to Guardfile
guard init rubocop
```

- <https://github.com/rubocop/guard-rubocop>
