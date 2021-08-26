---
title: Bundler
tags: Ruby
---

## Writing a Gemfile

```ruby
source 'https://example.org' do
  # If you have private gems, put them here so that someone doesn't spoof them on rubygems.org !
end

source 'https://rubygems.org' do
  # Gems here
end
```

## Install gems to dir

```bash
bundle install --path dir
```

## Resources

* <https://bundler.io>

