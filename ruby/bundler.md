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

## Using a git repository

```ruby
gem 'rack', git: 'https://github.com/rack/rack'
```

<https://bundler.io/v2.2/guides/git.html>

## Install gems to dir

```bash
bundle config set --local path 'vendor'
```

Deprecated way:

```bash
bundle install --path dir
```

## Resources

- <https://bundler.io>

