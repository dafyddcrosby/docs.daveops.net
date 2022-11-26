# Bundler

- <https://bundler.io>


## Writing a Gemfile

```ruby
source 'https://example.org' do
  # If you have private gems, put them here so that someone doesn't spoof them on rubygems.org !
end

source 'https://rubygems.org' do
  # Gems here
end

# Using a git repository
gem 'rack', git: 'https://github.com/rack/rack'

# Make a gem group optional
# use `bundle config set --local with GROUP` to install
group :development, optional: true do
  gem 'guard'
end
```

<https://bundler.io/v2.2/guides/git.html>


## Install gems to dir

```shell
bundle config set --local path 'vendor'

# Deprecated way:
bundle install --path dir
```


## Using an http proxy

```shell
http_proxy=http://proxy bundle install
```