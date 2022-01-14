# RubyGems


# Ruby Gems

# Sorbet

## CLI

```bash
# First run
bundle exec srb init
# Run typechecker
bundle exec src tc
```

## Install

```ruby
gem 'sorbet', :group => :development
```

- [Home page](https://sorbet.org)


# Sinatra
## Cheatsheet

```bash
# To run a Sinatra server
ruby app.rb
```

```ruby
require 'sinatra'
get '/helloworld' do
  'Hello, world!'
end
```

## Using Rack

### Classic-style

```ruby
require './app'
run Sinatra::Application
```

## Using haml

* Add haml to Gemfile
* To add partials use: ``= haml :footer``

## Links
* <http://www.sinatrarb.com>
* [Dockerizing Sinatra](https://www.codewithjason.com/dockerize-sinatra-application/)


# Capistrano

```bash
# Create new stub
cap install
```


# Camping

- <http://www.ruby-camping.com/>





# Guard

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


# adsf

```bash
gem install 'adsf'
gem install 'adsf-live' # Live reload functionality
adsf --live-reload
```

- <https://github.com/ddfreyne/adsf/>
