---
title: Sinatra
---

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
