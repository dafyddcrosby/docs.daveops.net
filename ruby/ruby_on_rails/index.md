---
title: Ruby on Rails
---

.. TODO - <http://ruby.railstutorial.org/ruby-on-rails-tutorial-book?version=3.2>

Installing and setting up Rails
----------------

```bash
gem install rails
rails new app_name
cd app_name
rake db:create
rails server # (on standalone machine)
rails generate controller home index
rm public/index.html
# Uncomment the ``root :to => "home#index"`` line
$EDITOR config/routes.rb
```

Scaffolding
-----------

```bash
rails generate scaffold Post user:references title:string{50} content:text
```

Add indexes to migration
------------------------



 rails g resource user name:index email:uniq

Using Dreamhost
---------------

* Passenger must be enabled for domain/subdomain
* Passenger assumes it's in production mode, touch tmp/restart.txt to have it reload pages
* For database ``rake db:migrate RAILS_ENV=production``

Validating Active Records
-------------------------

```ruby
class Post < ActiveRecord::Base
  # ...

  validates :name,  :presence => true
  validates :title, :presence => true,
       				 :length => { :minimum => 5 }
end
```

### Ensure uniqueness at the db level

1. ``rails generate migration add_index_to_users_email``
2. add to migration file under def change: ``add_index :users, :email, unique: true``
3. ``bundle exec rake db:migrate``

Database
--------

```bash
# Migrate to new model
rake db:migrate

# Return to previous model
rake db:rollback
```

Automated testing with guard and spork
--------------------------------------

* Add to Gemfile:

```ruby
group :test, :development do
  gem 'guard-spork'
  gem 'spork'
  gem 'guard-rspec'
end
group :test do
  gem 'rb-inotify'
  gem 'libnotify'
end
```

* run the following

```bash
bundle exec guard init rspec
bundle exec spork --bootstrap
bundle exec guard init spork
```

* Add ``:cli => '--drb``' to guard 'rspec'
* ``bundle exec guard``


Creating tables
---------------

```ruby
create_table "contacts" do |t|
  t.integer  "user_id", :null => false
  t.string   "name", :null => false
  t.string   "phone", :limit => 40
  t.string   "email"
end
```

Reset test database
-------------------

```bash
bundle exec rake db:test:prepare
```

## Rake default niceties

```bash
# Find TODO, FIXME, and OPTIMIZE comment tags
rake notes

# Get versions
rake about
```

Simple wins
-----------

* Use ``find_each`` instead of ``each`` when searching through large sets of iterables
* Use ``content_tag`` to avoid XSS hacks


Links
-----

* <https://robertheaton.com/2013/07/22/how-to-hack-a-rails-app-using-its-secret-token/>
