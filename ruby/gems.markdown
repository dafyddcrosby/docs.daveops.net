# gems
@Ruby

Build a gem
-----------


  gem build name.gemspec

Uploading
---------


  # push gem to rubygems.org or other host
  gem push name-0.0.1.gem [--host HOST]

Sources
-------

	# add a source
	gem source -a SOURCE
	# remove a source
	gem source -r SOURCE
	# update source cache
	gem source -u

Ownership
---------

	gem owner GEM --add EMAIL
	gem owner GEM --remove EMAIL


Environment variables
---------------------

| var      | description              |
|----------|--------------------------|
| GEM_PATH | where gems can be loaded |
| GEM_HOME | where gems are installed |


Links
-----

* [Gem packaging best practices](http://weblog.rubyonrails.org/2009/9/1/gem-packaging-best-practices/)


