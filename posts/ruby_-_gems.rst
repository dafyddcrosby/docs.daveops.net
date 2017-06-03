Ruby - Gems
===========
:date: 2016-01-26
:tags: Ruby

Build a gem
-----------
::

  gem build name.gemspec

Uploading
---------
::

  # push gem to rubygems.org or other host
  gem push name-0.0.1.gem [--host HOST]

Sources
-------
::

  # add a source
  gem source -a SOURCE
  # remove a source
  gem source -r SOURCE
  # update source cache
  gem source -u
